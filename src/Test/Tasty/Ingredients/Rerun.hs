{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Tasty.Ingredients.Rerun (rerunningTests) where

import Prelude hiding (filter)

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace, toLower)
import Data.Foldable (asum)
import Data.List (intercalate)
import Data.List.Split (endBy)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Proxy (Proxy(..))
import Data.Typeable (Typeable)
import System.IO.Error (catchIOError, isDoesNotExistError)

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.State as State
import qualified Data.Functor.Compose as Functor
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Options.Applicative as OptParse
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.Runners as Tasty

--------------------------------------------------------------------------------
newtype RerunLogFile = RerunLogFile FilePath
  deriving (Typeable)

instance Tasty.IsOption RerunLogFile where
  optionName = return "rerun-log-file"
  optionHelp = return "Location of the log file (default: .tasty-rerun-log)"
  defaultValue = RerunLogFile ".tasty-rerun-log"
  parseValue = Just . RerunLogFile
  optionCLParser = Tasty.mkOptionCLParser (OptParse.metavar "FILE")

--------------------------------------------------------------------------------
newtype UpdateLog = UpdateLog Bool
  deriving (Typeable)

instance Tasty.IsOption UpdateLog where
  optionName = return "rerun-update"
  optionHelp = return "Update the log file to reflect latest test outcomes"
  defaultValue = UpdateLog False
  parseValue = fmap UpdateLog . Tasty.safeReadBool
  optionCLParser = Tasty.mkFlagCLParser mempty (UpdateLog True)

--------------------------------------------------------------------------------
data Filter = Failures | Exceptions | New | Successful
  deriving (Eq, Ord, Enum, Bounded, Show)

parseFilter :: String -> Maybe Filter
parseFilter s = lookup s (map (\x -> (map toLower (show x), x)) everything)

--------------------------------------------------------------------------------
everything :: [Filter]
everything = [minBound..maxBound]

--------------------------------------------------------------------------------
newtype FilterOption = FilterOption (Set.Set Filter)
  deriving (Typeable)

instance Tasty.IsOption FilterOption where
  optionName = return "rerun-filter"
  optionHelp = return
    $  "Read the log file and rerun only tests from a given comma-separated list of categories: "
    ++ map toLower (intercalate ", " (map show everything))
    ++ ". If this option is omitted or the log file is missing, rerun everything."
  defaultValue = FilterOption (Set.fromList everything)
  parseValue =
    fmap (FilterOption . Set.fromList) . mapM (parseFilter . trim) . endBy ","
    where trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
  optionCLParser = Tasty.mkOptionCLParser (OptParse.metavar "CATEGORIES")

--------------------------------------------------------------------------------
data TestResult = Completed Bool | ThrewException
  deriving (Read, Show)


--------------------------------------------------------------------------------
-- | This 'Tasty.Ingredient' transformer allows to control
-- which tests to run depending on their previous outcomes.
-- For example, you can rerun only failing tests or only new tests.
-- The behaviour is controlled by command-line options:
--
-- * @--rerun-update@ @ @
--
--     Update the log file to reflect latest test outcomes.
--
-- * @--rerun-filter@ @CATEGORIES@
--
--     Read the log file and rerun only tests from a given
--     comma-separated list of categories: @failures@,
--     @exceptions@, @new@, @successful@. If this option is
--     omitted or the log file is missing, rerun everything.
--
-- * @--rerun-log-file@ @FILE@
--
--     Location of the log file (default: @.tasty-rerun-log@).
--
-- Usage example:
--
-- > import Test.Tasty
-- > import Test.Tasty.Runners
-- > import Test.Tasty.Ingredients.Rerun
-- >
-- > main :: IO ()
-- > main =
-- >   defaultMainWithIngredients
-- >     [ rerunningTests [ listingTests, consoleTestReporter ] ]
-- >     tests
-- >
-- > tests :: TestTree
-- > tests = undefined
rerunningTests :: [Tasty.Ingredient] -> Tasty.Ingredient
rerunningTests ingredients =
  Tasty.TestManager (rerunOptions ++ Tasty.ingredientsOptions ingredients) $
    \options testTree -> Just $ do
      let RerunLogFile stateFile = Tasty.lookupOption options
          UpdateLog updateLog = Tasty.lookupOption options
          FilterOption filter = Tasty.lookupOption options

      filteredTestTree <- maybe testTree (filterTestTree testTree filter)
                           <$> tryLoadStateFrom stateFile

      let tryAndRun (Tasty.TestReporter _ f) = do
            runner <- f options filteredTestTree
            return $ do
              (statusMap, outcome) <-
                Tasty.launchTestTree options filteredTestTree $ \sMap ->
                  do f' <- runner sMap
                     return (fmap (\a -> (sMap, a)) . f')

              let getTestResults =
                    fmap getConst $
                    flip State.evalStateT 0 $
                    Functor.getCompose $
                    Tasty.getTraversal $
                    Tasty.foldTestTree (observeResults statusMap)
                                        options filteredTestTree

              when updateLog (saveStateTo stateFile getTestResults)
              return outcome

          tryAndRun (Tasty.TestManager _ f) =
            f options filteredTestTree

      case asum (map tryAndRun ingredients) of
        -- No Ingredients chose to run the tests, we should really return
        -- Nothing, but we've already committed to run by the act of
        -- filtering the TestTree.
        Nothing -> return False
        -- Otherwise, an Ingredient did choose to run the tests, so we
        -- simply run the above constructed IO action.
        Just e -> e
  where
  rerunOptions = [ Tasty.Option (Proxy :: Proxy UpdateLog)
                 , Tasty.Option (Proxy :: Proxy FilterOption)
                 , Tasty.Option (Proxy :: Proxy RerunLogFile)
                 ]

  ------------------------------------------------------------------------------
  filterTestTree testTree filter lastRecord =
    let go prefix (Tasty.SingleTest name t) =
          let requiredFilter = case Map.lookup (prefix ++ [name]) lastRecord of
                Just (Completed False) -> Failures
                Just ThrewException -> Exceptions
                Just (Completed True) -> Successful
                Nothing -> New
          in if (requiredFilter `Set.member` filter)
               then Tasty.SingleTest name t
               else Tasty.TestGroup "" []

        go prefix (Tasty.TestGroup name tests) =
          Tasty.TestGroup name (go (prefix ++ [name]) <$> tests)

        go prefix (Tasty.PlusTestOptions f t) =
          Tasty.PlusTestOptions f (go prefix t)

        go prefix (Tasty.WithResource rSpec k) =
          Tasty.WithResource rSpec (go prefix <$> k)

        go prefix (Tasty.AskOptions k) =
          Tasty.AskOptions (go prefix <$> k)

        go prefix (Tasty.After a b c) =
          Tasty.After a b (go prefix c)

    in go [] testTree

  tryLoadStateFrom filePath = do
    fileContents <- (Just <$> readFile filePath)
                      `catchIOError` (\e -> if isDoesNotExistError e
                                              then return Nothing
                                              else ioError e)
    return (read <$> fileContents)

  ------------------------------------------------------------------------------
  saveStateTo filePath getTestResults =
    getTestResults >>= (show >>> writeFile filePath)

  ------------------------------------------------------------------------------
  observeResults statusMap =
    let foldSingle _ name _ = Tasty.Traversal $ Functor.Compose $ do
          i <- State.get

          status <- lift $ STM.atomically $ do
            status <- lookupStatus i
            case status of
              Tasty.Done result -> return $
                case Tasty.resultOutcome result of
                  Tasty.Failure (Tasty.TestThrewException _) -> ThrewException
                  _ -> Completed (Tasty.resultSuccessful result)

              _ -> STM.retry

          Const (Map.singleton [name] status) <$ State.modify (+ 1)

        foldGroup name children = Tasty.Traversal $ Functor.Compose $ do
          Const soFar <- Functor.getCompose $ Tasty.getTraversal children
          pure $ Const (Map.mapKeys (name :) soFar)

    in Tasty.trivialFold
      { Tasty.foldSingle = foldSingle
      , Tasty.foldGroup = foldGroup
      }

    where
    lookupStatus i = STM.readTVar $
      fromMaybe (error "Attempted to lookup test by index outside bounds")
                (IntMap.lookup i statusMap)
