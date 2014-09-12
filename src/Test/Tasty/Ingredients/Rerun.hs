{-# LANGUAGE DeriveDataTypeable #-}
module Test.Tasty.Ingredients.Rerun (rerunningTests) where

import Prelude hiding (filter)

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace)
import Data.Foldable (asum)
import Data.List.Split (endBy)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Proxy (Proxy(..))
import Data.Semigroup.Applicative (Traversal(..))
import Data.Tagged (Tagged(..), untag)
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
  optionName = Tagged "rerun-log-file"
  optionHelp = Tagged "The path to which rerun's state file should be saved"
  defaultValue = RerunLogFile ".tasty-rerun-log"
  parseValue = Just . RerunLogFile


--------------------------------------------------------------------------------
newtype UpdateLog = UpdateLog Bool
  deriving (Typeable)

instance Tasty.IsOption UpdateLog where
  optionName = Tagged "rerun-update"

  optionHelp = Tagged "If present the log file will be updated, otherwise it \
                      \will be left unchanged"

  defaultValue = UpdateLog False

  parseValue = Just . UpdateLog . const True

  optionCLParser = fmap UpdateLog $ OptParse.switch $ mconcat
    [ OptParse.long name
    , OptParse.help helpString
    ]
    where
    name = untag (Tasty.optionName :: Tagged UpdateLog String)
    helpString = untag (Tasty.optionHelp :: Tagged UpdateLog String)

--------------------------------------------------------------------------------
data Filter = Failures | Exceptions | New | Successful
  deriving (Eq, Ord)

parseFilter :: String -> Maybe Filter
parseFilter "failures" = Just Failures
parseFilter "exceptions" = Just Exceptions
parseFilter "new" = Just New
parseFilter "successful" = Just Successful
parseFilter _ = Nothing

--------------------------------------------------------------------------------
everything :: [Filter]
everything = [Failures, Exceptions, New, Successful]

--------------------------------------------------------------------------------
newtype FilterOption = FilterOption (Set.Set Filter)
  deriving (Typeable)

instance Tasty.IsOption FilterOption where
  optionName = Tagged "rerun-filter"

  optionHelp = Tagged "A comma separated list to specify which tests to run when\
                      \ comparing against previous test runs. Valid options \
                      \are: everything, failures, exceptions, new"

  defaultValue = FilterOption (Set.fromList everything)

  parseValue =
    fmap (FilterOption . Set.fromList) . mapM (parseFilter . trim) . endBy ","
    where trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

--------------------------------------------------------------------------------
data TestResult = Completed Bool | ThrewException
  deriving (Read, Show)


--------------------------------------------------------------------------------
-- | This 'Tasty.Ingredient' transformer adds various @--rerun@ options to your
-- test program. These flags add stateful execution of your test suite, allowing
-- you to rerun only tests that are failing from the previous run, or tests that
-- that have been added since the last test ran, once the 'Tasty.TestTree' has
-- been filtered.
--
-- The input list of 'Tasty.Ingredient's specifies the 'Tasty.Ingredients's that
-- will actually work with the filtered 'Tasty.TestTree'. Normally, you'll want
-- at least 'Tasty.Test.Runners.consoleTestReporter'.
rerunningTests :: [Tasty.Ingredient] -> Tasty.Ingredient
rerunningTests ingredients =
  Tasty.TestManager (rerunOptions ++ existingOptions) $
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
                    getTraversal $
                    Tasty.foldTestTree (observeResults statusMap)
                                        options filteredTestTree

              when updateLog (saveStateTo stateFile getTestResults)
              return outcome

          tryAndRun (Tasty.TestManager _ f) =
            f options filteredTestTree

      case asum (map tryAndRun ingredients) of
        -- No Ingredients chose to run the tests, we should really return
        -- Nothing, but we've already commited to run by the act of
        -- filtering the TestTree.
        Nothing -> return False
        -- Otherwise, an Ingredient did choose to run the tests, so we
        -- simply run the above constructed IO action.
        Just e -> e
  where
  existingOptions = flip concatMap ingredients $ \ingredient ->
    case ingredient of
      Tasty.TestReporter options _ -> options
      Tasty.TestManager options _ -> options

  rerunOptions = [ Tasty.Option (Proxy :: Proxy RerunLogFile)
                 , Tasty.Option (Proxy :: Proxy UpdateLog)
                 , Tasty.Option (Proxy :: Proxy FilterOption)
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
    let foldSingle _ name _ = Traversal $ Functor.Compose $ do
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

        foldGroup name children = Traversal $ Functor.Compose $ do
          Const soFar <- Functor.getCompose $ getTraversal children
          pure $ Const (Map.mapKeys (name :) soFar)

    in Tasty.trivialFold
      { Tasty.foldSingle = foldSingle
      , Tasty.foldGroup = foldGroup
      }

    where
    lookupStatus i = STM.readTVar $
      fromMaybe (error "Attempted to lookup test by index outside bounds")
                (IntMap.lookup i statusMap)
