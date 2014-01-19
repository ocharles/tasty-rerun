{-# LANGUAGE DeriveDataTypeable #-}
module Test.Tasty.Ingredient.Rerun (rerunningTests) where

import Control.Applicative
import Control.Arrow ((>>>))
import Control.Monad (guard, join, when)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (for_)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid (mconcat)
import Data.Proxy (Proxy(..))
import Data.Semigroup.Applicative (Traversal(..))
import Data.Tagged (Tagged(..), untag)
import Data.Typeable (Typeable)
import System.IO.Error (catchIOError, ioError, isDoesNotExistError)

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.State as State
import qualified Data.Functor.Compose as Functor
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Options.Applicative as OptParse
import qualified Options.Applicative.Types as OptParse
import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Options as Tasty
import qualified Test.Tasty.Runners as Tasty

--------------------------------------------------------------------------------
data RerunLogFile = RerunLogFile { rerunLogFile :: FilePath }
  deriving (Typeable)

instance Tasty.IsOption RerunLogFile where
  optionName = Tagged "rerun-log-file"
  optionHelp = Tagged "The path to which rerun's state file should be saved"
  defaultValue = RerunLogFile ".tasty-rerun-log"
  parseValue = Just . RerunLogFile


--------------------------------------------------------------------------------
data UpdateLog = UpdateLog { updateLog :: Bool }
  deriving (Typeable)

instance Tasty.IsOption UpdateLog where
  optionName = Tagged "rerun-update"

  optionHelp = Tagged "If present the log file will be update, otherwise it \
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
data FilterOption = FilterOption (Set.Set Filter)
  deriving (Typeable)

instance Tasty.IsOption FilterOption where
  optionName = Tagged "rerun-filter"

  optionHelp = Tagged "Which tests to run when comparing against previous test \
                      \runs. Valid options are: \
                      \everything, failures, exceptions, new"

  defaultValue = FilterOption (Set.fromList everything)

  parseValue = fmap (FilterOption . Set.singleton) . parseFilter

  optionCLParser =
    fmap (FilterOption . Set.fromList . provideDefault) $
    many $ OptParse.nullOption $ mconcat
      [ OptParse.reader parser
      , OptParse.long (untag (Tasty.optionName :: Tagged FilterOption String))
      , OptParse.help (untag (Tasty.optionHelp :: Tagged FilterOption String))
      ]
    where
    provideDefault [] = everything
    provideDefault x  = x

    parser = OptParse.ReadM .
      maybe (Left (OptParse.ErrorMsg $ "Could not parse filter option")) Right .
        parseFilter

--------------------------------------------------------------------------------
data TestResult = Completed Bool | ThrewException
  deriving (Read, Show)


--------------------------------------------------------------------------------
-- | This 'Tasty.Ingredient' transformers adds various @--rerun@ options to your
-- test program. These flags add stateful execution of your test suite, allowing
-- you to rerun only tests that are failing from the previous run, or tests that
-- that have been added since the last test ran, once the 'Tasty.TestTree' has
-- been filtered.
--
-- The input list of 'Tasty.Ingredient's specify how the tests can be run
rerunningTests :: Tasty.Ingredient -> Tasty.Ingredient

-- TestManager doesn't give us access to a StatusMap, so there's nothing we
-- can do here. As such, just fall through to the ingredient we're transforming.
rerunningTests ingredient@Tasty.TestManager {} = ingredient

-- If we have a TestReporter, then we can wrap this to watch the StatusMap.
rerunningTests (Tasty.TestReporter os f) =
  Tasty.TestManager (rerunOptions ++ os) $ \options testTree -> Just $ do
    let RerunLogFile stateFile = Tasty.lookupOption options
    let UpdateLog updateLog = Tasty.lookupOption options
    let FilterOption filter = Tasty.lookupOption options

    testTree' <- maybe (Just testTree) (filterTestTree options testTree filter)
                   <$> tryLoadStateFrom stateFile

    let runData = do
          filteredTestTree <- testTree'
          runner <- f options filteredTestTree
          return (filteredTestTree, runner)

    case runData of
      Nothing -> return False
      Just (filteredTestTree, runner) -> do
        statusMap <- Tasty.launchTestTree options filteredTestTree
        let getTestResults =
              fmap getConst $
              flip State.evalStateT 0 $
              Functor.getCompose $
              getTraversal $
              Tasty.foldTestTree (observeResults statusMap)
                                 options filteredTestTree
        runner statusMap <* when updateLog (saveStateTo stateFile getTestResults)

  where
  rerunOptions = [ Tasty.Option (Proxy :: Proxy RerunLogFile)
                 , Tasty.Option (Proxy :: Proxy UpdateLog)
                 , Tasty.Option (Proxy :: Proxy FilterOption)
                 ]

  ------------------------------------------------------------------------------
  filterTestTree options testTree filter lastRecord =
    let foldSingle _ name t = \prefix ->
          let requiredFilter = case Map.lookup (prefix ++ [name]) lastRecord of
                Just (Completed False) -> Failures
                Just ThrewException -> Exceptions
                Just (Completed True) -> Successful
                Nothing -> New

          in do guard (requiredFilter `Set.member` filter)
                return (Tasty.SingleTest name t)

        foldGroup name tests = \prefix ->
          [ Tasty.testGroup name (tests (prefix ++ [name])) ]

        treeFold = Tasty.trivialFold { Tasty.foldSingle = foldSingle
                                     , Tasty.foldGroup = foldGroup
                                     }
    in case Tasty.foldTestTree treeFold options testTree [] of
         [t] -> Just t
         [] -> Nothing

         -- This state is impossible as a TestTree is either a single test
         -- or a test group. Test groups are folded into a single list element
         -- and single tests to 0-or-1. Thus I believe this state is impossible.
         (x:xs) ->
            error "tasty-rerun found multiple tests when one was expected. \
                  \If you can produce this error, please report this as a bug!"

  ------------------------------------------------------------------------------
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
              Tasty.Done result ->
                return (Completed (Tasty.resultSuccessful result))

              Tasty.Exception e -> return ThrewException

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
