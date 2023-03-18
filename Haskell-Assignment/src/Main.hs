module Main where

import Args
  ( AddOptions (..),
    Args (..),
    GetOptions (..),
    SearchOptions (..),
    parseArgs,
  )
import qualified Data.List as L
import qualified Entry.DB as DB
import Entry.Entry
  ( Entry (..),
    FmtEntry (FmtEntry),
    matchedByAllQueries,
    matchedByQuery,
  )
import Result
import System.Environment (getArgs)
import Test.SimpleTest.Mock
import Prelude hiding (print, putStrLn, readFile)
import qualified Prelude

usageMsg :: String
usageMsg =
  L.intercalate
    "\n"
    [ "snip - code snippet manager",
      "Usage: ",
      "snip add <filename> lang [description] [..tags]",
      "snip search [code:term] [desc:term] [tag:term] [lang:term]",
      "snip get <id>",
      "snip init"
    ]

-- | Handle the init command
handleInit :: TestableMonadIO m => m ()
handleInit = do 
  DB.save DB.empty
  return ()

-- | Handle the get command
handleGet :: TestableMonadIO m => GetOptions -> m ()
handleGet getOpts = do 
  databaseInfo <- DB.load
  case databaseInfo of 
    Success valoare ->
      let 
        firstValue = DB.findFirst (\x -> entryId x == getOptId getOpts) <$> databaseInfo
      in
        case firstValue of
          Success val -> 
            case val of
              Just entry -> putStrLn $ entrySnippet entry
              Nothing -> putStrLn "No first entry found"
          _ -> putStrLn "No first entry found"
    Error err -> putStrLn "Failed to load DB"
  --return ()

showEntries :: TestableMonadIO m => [Entry] -> m ()
showEntries entryList =
  case entryList of
    [] -> return ()
    x:xs -> putStrLn (show (FmtEntry x)) >> showEntries xs

-- | Handle the search command
handleSearch :: TestableMonadIO m => SearchOptions -> m ()
handleSearch searchOpts = do 
  databaseInfo <- DB.load
  case databaseInfo of
    Success value ->
      let
        allEntry = DB.findAll (\x -> Entry.Entry.matchedByAllQueries (searchOptTerms searchOpts) x) <$> databaseInfo
      in
        case allEntry of
          Success val -> 
            case val of
              [] -> putStrLn "No entries found"
              x:xs -> showEntries val
          _ -> putStrLn "No entries found"
    Error err -> putStrLn "Failed to load DB"

-- | Handle the add command
handleAdd :: TestableMonadIO m => AddOptions -> m ()
handleAdd addOpts = do
  databaseInfo <- DB.load
  src <- readFile (addOptFilename addOpts)

  case databaseInfo of
    Success a -> 
      let 
        databaseInsertWith = DB.insertWith (\id -> makeEntry id src addOpts) databaseEmpty where databaseEmpty = getSuccess databaseInfo DB.empty
        existsQuery = DB.findFirst(\elem -> entrySnippet elem == src) (getSuccess databaseInfo DB.empty)
      in

      case existsQuery of
        Just val -> Prelude.mapM_ putStrLn (["Entry with this content already exists: ", (show (FmtEntry val))])
        _ -> do
                DB.save databaseInsertWith
                return ()

    _ -> putStrLn "Failed to load DB"

  return ()
  where
    makeEntry :: Int -> String -> AddOptions -> Entry
    makeEntry id snippet addOpts =
      Entry
        { entryId = id,
          entrySnippet = snippet,
          entryFilename = addOptFilename addOpts,
          entryLanguage = addOptLanguage addOpts,
          entryDescription = addOptDescription addOpts,
          entryTags = addOptTags addOpts
        }
  -- return ()
  -- where
  --   makeEntry :: Int -> String -> AddOptions -> Entry
  --   makeEntry id snippet addOpts =
  --     Entry
  --       { entryId = id,
  --         entrySnippet = snippet,
  --         entryFilename = addOptFilename addOpts,
  --         entryLanguage = addOptLanguage addOpts,
  --         entryDescription = addOptDescription addOpts,
  --         entryTags = addOptTags addOpts
  --       }

-- | Dispatch the handler for each command
run :: TestableMonadIO m => Args -> m ()
run (Add addOpts) = handleAdd addOpts
run (Search searchOpts) = handleSearch searchOpts
run (Get getOpts) = handleGet getOpts
run Init = handleInit
run Help = putStrLn usageMsg

main :: IO ()
main = do
  args <- getArgs
  let parsed = parseArgs args
  case parsed of
    (Error err) -> Prelude.putStrLn usageMsg
    (Success args) -> run args
