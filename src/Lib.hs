module Lib
    ( scanFiles
    , mapFiles
    , listMapping
    , mapDirectFiles
    ) where
import Mapper
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, join)
import Database.LevelDB.Higher
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Char8 as BS
import System.IO (openFile, hClose, Handle(..), hIsEOF, IOMode(..))
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Text (Text (..))
import Data.Text.IO (hGetLine, hPutStrLn)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Function (on)
import Data.List (sortBy)

-- |Show all mappings available in the database
listMapping :: LevelDB ()
listMapping = do
    liftIO $ putStrLn "Listing dictionary"
    join $ scan (B.fromString "") queryBegins {
        scanInit = return ()
      , scanMap = \(key, value) -> liftIO $ BS.putStrLn $ BS.intercalate (B.fromString ":") [key, value]
      , scanFold = (>>)
    }


--Pattern match all keys from a line
keysFromLine :: Text -> [Text]
keysFromLine = T.words

readNextKeys :: Handle -> IO (Maybe [Text])
readNextKeys fh = do
    isoef <- hIsEOF fh
    if isoef
        then return Nothing
        else do l <- hGetLine fh
                return $ Just $ keysFromLine l


runInputPath :: InputPath -> Text -> LevelDB ()
runInputPath (highlighter, mapper) line = do
    let keys = highlighter line
    mappedKeys <- liftIO $ mapM mapper keys
    writeMapping (zip keys mappedKeys)

runAllInputPaths :: Text -> LevelDB()
runAllInputPaths line = mapM_ (`runInputPath` line) defaultInputPaths

writeMapping :: [(Text, Text)] -> LevelDB ()
writeMapping = mapM_ (\(k, v) -> put (encodeUtf8 k) (encodeUtf8 v))

-- readKey :: String -> LevelDB (String, String)

-- |Read all given keys from the database and return their mappings
readMapping :: [Text] -> LevelDB [(Text, Text)]
readMapping = mapM (\k -> do
    v <- get (encodeUtf8 k)
    return (k, decodeUtf8 $ fromJust v) --The key must exists, or scanning failed and we could just crash here
    )


mapMKeysFromFile :: ([Text] -> LevelDB ()) -> Handle -> LevelDB ()
mapMKeysFromFile handler handle = do
    next <- liftIO $ readNextKeys handle
    case next of
        Nothing -> return ()
        Just keys -> do
            handler keys
            mapMKeysFromFile handler handle


runOnLinesFromHandle :: (Text -> LevelDB ()) -> Handle -> LevelDB ()
runOnLinesFromHandle handler handle = do
    isoef <- liftIO $ hIsEOF handle
    unless isoef $ do
            line <- liftIO $ hGetLine handle
            handler line
            runOnLinesFromHandle handler handle

-- |Apply mapping from a to be on subject
applyMapping :: (Text, Text) -> Text -> Text
applyMapping (a, b) = T.replace a b

mapLine :: [(Text, Text)] -> Text -> Text
mapLine mapping line = foldr applyMapping line (sortBy (compare `on` (T.length . fst)) mapping)

mapLinesFromTo :: Highlighter -> Handle -> Handle -> LevelDB ()
mapLinesFromTo highlight ifh ofh = do
    iseof <- liftIO $ hIsEOF ifh
    unless iseof $ do
            line <- liftIO $ hGetLine ifh
            let keys = highlight line
            mapping <- readMapping keys
            let mappedLine = mapLine mapping line
            liftIO $ hPutStrLn ofh mappedLine
            mapLinesFromTo highlight ifh ofh

-- |Open each of the files and create translations for each candidate
scanFiles :: [FilePath] -> LevelDB ()
scanFiles = mapM_ scanFile

scanFile :: FilePath -> LevelDB ()
scanFile file = do
    liftIO $ putStrLn $ "Scanning " ++ file
    fh <- liftIO $ openFile file ReadMode
    runOnLinesFromHandle runAllInputPaths fh
    liftIO $ hClose fh


combinedHighlighter :: Highlighter
combinedHighlighter line = concatMap (\h -> h line) defaultHighlighters


mapFile :: FilePath -> LevelDB ()
mapFile inputPath = do
    liftIO $ putStrLn $ "Mapping " ++ inputPath
    let outputPath = inputPath ++ ".anon"
    ifh <- liftIO $ openFile inputPath ReadMode
    ofh <- liftIO $ openFile outputPath WriteMode
    mapLinesFromTo combinedHighlighter ifh ofh
    liftIO $ hClose ifh
    liftIO $ hClose ofh


mapFiles :: [FilePath] -> LevelDB ()
mapFiles = mapM_ mapFile

applyInputPath :: InputPath -> Text -> IO [(Text, Text)]
applyInputPath (highlighter, mapGenerator) line = do
    let keys = highlighter line
    mappedKeys <- mapM mapGenerator keys
    return $ zip keys mappedKeys


runInputPaths :: [InputPath] -> Text -> IO [(Text, Text)]
runInputPaths paths line = do
    mapped <- mapM (`applyInputPath` line) paths
    return $ concat mapped


mapDirectLinesFromTo :: [InputPath] -> Handle -> Handle -> IO ()
mapDirectLinesFromTo inputPaths ifh ofh = do
    iseof <- liftIO $ hIsEOF ifh
    unless iseof $ do
            line <- liftIO $ hGetLine ifh
            mapping <- runInputPaths inputPaths line
            let mappedLine = mapLine mapping line
            liftIO $ hPutStrLn ofh mappedLine
            mapDirectLinesFromTo inputPaths ifh ofh


mapDirectFile :: FilePath -> IO ()
mapDirectFile inputPath = do
    liftIO $ putStrLn $ "Directly mapping " ++ inputPath
    let outputPath = inputPath ++ ".anon"
    ifh <- liftIO $ openFile inputPath ReadMode
    ofh <- liftIO $ openFile outputPath WriteMode
    mapDirectLinesFromTo defaultInputPaths ifh ofh
    liftIO $ hClose ifh
    liftIO $ hClose ofh

mapDirectFiles :: [FilePath] -> IO ()
mapDirectFiles = mapM_ mapDirectFile
