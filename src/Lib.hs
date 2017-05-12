module Lib
    ( scanFiles
    , mapFiles
    , listMapping
    ) where
import Mapper
import Control.Monad.IO.Class       (liftIO)
import Database.LevelDB.Higher
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Char8 as BS
import System.IO
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Data.Function (on)
import Data.List (sortBy)

-- |Show all mappings available in the database
listMapping :: LevelDB ()
listMapping = do
    liftIO $ putStrLn "Listing dictionary"
    linesMeantForOutput <- scan (B.fromString "") queryList {
        scanMap = \(key, value) -> BS.intercalate (B.fromString ":") [key, value]
    }
    liftIO $ mapM_ BS.putStrLn linesMeantForOutput


--Pattern match all keys from a line
keysFromLine :: String -> [String]
keysFromLine = words

readNextKeys :: Handle -> IO (Maybe [String])
readNextKeys fh = do
    isoef <- hIsEOF fh
    if isoef
        then return Nothing
        else do l <- hGetLine fh
                return $ Just $ keysFromLine l


runInputPath :: InputPath -> String -> LevelDB ()
runInputPath (highlighter, mapper) line = do
    let keys = highlighter line
    mappedKeys <- liftIO $ mapM mapper keys
    writeMapping (zip keys mappedKeys)

runAllInputPaths :: String -> LevelDB()
runAllInputPaths line = mapM_ (\path -> runInputPath path line) inputPaths


writeMapping :: [(String, String)] -> LevelDB ()
writeMapping = mapM_ (\(k, v) -> put (B.fromString k) (B.fromString v))

-- readKey :: String -> LevelDB (String, String)

readMapping :: [String] -> LevelDB [(String, String)]
readMapping keys = mapM (\k -> do
    v <- get (B.fromString k)
    return (k, B.toString $ fromJust v) --The key must exists, or scanning failed and we could just crash here
    ) keys


mapMKeysFromFile :: ([String] -> LevelDB ()) -> Handle -> LevelDB ()
mapMKeysFromFile handler handle = do
    next <- liftIO $ readNextKeys handle
    case next of
        Nothing -> return ()
        Just keys -> do
            handler keys
            mapMKeysFromFile handler handle


runOnLinesFromHandle :: (String -> LevelDB ()) -> Handle -> LevelDB ()
runOnLinesFromHandle handler handle = do
    isoef <- liftIO $ hIsEOF handle
    if isoef
        then return ()
        else do line <- liftIO $ hGetLine handle
                handler line
                runOnLinesFromHandle handler handle

applyMapping :: (String, String) -> String -> String
applyMapping (a, b) line = T.unpack $ T.replace (T.pack a) (T.pack b) (T.pack line)

mapLinesFromTo :: Highlighter -> Handle -> Handle -> LevelDB ()
mapLinesFromTo highlight ifh ofh = do
    isoef <- liftIO $ hIsEOF ifh
    if isoef
        then return ()
        else do line <- liftIO $ hGetLine ifh
                let keys = highlight line
                mapping <- readMapping keys
                let mappedLine = foldr applyMapping line (sortBy (compare `on` (length . fst)) mapping)
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
combinedHighlighter line = concatMap (\h -> h line) highlighters


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
