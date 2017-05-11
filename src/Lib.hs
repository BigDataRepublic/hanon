module Lib
    ( scanFiles
    , translateFiles
    , listMapping
    ) where
import Mapper
import Control.Monad.IO.Class       (liftIO)
import Database.LevelDB.Higher
import qualified Data.ByteString.UTF8 as B
import qualified Data.ByteString.Char8 as BS
import System.IO



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
    mappedKeys <- mapM mapper keys
    writeMapping (zip keys mappedKeys)

runAllInputPaths :: String -> LevelDB()
runAllInputPaths = mapM_ runInputPath


writeMapping :: [(String, String)] -> LevelDB ()
writeMapping = mapM_ (\(k, v) -> put (B.fromString k) (B.fromString v))

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
    isoef <- hIsEOF fh
    if isoef
        then return ()
        else do line <- hGetLine fh
                return $ handler line

-- |Open each of the files and create translations for each candidate
scanFiles :: [FilePath] -> LevelDB ()
scanFiles = mapM_ scanFile

scanFile :: FilePath -> LevelDB ()
scanFile file = do
    fh <- liftIO $ openFile file ReadMode
    runOnLinesFromHandle runAllInputPaths fh
    mapMKeysFromFile generateMappingKeys fh
    liftIO $ hClose fh


mapFiles :: [FilePath] -> LevelDB ()
mapFiles _ = return ()
