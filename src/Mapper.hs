module Mapper
    ( InputPath
    , Highlighter
    , inputPaths
    , highlighters
    , namesHighlighter
    , phoneNumberHighlighter
    ) where

import Data.Random (runRVar)
import Data.Random.Source.DevRandom
import Data.Random.Extras (choice)
import Text.Regex.TDFA

someWords = words "stane furor polder uppsala atomised ruffler paten recco hipping calcaneus wampanoag eulogium brainier semipious legalised vinethene \
    \ unvirile mignonne untelic seasick umtali nontonic curler oeuvre ube boggart megiddo seconde juryless trounce tarn korona unfealty corrade \
    \ rompingly tachisme greer unaverred revetment nitralloy solarium depositor cong comate matlock fromentin acetal darlan field favours paragraph erlanger \
    \ taconite facilely nooky passable tableaux regarding hooke boggler topeka insular microcopy tsaritsyn cumulated lasket syruplike telegony eagre unjamming \
    \ simbirsk judaized substrate sulawesi jemadar preta rebind psalmody perigone euthenist dean wove grunth sarabande reembrace ller nccl sightable keb dentes \
    \ degassing hooves vigilante rockiness varanasi couchant porrect subjugate"

-- |Find text that should be replaced with something
type Highlighter = String -> [String]

-- |Generate a new replacement from a given key
type MappingGenerator = String -> IO String

-- |Everything required to make a concrete mapping from a line
type InputPath = (Highlighter, MappingGenerator)

-- |Highlight helper from regex and group index to key listing
regexHighlighter :: String -> Int -> Highlighter
regexHighlighter re groupIndex subject = map (!! groupIndex) (subject =~ re :: [[String]])

-- http://gabebw.com/blog/2015/10/11/regular-expressions-in-haskell
-- (BC.pack "hello there") =~ "e" :: AllTextMatches [] BC.ByteString
-- bsResult :: AllTextMatches [] BC.ByteString

-- |Highlight any x@x.x string
emailHighlighter :: Highlighter
emailHighlighter = regexHighlighter "[^[:space:]]+@[^[:space:]]+\\.[^[:space:]]+" 0

phoneNumberHighlighter :: Highlighter
phoneNumberHighlighter = regexHighlighter "[+]?[0-9]{8,13}" 0

dutchPostalCodeHighlighter :: Highlighter
dutchPostalCodeHighlighter = regexHighlighter "[0-9]{4} *[A-Za-z]{2}" 0

namesHighlighter :: Highlighter
namesHighlighter = regexHighlighter "[A-Z][a-z]+ +[A-Z][a-z]+" 0



-- |Generate random@random.com
randomEmail :: MappingGenerator
randomEmail _ = do
    a <- getRandomWord
    b <- getRandomWord
    return $ a ++ "@" ++ b ++ ".com"



-- |MappingGenerator that always results in a constant value
constant :: String -> MappingGenerator
constant v _ = return v

-- |Get a random word from a small dictionary
getRandomWord :: IO String
getRandomWord = runRVar (choice someWords) DevURandom


-- |The input paths available
inputPaths :: [InputPath]
inputPaths = [
    (emailHighlighter, randomEmail)
  , (dutchPostalCodeHighlighter, constant "1234AA")
  , (namesHighlighter, constant "Willem Wever")
  , (phoneNumberHighlighter, constant "03012345678")
    ]

-- |The highlighters of all input paths
highlighters :: [Highlighter]
highlighters = map fst inputPaths
