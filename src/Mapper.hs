module Mapper
    ( InputPath
    , Highlighter
    , defaultInputPaths
    , defaultHighlighters
    , namesHighlighter
    , phoneNumberHighlighter
    ) where

import Data.Random (runRVar)
import Data.Random.Source.DevRandom
import Data.Random.Extras (choice)
import Text.Regex.TDFA
import Text.Regex.TDFA.Common (Regex (..))
import Data.Text (Text (..))
import qualified Data.Text as T

someWords :: [Text]
someWords = (T.words . T.pack) "stane furor polder uppsala atomised ruffler paten recco hipping calcaneus wampanoag eulogium brainier semipious legalised vinethene \
    \ unvirile mignonne untelic seasick umtali nontonic curler oeuvre ube boggart megiddo seconde juryless trounce tarn korona unfealty corrade \
    \ rompingly tachisme greer unaverred revetment nitralloy solarium depositor cong comate matlock fromentin acetal darlan field favours paragraph erlanger \
    \ taconite facilely nooky passable tableaux regarding hooke boggler topeka insular microcopy tsaritsyn cumulated lasket syruplike telegony eagre unjamming \
    \ simbirsk judaized substrate sulawesi jemadar preta rebind psalmody perigone euthenist dean wove grunth sarabande reembrace ller nccl sightable keb dentes \
    \ degassing hooves vigilante rockiness varanasi couchant porrect subjugate"

-- |Find text that should be replaced with something
type Highlighter = Text -> [Text]

-- |Generate a new replacement from a given key
type MappingGenerator = Text -> IO Text

-- |Everything required to make a concrete mapping from a line
type InputPath = (Highlighter, MappingGenerator)

-- |Highlight helper from regex and group index to key listing
regexHighlighter :: Regex -> Int -> Highlighter
regexHighlighter re groupIndex subject = map (T.pack . (!! groupIndex)) (match re (T.unpack subject) :: [[String]])

-- |Highlight any x@x.x string
emailHighlighter :: Highlighter
emailHighlighter = regexHighlighter (makeRegex "[^[:space:]]+@[^[:space:]]+\\.[^[:space:]]+") 0

-- |Highlight anything that vagely resembles a phone number
phoneNumberHighlighter :: Highlighter
phoneNumberHighlighter = regexHighlighter (makeRegex "[+]?[0-9]{8,13}") 0

dutchPostalCodeHighlighter :: Highlighter
dutchPostalCodeHighlighter = regexHighlighter (makeRegex "[0-9]{4} *[A-Za-z]{2}") 0

namesHighlighter :: Highlighter
namesHighlighter = regexHighlighter (makeRegex "[A-Z][a-z]+ +[A-Z][a-z]+") 0



-- |Generate random@random.com
randomEmail :: MappingGenerator
randomEmail _ = do
    a <- getRandomWord
    b <- getRandomWord
    return $ T.concat [a, T.pack "@", b, T.pack ".com"]



-- |MappingGenerator that always results in a constant value
constant :: String -> MappingGenerator
constant v _ = return $ T.pack v

-- |Get a random word from a small dictionary
getRandomWord :: IO Text
getRandomWord = runRVar (choice someWords) DevURandom


-- |The input paths available
defaultInputPaths :: [InputPath]
defaultInputPaths = [
    (emailHighlighter, randomEmail)
  , (dutchPostalCodeHighlighter, constant "1234AA")
  , (namesHighlighter, constant "Willem Wever")
  , (phoneNumberHighlighter, constant "03012345678")
    ]

-- |The highlighters of all input paths
defaultHighlighters :: [Highlighter]
defaultHighlighters = map fst defaultInputPaths
