module Mapper
    ( InputPath
    , inputPaths
    , highlighters
    ) where

import Data.Random (runRVar)
import Data.Random.Source.DevRandom
import Data.Random.Extras (choice)

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

allWords :: Highlighter
allWords = words

getRandomWord :: MappingGenerator
getRandomWord _ = runRVar (choice someWords) DevURandom


-- |The input paths available
inputPaths :: [InputPath]
inputPaths = [
    (allWords, \_ -> return "banaan")
    ]

-- |The highlighters of all input paths
highlighters :: [Highlighter]
highlighters = map fst inputPaths
