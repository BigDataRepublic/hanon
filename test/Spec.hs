{-# LANGUAGE OverloadedStrings #-}
import Test.Hspec
import Mapper
import Data.Text (Text(..))

main :: IO ()
main = hspec $ do
    describe "namesHighlighter" $ do
        it "can parse plain strings" $ do
            namesHighlighter "Hello There" `shouldBe` (["Hello There"] :: [Text])
        it "can parse UTF-8 strings" $ do
            namesHighlighter "‘I’ll tell it her,’ said the Mock Turtle in a deep, hollow tone: ‘sit" `shouldBe` (["Mock Turtle"] :: [Text])
    describe "phoneNumberHighlighter" $ do
        it "can parse flat number" $ do
            phoneNumberHighlighter " 0123345678asd " `shouldBe` (["0123345678"] :: [Text])
        it "can parse plus notation" $ do
            phoneNumberHighlighter "+31123345678asd" `shouldBe` (["+31123345678"] :: [Text])


--
