{-# LANGUAGE OverloadedStrings #-}

import LispVal
import Parser

import qualified Data.Text as T

import Test.Hspec
import System.IO.Unsafe

main :: IO ()
main = hspec $ describe "src/Parser.hs" $ do
    it "Atom" $
        readExpr "bb-8?" `shouldBe` Right (Atom "bb-8?")

    it "Num Negative" $
        readExpr "-83321" `shouldBe` Right (Number (-83321))

    it "Num Positive" $
        readExpr "112233" `shouldBe` Right (Number 112233)

    it "Num Positive with Sign" $
        readExpr "+12345" `shouldBe` Right (Number 12345)

    it "String" $
        readExpr "\"Gen L Organa\"" `shouldBe` Right (String "Gen L Organa")

    it "Bool True" $
        readExpr "#t" `shouldBe` Right (Bool True)

    it "Bool False" $
        readExpr "#f" `shouldBe` Right (Bool False)

    it "Quoted List" $
        readExpr "'(1 2 3)" `shouldBe` Right (List [Atom "quote", List [Number 1, Number 2, Number 3]])

    it "Nil" $
        readExpr "'()" `shouldBe` Right Nil
