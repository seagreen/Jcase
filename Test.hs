module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy           as B
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as H
import           Data.Jcase                     (Jsuite, jsuiteToHUnit)
import           Data.JsonSchema
import           Data.Text                      (Text)
import qualified Data.Vector                    as V
import           Test.Framework                 (defaultMain)
import           Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit                     as HU

main :: IO ()
main = do
  jcaseSchema <- getJcaseSchema
  jcaseTests  <- getJcaseTests
  defaultMain
    [ testCase "The Jcase spec is a valid JSON Schema" (assertValid jcaseSchema)
    , jsuiteToHUnit (f jcaseSchema) jcaseTests
    ]
  where
    assertValid :: HashMap Text Value -> HU.Assertion
    assertValid h =
      HU.assertEqual "" V.empty (isValidSchema $ RawSchema "" h)

    f :: HashMap Text Value -> Maybe Value -> Value -> Value
    f h _ input =
      Bool . V.null $ validate (compile draft4 H.empty $ RawSchema "" h) input

    getJcaseSchema :: IO (HashMap Text Value)
    getJcaseSchema = do
      schemaBytes <- B.readFile "jcase-schema.json"
      case eitherDecode schemaBytes of
        Left e  -> error e
        Right a -> return a

    getJcaseTests :: IO Jsuite
    getJcaseTests = do
      testData <- B.readFile "test-jcase.json"
      case eitherDecode testData of
        Left e  -> error e
        Right a -> return a
