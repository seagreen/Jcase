module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy           as B
import           Data.HashMap.Strict            (HashMap)
import qualified Data.HashMap.Strict            as H
import           Data.Jcase                     (Jcase, jcaseToHUnit)
import           Data.JsonSchema
import           Data.Text                      (Text)
import qualified Data.Vector                    as V
import           Test.Framework                 (defaultMain)
import           Test.Framework.Providers.HUnit (testCase)
import qualified Test.HUnit                     as HU

main :: IO ()
main = do
  schemaObj <- getSchemaObj
  jcase     <- getJcase
  defaultMain
    [ testCase "The Jcase spec is a valid JSON Schema" (assertValid schemaObj)
    , jcaseToHUnit (f schemaObj) jcase
    ]
  where
    assertValid :: HashMap Text Value -> HU.Assertion
    assertValid h =
      HU.assertEqual "" V.empty (isValidSchema $ RawSchema "" h)

    f :: HashMap Text Value -> Maybe Value -> Value -> Value
    f h _ input =
      if V.null $ validate (compile draft4 H.empty $ RawSchema "" h) input
        then Bool True
        else Bool False

    getSchemaObj :: IO (HashMap Text Value)
    getSchemaObj = do
      schemaBytes <- B.readFile "jcase-schema.json"
      case eitherDecode schemaBytes of
        Left e  -> error e
        Right a -> return a

    getJcase :: IO Jcase
    getJcase = do
      testData <- B.readFile "test.json"
      case eitherDecode testData of
        Left e  -> error e
        Right a -> return a
