{-# LANGUAGE DuplicateRecordFields #-}
module Data.Aeson.Schema.V7.TestSuite
  ( buildTestsFromPath
  ) where

import qualified Data.Aeson.Schema.V7 as Schema.V7
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.BetterErrors as Aeson.BE
import           Data.Text (Text)
import qualified Data.Text.IO as T.IO
import           Data.ByteString.Lazy (hGetContents)

import qualified System.IO as IO
import qualified System.FilePath.Find as IO.Find

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.HUnit as Tasty.HUnit

buildTestsFromPath :: IO.FilePath -> IO Tasty.TestTree
buildTestsFromPath underPath = do
  testFiles <- IO.Find.find (pure True) ((== ".json") <$> IO.Find.extension) underPath

  Tasty.testGroup "All tests " <$> traverse buildTestFromPath testFiles

data Test
  = Test
      { description :: String
      , schema :: Schema.V7.Schema
      , tests :: [TestCase]
      }

asTests :: Aeson.BE.Parse Text [Test]
asTests = Aeson.BE.eachInArray asTest

asTest :: Aeson.BE.Parse Text Test
asTest = do
  description <- Aeson.BE.key "description" Aeson.BE.asString
  schema <- Aeson.BE.key "schema" (Aeson.BE.withValueM Schema.V7.parseSchemaSuppressingWarnings)
  tests <- Aeson.BE.key "tests" (Aeson.BE.eachInArray asTestCase)

  pure Test{..}

buildTestFromPath :: IO.FilePath -> IO Tasty.TestTree
buildTestFromPath path = do
  fileHandle <- IO.openFile path IO.ReadMode
  fileContents <- hGetContents fileHandle

  case Aeson.BE.parse asTests fileContents of
    Left err -> do
      traverse (T.IO.hPutStrLn IO.stderr) (Aeson.BE.displayError id err)

      pure (badParseTestCase path)
    Right tests -> do
      let testName = "Tests under `" <> path <> "`"

      pure (Tasty.testGroup testName (buildTest <$> tests))

badParseTestCase :: IO.FilePath -> Tasty.TestTree
badParseTestCase path
  = Tasty.HUnit.testCase ("Failed to parse tests under `" <> path <> "`, skipping...")
      (pure ())

buildTest :: Test -> Tasty.TestTree

buildTest Test{..}
  = Tasty.testGroup description
      (buildTestCase schema <$> tests)

data TestCase
  = TestCase
      { description :: String
      , testData :: Aeson.Value
      , valid :: Bool
      }

asTestCase :: Aeson.BE.Parse Text TestCase
asTestCase = do
  description <- Aeson.BE.key "description" Aeson.BE.asString
  testData <- Aeson.BE.key "data" Aeson.BE.asValue
  valid <- Aeson.BE.key "valid" Aeson.BE.asBool

  pure TestCase{..}

buildTestCase :: Schema.V7.Schema -> TestCase -> Tasty.TestTree

buildTestCase schema TestCase{..}
  = Tasty.HUnit.testCase description
      (valid Tasty.HUnit.@=? Schema.V7.validate schema testData)
