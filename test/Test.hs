module Main where

import qualified Data.Aeson.Schema.V7.TestSuite as TestSuite

import qualified System.Directory as Dir
import qualified Test.Tasty as Tasty

main = do
  directory <- Dir.getCurrentDirectory
  testTree <- TestSuite.buildTestsFromPath (directory <> "/test/test-suite/tests/draft7/")
  Tasty.defaultMain testTree
