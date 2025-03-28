module Main (main) where

import Test.Tasty (defaultMain, testGroup)

import qualified Property.PlyArg
import qualified Unit.Schema

main :: IO ()
main =
  defaultMain $
    testGroup
      "ply-core-tests"
      [ Unit.Schema.test
      , Property.PlyArg.test
      ]
