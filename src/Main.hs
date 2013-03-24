module Main where

import System.Environment
import Hsparse.Parser
import Hsparse.Evaluator


main :: IO ()
main = getArgs >>= print . eval . readExpr . head
