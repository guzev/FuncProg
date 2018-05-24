module Main where

import qualified Data.ByteString      as BS (readFile)
import           Parser               (runProgram)
import           System.Environment   (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let name = (head args)
    program <- BS.readFile name
    runProgram name program