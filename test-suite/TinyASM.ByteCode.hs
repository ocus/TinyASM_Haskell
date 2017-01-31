module Main ( main ) where

import TinyASM.ByteCode
import System.Exit

main :: IO ()
main = do
  let generatedByteCodeStrings = map show byteCodes
      testResults = [
                    generatedByteCodeStrings == expectedByteCodeStrings
                  ]

  if any (==False) testResults then exitFailure else return ()

byteCodes :: [ByteCode]
byteCodes = [
            BC1 0x01,
            BC2 0x02 5,
            BC3 0x03 6 7,
            BC4 0x04 8 9 10
          ]

expectedByteCodeStrings :: [String]
expectedByteCodeStrings = [
                          "ByteCode 0x01",
                          "ByteCode 0x02 0x05",
                          "ByteCode 0x03 0x06 0x07",
                          "ByteCode 0x04 0x08 0x09 0x0a"
                        ]
