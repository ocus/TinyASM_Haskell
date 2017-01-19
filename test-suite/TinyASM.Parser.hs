module Main ( main ) where

-- import System.Exit

import TinyASM.Instruction
import TinyASM.Parser
import TinyASM.Compiler
import TinyASM.VM
import Data.Sequence (fromList)

main :: IO ()
main = do
  let file =  "test-suite\\programs\\unit\\_all.asm"
  putStrLn $ "Parsing: " ++ file
  -- contents <- readFile "test-suite\\programs\\unit\\jeq_0x14.asm"
  contents <- readFile file
  let lns = contents
  let inst = parse lns
  -- inst <- fmap parse (readFile file)
      -- inst2 = parseString lns
  putStrLn $ "Contents:\n" ++ lns ++ "\n"
  putStrLn $ show inst
  putStrLn $ show $ compileInstructions inst
  -- putStrLn $ show lns
  -- putStrLn $ show $ inst
  let vm = run VM {
            stack = fromList $ compileInstructions inst,
            memory = fromList [ 0x00 | _ <- [(0 :: Integer)..255] ],
            screen = fromList []
          }
  putStrLn $ show $ vm
  -- let generatedByteCodeStrings = map toString byteCodes
  --     testResults = [
  --                   generatedByteCodeStrings == expectedByteCodeStrings
  --                 ]
  --
  -- if any (==False) testResults then exitFailure else return ()
