module Main ( main ) where

-- import System.Exit

-- import TinyASM.Instruction
import TinyASM.Parser
import TinyASM.Compiler
import TinyASM.VM
import System.Directory
import Data.List (isInfixOf)

main :: IO ()
main = do
  -- let files = [
  --               "test-suite\\programs\\unit\\add_0x0A.asm",
  --               "test-suite\\programs\\unit\\add_0x0B.asm"
  --             ]
  -- mapM_  runFile files

  allFiles <- getDirectoryContents "test-suite\\programs\\unit\\"
  let allUnitProgramFiles = reverse $ map ("test-suite\\programs\\unit\\"++) $ filter (isInfixOf "_0x") allFiles
  putStrLn $ show allUnitProgramFiles
  mapM_  runFile allUnitProgramFiles
  -- let generatedByteCodeStrings = map toString byteCodes
  --     testResults = [
  --                   generatedByteCodeStrings == expectedByteCodeStrings
  --                 ]
  --
  -- if any (==False) testResults then exitFailure else return ()

runFile :: String -> IO ()
runFile file = do
    putStrLn $ "- - - - - - - - - - - - - - - -\nParsing: " ++ file
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
              stack = compileInstructions inst,
              memory = [0x00 | _ <- [(0 :: Int)..255] ],
              screen = []
            }
    putStrLn $ show $ vm
