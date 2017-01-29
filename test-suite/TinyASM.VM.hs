module Main ( main ) where

import Prelude
import Data.Foldable (toList)
-- import Data.Sequence (fromList, Seq)
import Data.List (intercalate)
import Data.Bits
import System.Exit

import TinyASM.Compiler
import TinyASM.VM
import TinyASM.Instruction

main :: IO ()
main = do
  let programResults = map testProgram programs
  putStrLn $ intercalate "\n" [ out | (_, out) <- programResults ]

  if any (==False) [ success | (success, _) <- programResults ] then exitFailure else return ()

testProgram :: (String, [Instruction], [String]) -> (Bool, String)
testProgram (l, p, e) =  (success, "========== Program: " ++ l ++ "" ++ (if not success then "\n" ++ (showVM result) else "") ++ (if not success then showScreen "expected" e else "") ++ " =====> " ++ (status success) ++ "\n")
  where result = executeProgram $ compileInstructions p
        resultScreen = (screen result)
        success = e == resultScreen
        status True = "OK"
        status False = "/!\\ KO /!\\"

executeProgram :: [Int] -> VM
executeProgram st = run VM {
          stack = st,
          memory = [ 0x00 | _ <- [(0 :: Int)..255] ],
          screen = []
        }

showVM :: VM -> String
showVM vm@VM {} = (show vm) ++ (showScreen "result" (screen vm))

showScreen :: String -> ([String]) -> String
showScreen label l = "\nScreen (" ++ label ++ "):\n-------\n" ++ (intercalate "\n" (toList l)) ++ "\n-------"

programs :: [(String, [Instruction], [String])]
programs = [
    programAnd0x00 1 5,
    programAnd0x01 6 7,
    programAdd0x0a 13 12,
    programAdd0x0b 130 12,
    programJeq0x14 71 7 71 100 200 66,
    programJeq0x14 71 71 71 100 200 66,
    programJeq0x14 1 2 3 100 200 66,
    programJeq0x15 71 7 71 100 200 66,
    programJeq0x15 71 71 71 100 200 66,
    programJeq0x15 1 2 3 100 200 66,
    programHalt0xff
  ]


programAnd0x00 :: Int -> Int -> (String, [Instruction], [String])
programAnd0x00 a b = ("And0x00 " ++ (show a) ++ " " ++ (show b), [
    Mov (Address 0) (Literal a),
    Mov (Address 1) (Literal b),
    And (Address 0) (Address 1),
    Dprint (Address 0),
    Halt
  ], [show (a .&. b)])

programAnd0x01 :: Int -> Int -> (String, [Instruction], [String])
programAnd0x01 a b = ("And0x01 " ++ (show a) ++ " " ++ (show b), [
    Mov (Address 0) (Literal a),
    And (Address 0) (Literal b),
    Dprint (Address 0),
    Halt
  ], [show (a .&. b)])

programAdd0x0a :: Int -> Int -> (String, [Instruction], [String])
programAdd0x0a a b = ("Add0x0a " ++ (show a) ++ " " ++ (show b), [
    Mov (Address 0) (Literal a),
    Mov (Address 1) (Literal b),
    Add (Address 0) (Address 1),
    Dprint (Address 0),
    Halt
  ], [show (a + b)])

programAdd0x0b :: Int -> Int -> (String, [Instruction], [String])
programAdd0x0b a b = ("Add0x0b " ++ (show a) ++ " " ++ (show b), [
    Mov (Address 0) (Literal a),
    Add (Address 0) (Literal b),
    Dprint (Address 0),
    Halt
  ], [show (a + b)])

programJeq0x14 :: Int -> Int -> Int -> Int -> Int -> Int -> (String, [Instruction], [String])
programJeq0x14 a b c d e f = ("Jeq0x14 " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e) ++ " " ++ (show f) ++ " -> " ++ (show expected), [
    -- if   a == b == c then print f
    -- elif a == c then print d
    -- else print e
    Mov (Address 0) (Literal a),
    Mov (Address 1) (Literal b),
    Mov (Address 2) (Literal c),
    Mov (Address 3) (Literal 22),
    Mov (Address 4) (Literal 29),
    Jeq (Address 3) (Address 0) (Address 2),
    Dprint (Literal d),
    Halt,
    Jeq (Address 4) (Address 0) (Address 1),
    Dprint (Literal e),
    Halt,
    Dprint (Literal f),
    Halt
  ], [show expected])
  where expected = if a == c && a == b then f else (if a == c then e else d)


programJeq0x15 :: Int -> Int -> Int -> Int -> Int -> Int -> (String, [Instruction], [String])
programJeq0x15 a b c d e f = ("Jeq0x15 " ++ (show a) ++ " " ++ (show b) ++ " " ++ (show c) ++ " " ++ (show d) ++ " " ++ (show e) ++ " " ++ (show f) ++ " -> " ++ (show expected), [
    -- if   a == b == c then print f
    -- elif a == c then print d
    -- else print e
    Mov (Address 0) (Literal a),
    Mov (Address 1) (Literal b),
    Mov (Address 2) (Literal c),
    Jeq (Literal 16) (Address 0) (Address 2),
    Dprint (Literal d),
    Halt,
    Jeq (Literal 23) (Address 0) (Address 1),
    Dprint (Literal e),
    Halt,
    Dprint (Literal f),
    Halt
  ], [show expected])
  where expected = if a == c && a == b then f else (if a == c then e else d)

programHalt0xff :: (String, [Instruction], [String])
programHalt0xff = ("Halt0xff", [
    Halt
  ], [])
