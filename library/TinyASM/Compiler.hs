module TinyASM.Compiler (
  compileInstructions
)
where

import TinyASM.Instruction
import TinyASM.ByteCode

compileInstructions :: [Instruction] -> [Int]
compileInstructions [] = []
compileInstructions (x:xs) = (compile $ toByteCode x) ++ (compileInstructions xs)
