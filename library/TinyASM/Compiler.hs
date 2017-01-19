module TinyASM.Compiler (
  compileInstructions
)
where

import TinyASM.Instruction
import TinyASM.ByteCode

compileInstructions :: [Instruction] -> [Integer]
compileInstructions [] = []
compileInstructions (x:xs) = (compile $ toByteCode x) ++ (compileInstructions xs)
