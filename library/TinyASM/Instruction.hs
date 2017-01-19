module TinyASM.Instruction (
  -- Argument(..),
  Argument(..),
  Instruction(..),
  Operation(..)
) where

import TinyASM.ByteCode

data Argument = Address Integer | Literal Integer

instance Show Argument where
  show (Address a) = "[" ++ show a ++ "]"
  show (Literal a) = show a

data Instruction = And Argument Argument
                 | Or Argument Argument
                 | Xor Argument Argument
                 | Not Argument
                 | Mov Argument Argument
                 | Random Argument
                 | Add Argument Argument
                 | Sub Argument Argument
                 | Jmp Argument
                 | Jz Argument Argument
                 | Jeq Argument Argument Argument
                 | Jls Argument Argument Argument
                 | Jgt Argument Argument Argument
                 | Halt
                 | Aprint Argument
                 | Dprint Argument
                 deriving (Show)

class Operation a where
  toByteCode :: a -> ByteCode
  toByteCodeString :: a -> String

instance Operation Instruction where
  toByteCode (And (Address a) (Address b))  = BC3 0x00 a b
  toByteCode (And (Address a) (Literal b))  = BC3 0x01 a b

  toByteCode (Or  (Address a) (Address b))  = BC3 0x02 a b
  toByteCode (Or  (Address a) (Literal b))  = BC3 0x03 a b

  toByteCode (Xor (Address a) (Address b))  = BC3 0x04 a b
  toByteCode (Xor (Address a) (Literal b))  = BC3 0x05 a b

  toByteCode (Not (Address a))              = BC2 0x06 a

  toByteCode (Mov (Address a) (Address b))  = BC3 0x07 a b
  toByteCode (Mov (Address a) (Literal b))  = BC3 0x08 a b

  toByteCode (Random (Address a))           = BC2 0x09 a

  toByteCode (Add (Address a) (Address b))  = BC3 0x0a a b
  toByteCode (Add (Address a) (Literal b))  = BC3 0x0b a b

  toByteCode (Sub (Address a) (Address b))  = BC3 0x0c a b
  toByteCode (Sub (Address a) (Literal b))  = BC3 0x0d a b

  toByteCode (Jmp (Address a))              = BC2 0x0e a
  toByteCode (Jmp (Literal a))              = BC2 0x0f a

  toByteCode (Jz (Address a) (Address b))   = BC3 0x10 a b
  toByteCode (Jz (Address a) (Literal b))   = BC3 0x11 a b
  toByteCode (Jz (Literal a) (Address b))   = BC3 0x12 a b
  toByteCode (Jz (Literal a) (Literal b))   = BC3 0x13 a b

  toByteCode (Jeq (Address a) (Address b) (Address c))   = BC4 0x14 a b c
  toByteCode (Jeq (Literal a) (Address b) (Address c))   = BC4 0x15 a b c
  toByteCode (Jeq (Address a) (Address b) (Literal c))   = BC4 0x16 a b c
  toByteCode (Jeq (Literal a) (Address b) (Literal c))   = BC4 0x17 a b c

  toByteCode (Jls (Address a) (Address b) (Address c))   = BC4 0x18 a b c
  toByteCode (Jls (Literal a) (Address b) (Address c))   = BC4 0x19 a b c
  toByteCode (Jls (Address a) (Address b) (Literal c))   = BC4 0x1a a b c
  toByteCode (Jls (Literal a) (Address b) (Literal c))   = BC4 0x1b a b c

  toByteCode (Jgt (Address a) (Address b) (Address c))   = BC4 0x1c a b c
  toByteCode (Jgt (Literal a) (Address b) (Address c))   = BC4 0x1d a b c
  toByteCode (Jgt (Address a) (Address b) (Literal c))   = BC4 0x1e a b c
  toByteCode (Jgt (Literal a) (Address b) (Literal c))   = BC4 0x1f a b c

  toByteCode (Halt) = BC1 0xff

  toByteCode (Aprint (Address a)) = BC2 0x20 a
  toByteCode (Aprint (Literal a)) = BC2 0x21 a

  toByteCode (Dprint (Address a)) = BC2 0x22 a
  toByteCode (Dprint (Literal a)) = BC2 0x23 a

  toByteCode a = error $ "Unsupported Instruction '" ++ (show a) ++ "'"
  toByteCodeString = toString . toByteCode
