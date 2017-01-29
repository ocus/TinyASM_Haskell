module TinyASM.VM (
  VM(..),
  Executor(..)
)
where

import Prelude
import Data.Bits ((.&.), (.|.), xor, complement)
import Data.Char (chr)
import Text.Printf (printf)

import TinyASM.ByteCode

type StackPointer = Int
type Stack = [Int]

type Memory = [Int]
type Screen = [String]
type Context = (StackPointer, Memory, Screen)
type Result = (Maybe StackPointer, Memory, Screen)

data VM = VM {
            stack :: Stack,
            memory :: Memory,
            screen :: Screen
        } deriving (Show)

class Executor a where
  run :: a -> a

instance Executor VM where
  run (VM st mem scr) = VM st updatedMem updatedScreen
    where (_, updatedMem, updatedScreen) = executeAtStackPointer st (Just 0, mem, scr)

executeAtStackPointer :: Stack -> Result -> Result
executeAtStackPointer st (Just sp, mem, scr) = executeAtStackPointer st $ handleResult $ executeByteCode byteCode (sp, mem, scr)
  where
    byteCode = extractByteCode sp st
    handleResult (Just result) = result
    handleResult Nothing = error $ "Unexecutable '" ++ show byteCode ++ "' Context: " ++ show (sp, mem, scr)
executeAtStackPointer _ x = x

executeByteCode :: ByteCode -> Context -> Maybe Result
executeByteCode bc@(BC3 0x00 address1 address2)           (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address1 ((mem !! address1) .&. (mem !! address2)) mem, scr)
executeByteCode bc@(BC3 0x01 address value)               (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address ((mem !! address) .&. value) mem, scr)
executeByteCode bc@(BC3 0x02 address1 address2)           (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address1 ((mem !! address1) .|. (mem !! address2)) mem, scr)
executeByteCode bc@(BC3 0x03 address value)               (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address ((mem !! address) .|. value) mem, scr)
executeByteCode bc@(BC3 0x04 address1 address2)           (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address1 ((mem !! address1) `xor` (mem !! address2)) mem, scr)
executeByteCode bc@(BC3 0x05 address value)               (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address ((mem !! address) `xor` value) mem, scr)
executeByteCode bc@(BC2 0x06 address)                     (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address (complement (mem !! address)) mem, scr)
executeByteCode bc@(BC3 0x07 address1 address2)           (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address1 (mem !! address2) mem, scr)
executeByteCode bc@(BC3 0x08 address value)               (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address value mem, scr)
executeByteCode bc@(BC2 0x09 address)                     (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address 4 mem, scr)  -- choosen by fair dice roll. guaranteed to be random. TODO just kidding, FIX THIS !!!
executeByteCode bc@(BC3 0x0a address1 address2)           (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address1 ((mem !! address1) + (mem !! address2)) mem, scr)
executeByteCode bc@(BC3 0x0b address value)               (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address ((mem !! address) + value) mem, scr)
executeByteCode bc@(BC3 0x0c address1 address2)           (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address1 ((mem !! address1) - (mem !! address2)) mem, scr)
executeByteCode bc@(BC3 0x0d address value)               (sp, mem, scr) = Just (Just (nextPointer sp bc), updateList address ((mem !! address) - value) mem, scr)
executeByteCode    (BC2 0x0e address)                     (_,  mem, scr) = Just (Just (mem !! address), mem, scr)
executeByteCode    (BC2 0x0f value)                       (_,  mem, scr) = Just (Just value, mem, scr)
executeByteCode bc@(BC3 0x10 address1 address2)           (sp, mem, scr) = Just (Just (if 0 == (mem !! address2) then (mem !! address1) else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC3 0x11 address value)               (sp, mem, scr) = Just (Just (if 0 == value then (mem !! address) else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC3 0x12 value address)               (sp, mem, scr) = Just (Just (if 0 == (mem !! address) then value else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC3 0x13 value1 value2)               (sp, mem, scr) = Just (Just (if 0 == value2 then value1 else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x14 address1 address2 address3)  (sp, mem, scr) = Just (Just (if (mem !! address2) == (mem !! address3) then (mem !! address1) else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x15 value address1 address2)     (sp, mem, scr) = Just (Just (if (mem !! address1) == (mem !! address2) then value else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x16 address1 address2 value)     (sp, mem, scr) = Just (Just (if (mem !! address2) == value then (mem !! address1) else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x17 value1 address1 value2)      (sp, mem, scr) = Just (Just (if (mem !! address1) == value2 then value1 else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x18 address1 address2 address3)  (sp, mem, scr) = Just (Just (if (mem !! address2) < (mem !! address3) then (mem !! address1) else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x19 value address1 address2)     (sp, mem, scr) = Just (Just (if (mem !! address1) < (mem !! address2) then value else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x1a address1 address2 value)     (sp, mem, scr) = Just (Just (if (mem !! address2) < value then (mem !! address1) else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x1b value1 address1 value2)      (sp, mem, scr) = Just (Just (if (mem !! address1) < value2 then value1 else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x1c address1 address2 address3)  (sp, mem, scr) = Just (Just (if (mem !! address2) > (mem !! address3) then (mem !! address1) else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x1d value address1 address2)     (sp, mem, scr) = Just (Just (if (mem !! address1) > (mem !! address2) then value else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x1e address1 address2 value)     (sp, mem, scr) = Just (Just (if (mem !! address2) > value then (mem !! address1) else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC4 0x1f value1 address1 value2)      (sp, mem, scr) = Just (Just (if (mem !! address1) > value2 then value1 else (nextPointer sp bc)), mem, scr)
executeByteCode bc@(BC2 0x20 address)                     (sp, mem, scr) = Just (Just (nextPointer sp bc), mem, scr ++ [[chr $ mem !! address]])
executeByteCode bc@(BC2 0x21 value)                       (sp, mem, scr) = Just (Just (nextPointer sp bc), mem, scr ++ [[chr value]])
executeByteCode bc@(BC2 0x22 address)                     (sp, mem, scr) = Just (Just (nextPointer sp bc), mem, scr ++ [show $ mem !! address])
executeByteCode bc@(BC2 0x23 value)                       (sp, mem, scr) = Just (Just (nextPointer sp bc), mem, scr ++ [show value])
executeByteCode    (BC1 0xff)                             (_,  mem, scr) = Just (Nothing, mem, scr)
executeByteCode _ _ = Nothing

nextPointer :: StackPointer -> ByteCode -> StackPointer
nextPointer sp bc = sp + (size bc)

extractByteCode :: StackPointer -> Stack -> ByteCode
extractByteCode sp st = extract $ drop sp st
  where
    extract :: Stack -> ByteCode
    extract s@(0x00 : _) = toBC3 s  -- AND [a] [b]
    extract s@(0x01 : _) = toBC3 s  -- AND [a] b
    extract s@(0x02 : _) = toBC3 s  -- OR [a] [b]
    extract s@(0x03 : _) = toBC3 s  -- OR [a] b
    extract s@(0x04 : _) = toBC3 s  -- XOR [a] [b]
    extract s@(0x05 : _) = toBC3 s  -- XOR [a] b
    extract s@(0x06 : _) = toBC2 s  -- NOT [a]
    extract s@(0x07 : _) = toBC3 s  -- MOV [a] [b]
    extract s@(0x08 : _) = toBC3 s  -- MOV [a] b
    extract s@(0x09 : _) = toBC2 s  -- RANDOM [a]
    extract s@(0x0a : _) = toBC3 s  -- ADD [a] [b]
    extract s@(0x0b : _) = toBC3 s  -- ADD [a] b
    extract s@(0x0c : _) = toBC3 s  -- SUB [a] [b]
    extract s@(0x0d : _) = toBC3 s  -- SUB [a] b
    extract s@(0x0e : _) = toBC2 s  -- JMP [x]
    extract s@(0x0f : _) = toBC2 s  -- JMP x
    extract s@(0x10 : _) = toBC3 s  -- JZ [x] [a]
    extract s@(0x11 : _) = toBC3 s  -- JZ [x] a
    extract s@(0x12 : _) = toBC3 s  -- JZ x [a]
    extract s@(0x13 : _) = toBC3 s  -- JZ x a
    extract s@(0x14 : _) = toBC4 s  -- JEQ [x] [a] [b]
    extract s@(0x15 : _) = toBC4 s  -- JEQ x [a] [b]
    extract s@(0x16 : _) = toBC4 s  -- JEQ [x] [a] b
    extract s@(0x17 : _) = toBC4 s  -- JEQ [x] [a] b
    extract s@(0x18 : _) = toBC4 s  -- JLS [x] [a] [b]
    extract s@(0x19 : _) = toBC4 s  -- JLS x [a] [b]
    extract s@(0x1a : _) = toBC4 s  -- JLS [x] [a] b
    extract s@(0x1b : _) = toBC4 s  -- JLS [x] [a] b
    extract s@(0x1c : _) = toBC4 s  -- JGT [x] [a] [b]
    extract s@(0x1d : _) = toBC4 s  -- JGT x [a] [b]
    extract s@(0x1e : _) = toBC4 s  -- JGT [x] [a] b
    extract s@(0x1f : _) = toBC4 s  -- JGT [x] [a] b
    extract s@(0x20 : _) = toBC2 s  -- APRINT [a]
    extract s@(0x21 : _) = toBC2 s  -- APRINT a
    extract s@(0x22 : _) = toBC2 s  -- DPRINT [a]
    extract s@(0x23 : _) = toBC2 s  -- DPRINT a
    extract s@(0xff : _) = toBC1 s  -- HALT
    extract _ = error $ "Cannot extract ByteCode at " ++ showHex sp ++ " from stack " ++ show st

toBC1 :: Stack -> ByteCode
toBC1 s = BC1 (s !! 0)

toBC2 :: Stack -> ByteCode
toBC2 s = BC2 (s !! 0) (s !! 1)

toBC3 :: Stack -> ByteCode
toBC3 s = BC3 (s !! 0) (s !! 1) (s !! 2)

toBC4 :: Stack -> ByteCode
toBC4 s = BC4 (s !! 0) (s !! 1) (s !! 2) (s !! 3)

updateList :: Int -> a -> [a] -> [a]
updateList n v l = (take n l) ++ [v] ++ (drop (n + 1) l)

showHex :: Int -> String
showHex = printf "0x%02x"
