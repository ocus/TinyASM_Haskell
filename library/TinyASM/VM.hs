module TinyASM.VM (
  VM(..),
  Executor(..)
)
where

import Prelude hiding (length, drop, take)
import Data.Sequence
import Data.Bits ((.&.))
import Data.Char (chr)
import Data.Foldable (toList)
import Text.Printf (printf)

type StackPointer = Integer
type Stack = Seq Integer

type Stop = Bool
type Memory = Seq Integer
type Screen = Seq String
type Result = (Stop, StackPointer, Memory, Screen)

data VM = VM {
            stack :: Stack,
            memory :: Memory,
            screen :: Screen
        } deriving (Show)

class Executor a where
  run :: a -> a

instance Executor VM where
  run vm@(VM _ _ _) = executeAtStackPointer 0 vm

executeAtStackPointer :: StackPointer -> VM -> VM
executeAtStackPointer cip (VM cstack cmemory cscreen) = nextInstruction VM { stack = cstack, memory = nmemory, screen = niscreen }
  where stackBottom = drop (fromIntegral cip) cstack
        (halt, nip, nmemory, nscreen) = if cip < stackSize then executeInstruction stackBottom cip cmemory cscreen else (True, cip, cmemory, cscreen)
        nextInstruction = if (not halt) && hasNextInstruction then (executeAtStackPointer nip) else id
        niscreen = if (not halt) && (not hasNextInstruction) then invalidInstruction else nscreen
        stackSize = fromIntegral $ length cstack
        hasNextInstruction = nip < stackSize
        invalidInstruction = (nscreen |> ("Invalid next stack-pointer " ++ (show nip) ++ " following " ++ (show (toList (take 1 stackBottom) !! 0))))
--
executeInstruction :: Stack -> StackPointer -> Memory -> Screen -> Result
executeInstruction cstack cip cmemory cscreen = (halt, nip, nmemory, nscreen)
  where (halt, nip, nmemory, nscreen) = case takeFromStack 4 of
           (0x00 : address1 : address2 : _)             -> (False, cip + 3, setAddr address1 (executeAddressAddress (.&.) address1 address2), printToScreenDbg ("AND ["++(show address1)++"] ["++(show address2)++"]")) -- AND [a] [b]
           (0x01 : address : value : _)                 -> (False, cip + 3, setAddr address (executeAddressLiteral (.&.) address value), printToScreenDbg ("AND ["++(show address)++"] "++(show value))) -- AND [a] b
           (0x08 : address : value : _)                 -> (False, cip + 3, setAddr address value, printToScreenDbg ("MOV ["++(show address)++"] "++(show value))) -- MOV [a] b
           (0x0a : address1 : address2 : _)             -> (False, cip + 3, setAddr address1 ((getAddr address1) + (getAddr address2)), printToScreenDbg ("ADD ["++(show address1)++"] ["++(show address2)++"]")) -- ADD [a] [b]
           (0x0b : address : value : _)                 -> (False, cip + 3, setAddr address ((getAddr address) + value), printToScreenDbg ("ADD ["++(show address)++"] ["++(show value)++"]")) -- ADD [a] b
           (0x0f : value : _)                           -> (False, toInteger value, cmemory, printToScreenDbg ("JMP "++(show value))) -- JMP a
           (0x14 : address1 : address2 : address3 : _)  -> (False, (if (executeAddressAddress (==) address2 address3) then getAddr address1 else cip + 4), cmemory, printToScreenDbg ("JEQ ["++(show address1)++"] ["++(show address2)++"] ["++(show address3)++"]")) -- JEQ [a] [b] [c]
           (0x15 : value : address1 : address2 : _)     -> (False, (if (executeAddressAddress (==) address1 address2) then toInteger value else cip + 4), cmemory, printToScreenDbg ("JEQ ["++(show address1)++"] ["++(show address2)++"]")) -- JEQ a [b] [c]
           (0x20 : address : _)                         -> (False, cip + 2, cmemory, printToScreen [chr $ fromIntegral $ getAddr address]) -- APRINT [a]
           (0x22 : address : _)                         -> (False, cip + 2, cmemory, printToScreen $ show $ getAddr address) -- DPRINT [a]
           (0x23 : value : _)                           -> (False, cip + 2, cmemory, printToScreen $ show $ value) -- DPRINT a
           (0xff : _)                                   -> (True,  cip + 1, cmemory, printToScreenDbg "HALT" ) -- HALT
           _                                            -> (True,  cip + 1, cmemory, printToScreen $ exception)

        debug = False
        opCode = (takeFromStack 1) !! 0
        takeFromStack :: Integer -> [Integer]
        takeFromStack n = map fromIntegral $ toList $ take (fromIntegral n) cstack
        getAddr :: Integer -> Integer
        getAddr address = index cmemory (fromIntegral address)
        setAddr :: Integer -> Integer -> Memory
        setAddr address value = update (fromIntegral address) value cmemory
        executeAddressLiteral :: (Integer -> Integer -> Integer) -> Integer -> Integer -> Integer
        executeAddressLiteral f a b = f (getAddr a) b
        executeAddressAddress f a b = f (getAddr a) (getAddr b)
        printToScreen value = cscreen |> value
        printToScreenDbg a = if debug then (printToScreen a) else cscreen
        exception = error $ "/!\\ Unsupported instruction " ++ (showHex $ opCode) ++ " at stack-pointer " ++ (show cip)

showHex :: Integer -> String
showHex = printf "0x%02x"
