module TinyASM.Parser (
  parse
)
where

import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many)

import TinyASM.Instruction

parse :: String -> [Instruction]
parse "" = []
parse content = case readP_to_S instructions content of
  [] -> []
  result -> case last result of
    (_, remains@(_:_)) -> error $ show $ "Parsing error at: " ++ remains
    (parsed, _) -> fmap extractInstruction parsed

extractInstruction :: (Either String Instruction) -> Instruction
extractInstruction (Left e) = error e
extractInstruction (Right i) = i

instructions :: ReadP [(Either String Instruction)]
instructions = do
  is <- sepBy numberedInstruction $ many1 $ string "\n"
  _ <- option "" $ string "\n"
  return (is)

numberedInstruction :: ReadP (Either String Instruction)
numberedInstruction = do
  i <- instruction
  return (i)

instruction :: ReadP (Either String Instruction)
instruction = do
  i  <- (instructionWith2Arguments "AND" And)
    <|> (instructionWith2Arguments "OR" Or)
    <|> (instructionWith2Arguments "XOR" Xor)
    <|> (instructionWith1Argument  "NOT" Not)

    <|> (instructionWith2Arguments "MOV" Mov)

    <|> (instructionWith1Argument  "RANDOM" Random)

    <|> (instructionWith2Arguments "ADD" Add)
    <|> (instructionWith2Arguments "SUB" Sub)

    <|> (instructionWith1Argument  "JMP" Jmp)
    <|> (instructionWith2Arguments "JZ" Jz)
    <|> (instructionWith3Arguments "JEQ" Jeq)
    <|> (instructionWith3Arguments "JLS" Jls)
    <|> (instructionWith3Arguments "JGT" Jgt)

    <|> (instructionWith1Argument  "APRINT" Aprint)
    <|> (instructionWith1Argument  "DPRINT" Dprint)

    <|> (instructionWithoutArgument "HALT" Halt)
  return (i)

instructionWithoutArgument :: String -> b -> ReadP (Either String b)
instructionWithoutArgument mne t = do
  _ <- string mne
  return $ Right t

instructionWith1Argument :: String -> (Argument -> b) -> ReadP (Either String b)
instructionWith1Argument mne t = do
  _ <- string mne
  skipSpaces
  argument <- oneArgument
  return $ Right $ t argument

instructionWith2Arguments :: String -> (Argument -> Argument -> b) -> ReadP (Either String b)
instructionWith2Arguments mne t = do
  _ <- string mne
  skipSpaces
  (arg1, arg2) <- twoArguments
  return $ Right $ t arg1 arg2

instructionWith3Arguments :: String -> (Argument -> Argument -> Argument -> b) -> ReadP (Either String b)
instructionWith3Arguments mne t = do
  _ <- string mne
  skipSpaces
  (arg1, arg2, arg3) <- threeArguments
  return $ Right $ t arg1 arg2 arg3

oneArgument :: ReadP Argument
oneArgument = do
  arg <- address <|> literal
  return (arg)

twoArguments :: ReadP (Argument, Argument)
twoArguments = do
  arg1 <- oneArgument
  skipSpaces
  arg2 <- oneArgument
  return (arg1, arg2)

threeArguments :: ReadP (Argument, Argument, Argument)
threeArguments = do
  (arg1, arg2) <- twoArguments
  skipSpaces
  arg3 <- oneArgument
  return (arg1, arg2, arg3)

address :: ReadP Argument
address = do
  addr <- between (string "[") (string "]") digits
  return (Address (read addr))

literal :: ReadP Argument
literal = do
  lit <- digits
  return (Literal (read lit))

digits :: ReadP String
digits = munch1 (\chr -> chr >= '0' && chr <= '9')
