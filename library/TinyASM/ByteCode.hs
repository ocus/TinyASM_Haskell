module TinyASM.ByteCode (
  ByteCode(..),
  Compilable(..)
)
where

import Text.Printf (printf)

data ByteCode = BC1 Integer
              | BC2 Integer Integer
              | BC3 Integer Integer Integer
              | BC4 Integer Integer Integer Integer

instance Show ByteCode where
  show (BC1 op)       = "ByteCode " ++ (showHex op)
  show (BC2 op b)     = (show $ BC1 op) ++ " " ++ (show b)
  show (BC3 op b c)   = (show $ BC2 op b) ++ " " ++ (show c)
  show (BC4 op b c d) = (show $ BC3 op b c) ++ " " ++ (show d)

class Compilable a where
  toString :: a -> String
  size :: a -> Integer
  compile :: a -> [Integer]

instance Compilable ByteCode where
  toString (BC1 op)       = showHex op
  toString (BC2 op b)     = (toString $ BC1 op) ++ " " ++ (showHex b)
  toString (BC3 op b c)   = (toString $ BC2 op b) ++ " " ++ (showHex c)
  toString (BC4 op b c d) = (toString $ BC3 op b c) ++ " " ++ (showHex d)

  size (BC1 _)       = 1
  size (BC2 _ _)     = 2
  size (BC3 _ _ _)   = 3
  size (BC4 _ _ _ _) = 4

  compile (BC1 op)       = [op]
  compile (BC2 op b)     = [op, b]
  compile (BC3 op b c)   = [op, b, c]
  compile (BC4 op b c d) = [op, b, c, d]

showHex :: Integer -> String
showHex = printf "0x%02x"
