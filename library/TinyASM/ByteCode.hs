module TinyASM.ByteCode (
  ByteCode(..),
  Compilable(..)
)
where

import Text.Printf (printf)

data ByteCode = BC1 Int
              | BC2 Int Int
              | BC3 Int Int Int
              | BC4 Int Int Int Int

instance Show ByteCode where
  show (BC1 op)       = "ByteCode " ++ (showHex op)
  show (BC2 op b)     = (show $ BC1 op) ++ " " ++ (showHex b)
  show (BC3 op b c)   = (show $ BC2 op b) ++ " " ++ (showHex c)
  show (BC4 op b c d) = (show $ BC3 op b c) ++ " " ++ (showHex d)

class Compilable a where
  toString :: a -> String
  size :: a -> Int
  compile :: a -> [Int]

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

showHex :: Int -> String
showHex = printf "0x%02x"
