{-# LANGUAGE GADTs #-}

module Knowledge.Maths.Literal (Literal, LiteralC(..)) where

data Literal t where
  Int :: Int -> Literal Int
  Dbl :: Double -> Literal Double
  Str :: String -> Literal String
  Bool :: Bool -> Literal Bool

class LiteralC r where
  int :: Int -> r Int
  dbl :: Double -> r Double
  str :: String -> r String
  bool :: Bool -> r Bool

instance LiteralC Literal where
  int = Int
  dbl = Dbl
  str = Str
  bool = Bool
