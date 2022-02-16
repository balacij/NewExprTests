{-# LANGUAGE GADTs #-}

module Knowledge.Maths.Literal (Literal, LiteralC (..)) where

import qualified Data.Bool as S
import Knowledge.Maths.Space (HasSpace (space))
import qualified Knowledge.Maths.Space as S

data Literal where
  Int :: Int -> Literal
  Dbl :: Double -> Literal -- TODO: Rename to "Real"
  Str :: String -> Literal
  Bool :: Bool -> Literal

class LiteralC r where
  int :: Int -> r
  dbl :: Double -> r
  str :: String -> r
  bool :: Bool -> r

instance LiteralC Literal where
  int = Int
  dbl = Dbl
  str = Str
  bool = Bool

instance HasSpace Literal where
  space (Int n) = S.Integer
  space (Dbl y) = S.Real
  space (Str s) = S.String
  space (Bool b) = S.Boolean
