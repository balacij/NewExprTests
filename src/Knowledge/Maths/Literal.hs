{-# LANGUAGE GADTs #-}

module Knowledge.Maths.Literal (Literal (..)) where

data Literal t where
  Int :: Int -> Literal Int
  Dbl :: Double -> Literal Double
  Str :: String -> Literal String
  Bool :: Bool -> Literal Bool
