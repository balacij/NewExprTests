module Knowledge.Maths.Space where

import qualified Data.List.NonEmpty as NE

-- | The difference kinds of spaces that may exist. This type holds
-- numerical spaces (such as the set of integers, rationals, etc.),
-- a space for booleans, a space for characters, dimensional spaces (vectors, arrays, etc.),
-- a space for Actors, discrete sets (both for numbers and strings), and a void space.
data Space
  = Integer
  | Rational
  | Real
  | Natural
  | Boolean
  | Char
  | String
  | Radians
  | Vect Space
  | Array Space
  | Actor String
  | DiscreteD [Double]
  | DiscreteS [String]
  | Function (NE.NonEmpty Primitive) Primitive
  | Void
  deriving (Eq, Show)

type Primitive = Space


class HasSpace t where
    space :: t -> Space
