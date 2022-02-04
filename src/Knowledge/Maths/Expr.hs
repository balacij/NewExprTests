{-# LANGUAGE GADTs #-}

module Knowledge.Maths.Expr where

import Knowledge.Maths.Aliases
  ( BFDefinition,
    QDefinition,
    TFDefinition,
    UFDefinition,
  )
import Knowledge.Maths.Literal (Literal)
import Knowledge.Maths.QuantityDict (mkQuantityDict)
import KnowledgeBase.TypedUIDRef (TypedUIDRef)

data Expr t where
  Lit :: Literal t -> Expr t
  Add :: Num t => Expr t -> Expr t -> Expr t
  Sub :: Num t => Expr t -> Expr t -> Expr t -- TODO: We might need to create our own copies of Haskell 'Num' typeclass more suited to Drasil, 'Sub' under Naturals is 'Monus' for example, and we should make this difference a bit more explicit ... somehow... im not sure yet how

  -- TODO: I would like an alternative to this all below, or else creating functions with >10 variables will be a real hassle (but, safe at least!).
  -- | Symbol
  Sy :: TypedUIDRef (QDefinition Expr t) -> Expr t
  -- | Unary Function "Call"s
  UFCall :: TypedUIDRef (UFDefinition Expr a t) -> Expr a -> Expr t
  -- | Binary Function "Call"s
  BFCall :: TypedUIDRef (BFDefinition Expr a b t) -> Expr a -> Expr b -> Expr t
  -- | Tertiary Functions "Call"s
  TFCall :: TypedUIDRef (TFDefinition Expr a b c t) -> Expr a -> Expr b -> Expr c -> Expr t
