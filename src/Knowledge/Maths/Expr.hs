{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeApplications #-}

module Knowledge.Maths.Expr where

import Data.Typeable (Proxy (Proxy))
import Knowledge.Maths.Aliases
  ( BFDefinition,
    QDefinition,
    TFDefinition,
    UFDefinition,
  )
import Knowledge.Maths.Literal (Literal, LiteralC (..))
import Knowledge.Maths.QuantityDict (mkQuantityDict)
import KnowledgeBase.TypedUIDRef (TypedUIDRef)

data Expr t where
  Lit :: Literal t -> Expr t
  Not :: Expr Bool -> Expr Bool
  Add :: Num t => Expr t -> Expr t -> Expr t
  Sub :: Num t => Expr t -> Expr t -> Expr t -- TODO: We might need to create our own copies of Haskell 'Num' typeclass more suited to Drasil, 'Sub' under Naturals is 'Monus' for example, and we should make this difference a bit more explicit ... somehow... im not sure yet how
  IfTE :: Expr Bool -> Expr a -> Expr a -> Expr a
  -- TODO: I would like an alternative to this all below, or else creating functions with >10 variables will be a real hassle (but, safe at least!).

  -- TODO: NamedArguments: Do we want them here? It depends on what we want really. We _can_ have them here either wrapping the arguments with a "Maybe" or making them an "unsafe" component checked at Drasil-runtime (not Drasil-artifact-runtime), or we can move them completely into CodeExpr (seems a bit more appropriate I think) with the same "Maybe"s
  --       For now, I've excluded them from this test.

  -- | Symbol
  Sy :: TypedUIDRef (QDefinition Expr t) -> Expr t

  -- ASIDE: Suggested style

  Lam :: (Expr a -> Expr b) -> Expr (a -> b)
  App :: Expr (a -> b) -> Expr a -> Expr b

  -- ASIDE: Trying something out...

  -- | Unary Function "Call"s
  UFCall :: TypedUIDRef (UFDefinition Expr a t) -> Expr a -> Expr t
  -- | Binary Function "Call"s
  BFCall :: TypedUIDRef (BFDefinition Expr a b t) -> Expr a -> Expr b -> Expr t
  -- | Tertiary Functions "Call"s
  TFCall :: TypedUIDRef (TFDefinition Expr a b c t) -> Expr a -> Expr b -> Expr c -> Expr t
  -- TODO: It would be really nice here if we could get these to scale better.
  --       Right now, it would require us to manually build many constructors.
  --       Writing a constructor for 1-5 parameter functions would be do-able,
  --       ...                       1-10 ...                         okay
  --       ...                       1-100 ...                     make this file huge.

  -- TODO: Should we try to replace the above constructors with something similar to the type-indexed heterogeneous lists?
  --       I've given this a few attempts, but have failed thus far.

class ExprC r where
  lit :: Literal t -> r t
  not_ :: r Bool -> r Bool
  add :: Num t => r t -> r t -> r t
  sub :: Num t => r t -> r t -> r t

  ifTE :: r Bool -> r a -> r a -> r a

  sy :: TypedUIDRef (QDefinition r t) -> r t

  lam :: (r a -> r b) -> r (a -> b)
  app :: r (a -> b) -> r a -> r b

  ufCall :: TypedUIDRef (UFDefinition r a t) -> r a -> r t
  bfCall :: TypedUIDRef (BFDefinition r a b t) -> r a -> r b -> r t
  tfCall :: TypedUIDRef (TFDefinition r a b c t) -> r a -> r b -> r c -> r t

instance LiteralC Expr where
  int = lit . int
  dbl = lit . dbl
  str = lit . str
  bool = lit . bool

instance ExprC Expr where
  lit = lit
  not_ = not_
  add = add
  sub = sub
  ifTE = ifTE
  sy = sy
  lam = Lam
  app = App
  ufCall = ufCall
  bfCall = bfCall
  tfCall = tfCall
