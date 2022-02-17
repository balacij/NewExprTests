{-# LANGUAGE GADTs #-}

module Knowledge.Maths.Expr where

import Knowledge.Maths.Literal (Literal, LiteralC (..))
import Knowledge.Maths.QuantityDict (QuantityDict)
import Knowledge.Maths.Space (HasSpace (..), Space)
import qualified Knowledge.Maths.Space as S
import KnowledgeBase.TypedUIDRef (TypedUIDRef, mkRef)

-- A conscious choice was made to make Expr not refer to specific definitions, but to refer to only QuantityDicts.
-- This is probably a big decision. Might be notable on thesis.

data Expr where
  Lit :: Literal -> Expr
  Not :: Expr -> Expr
  Add :: [Expr] -> Expr
  Sub :: Expr -> Expr -> Expr
  IfTE :: Expr -> Expr -> Expr -> Expr
  -- | Symbol
  Sy :: TypedUIDRef QuantityDict -> Space -> Expr
  FCall :: TypedUIDRef QuantityDict -> [Expr] -> Space -> Expr

{-
  Lam :: (Expr -> Expr) -> Space -> Expr
  App :: Expr -> Expr -> Expr
-}

class ExprC r where
  lit :: Literal -> r
  not_ :: r -> r
  add :: [r] -> r
  sub :: r -> r -> r

  ifTE :: r -> r -> r -> r

  sy :: QuantityDict -> r

  fcall :: QuantityDict -> [r] -> r

{-
  lam :: (r -> r) -> Space -> r
  app :: r -> r -> r
-}

instance LiteralC Expr where
  int = lit . int
  dbl = lit . dbl
  str = lit . str
  bool = lit . bool

instance ExprC Expr where
  lit = Lit
  not_ = Not
  add = Add
  sub = Sub
  ifTE = IfTE
  sy qd = Sy (mkRef qd) (space qd)

  fcall qd es = FCall (mkRef qd) es s
    where
      S.Function _ s = space qd -- TODO: exhaustive search?

{-
  lam f s = Lam f s
  app = App
-}

instance HasSpace Expr where
  space (Lit l) = space l
  space (Not _) = S.Boolean
  space (Add exs) = space (head exs)
  space (Sub l _) = space l
  space (IfTE _ _ r) = space r
  space (Sy _ s) = s
  space (FCall _ _ s) = s

{-
  space (Lam f s) = S.Function _ _
  space (App e (Lam f _)) = space (f e)
  space (App _ _) = error "Invalid case in 'space'."
-}
