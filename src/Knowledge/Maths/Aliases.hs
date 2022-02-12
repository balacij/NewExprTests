module Knowledge.Maths.Aliases where

import Knowledge.Concepts.Definition (Definition)
import Knowledge.Maths.QuantityDict (QuantityDict)
import KnowledgeBase.TypedUIDRef (TypedUIDRef)

type UFuncDict a t = QuantityDict (a -> t)
type BFuncDict a b t = QuantityDict (a -> b -> t)
type TFuncDict a b c t = QuantityDict (a -> b -> c -> t)

type UFCall defT a t = TypedUIDRef (UFuncDict a t) -> defT a -> defT t
type BFCall defT a b t = TypedUIDRef (BFuncDict a b t) -> defT a -> defT b -> defT t
type TFCall defT a b c t = TypedUIDRef (TFuncDict a b c t) -> defT a -> defT b -> defT c -> defT t
