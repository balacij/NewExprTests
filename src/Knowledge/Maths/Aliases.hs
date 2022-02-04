module Knowledge.Maths.Aliases where

import Knowledge.Concepts.Definition (Definition)
import Knowledge.Maths.QuantityDict (QuantityDict)
import KnowledgeBase.TypedUIDRef (TypedUIDRef)

type QDefinition defT t = Definition (QuantityDict t) (defT t)

type UFDefinition defT a t = Definition (QuantityDict (a -> t)) (defT a -> defT t)

type BFDefinition defT a b t = Definition (QuantityDict (a -> b -> t)) (defT a -> defT b -> defT t)

type TFDefinition defT a b c t = Definition (QuantityDict (a -> b -> c -> t)) (defT a -> defT b -> defT c -> defT t)

type NFCall defT t = TypedUIDRef (QDefinition defT t) -> defT t

type UFCall defT a t = TypedUIDRef (UFDefinition defT a t) -> defT a -> defT t

type BFCall defT a b t = TypedUIDRef (BFDefinition defT a b t) -> defT a -> defT b -> defT t

type TFCall defT a b c t = TypedUIDRef (TFDefinition defT a b c t) -> defT a -> defT b -> defT c -> defT t
