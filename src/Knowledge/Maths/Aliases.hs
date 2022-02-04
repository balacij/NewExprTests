module Knowledge.Maths.Aliases where

import Knowledge.Concepts.Definition (Definition)
import Knowledge.Maths.QuantityDict (QuantityDict)

type QDefinition  defT       t = Definition (QuantityDict t)                               (defT t)
type UFDefinition defT a     t = Definition (QuantityDict t)                     (defT a -> defT t)
type BFDefinition defT a b   t = Definition (QuantityDict t)           (defT a -> defT b -> defT t)
type TFDefinition defT a b c t = Definition (QuantityDict t) (defT a -> defT b -> defT c -> defT t)
