module Knowledge.Maths.QuantityDict.Definition
  ( QDefinition,
    SimpleQDef,
    mkQDefinition,
  )
where

import Knowledge.Concepts.Definition (Definition, mkDefinition)
import Knowledge.Maths.Expr (Expr)
import Knowledge.Maths.QuantityDict (QuantityDict)
import KnowledgeBase.UID (UID)

type QDefinition r typ = Definition (QuantityDict typ) (r typ)

type SimpleQDef typ = QDefinition Expr typ

mkQDefinition :: UID -> QuantityDict typ -> r typ -> String -> String -> QDefinition r typ
mkQDefinition = mkDefinition
