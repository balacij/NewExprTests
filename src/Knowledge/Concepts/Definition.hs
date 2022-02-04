module Knowledge.Concepts.Definition (Definition, mkDefinition) where

import KnowledgeBase.UID (UID)

data Definition trgT srcT = Definition
  { _uid :: UID,
    _trg :: trgT,
    _src :: srcT,
    _explanation :: String,
    _other :: String
  }

mkDefinition :: UID -> trgT -> srcT -> String -> String -> Definition trgT srcT
mkDefinition = Definition
