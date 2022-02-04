module Knowledge.Concepts.Definition (Definition, mkDefinition) where

import KnowledgeBase.Chunk (HasChunkRefs (chunkRefs))
import KnowledgeBase.UID (HasUID (uid), UID)

data Definition trgT srcT = Definition
  { _uid :: UID,
    _trg :: trgT,
    _src :: srcT,
    _explanation :: String,
    _other :: String
  }

instance HasUID (Definition trgT srcT) where uid = _uid

instance HasChunkRefs (Definition trgT srcT) where chunkRefs = const []

mkDefinition :: UID -> trgT -> srcT -> String -> String -> Definition trgT srcT
mkDefinition = Definition
