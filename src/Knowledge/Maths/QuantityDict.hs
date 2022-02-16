module Knowledge.Maths.QuantityDict
  ( QuantityDict,
    mkQuantityDict,
  )
where

import Data.Typeable (Proxy (..))
import Knowledge.Concepts.Definition (Definition, mkDefinition)
import Knowledge.Maths.Space (HasSpace (space), Space)
import KnowledgeBase.Chunk (HasChunkRefs (chunkRefs))
import KnowledgeBase.UID (HasUID (uid), UID, mkUid)

data QuantityDict = QuantityDict
  { _uid :: UID,
    _fakeData1 :: String,
    _fakeData2 :: String,
    -- TODO: We should explore making this "Proxy" usable somehow, it would be
    --       nice if this could be replaced with an upgraded "Space"
    _typ :: Space
  }

instance HasUID QuantityDict where uid = _uid

instance HasChunkRefs QuantityDict where chunkRefs = const []

instance HasSpace QuantityDict where space = _typ

-- * Smart Constructor

mkQuantityDict :: Space -> UID -> String -> String -> QuantityDict
mkQuantityDict sp u a b = QuantityDict u a b sp
