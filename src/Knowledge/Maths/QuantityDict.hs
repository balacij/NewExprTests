module Knowledge.Maths.QuantityDict
  ( QuantityDict,
    mkQuantityDict,
  )
where

import Data.Typeable (Proxy (..))
import KnowledgeBase.Chunk (HasChunkRefs (chunkRefs))
import KnowledgeBase.UID (HasUID (uid), UID, mkUid)

data QuantityDict typ = QuantityDict
  { _uid :: UID,
    _fakeData1 :: String,
    _fakeData2 :: String,
    -- TODO: We should explore making this "Proxy" usable somehow, it would be
    --       nice if this could be replaced with an upgraded "Space"
    _typ :: Proxy typ
  }

instance HasUID (QuantityDict t) where uid = _uid

instance HasChunkRefs (QuantityDict t) where chunkRefs = const []

-- * Smart Constructor

mkQuantityDict :: Proxy typ -> UID -> String -> String -> QuantityDict typ
mkQuantityDict pr u a b = QuantityDict u a b pr
