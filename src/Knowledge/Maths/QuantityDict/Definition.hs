module Knowledge.Maths.QuantityDict.Definition
  ( QDefinition,
    mkQDefinition,
    mkFuncDefinition
  )
where

import qualified Data.List.NonEmpty as NE
import Knowledge.Maths.QuantityDict (QuantityDict)
import Knowledge.Maths.Space (HasSpace (..))
import qualified Knowledge.Maths.Space as S
import KnowledgeBase.Chunk (HasChunkRefs (..))
import KnowledgeBase.UID (HasUID (uid), UID)

-- TODO: Get rid of "r" type arg
data QDefinition r = QDefinition
  { _uid :: UID,
    _qd :: QuantityDict, -- TODO: TypedRefs to these
    _expr :: r,
    _ins :: [QuantityDict],
    -- _namedIns :: [QuantityDict] -- TODO: Named inputs
    _explanation :: String
  }

instance HasUID (QDefinition r) where uid = _uid

instance HasChunkRefs (QDefinition r) where chunkRefs = const [] -- TODO:

mkQDefinition :: HasSpace r => UID -> QuantityDict -> r -> String -> QDefinition r
mkQDefinition u qd d
  -- TODO: space of the qd should not be a function type
  | space qd == space d = QDefinition u qd d []
  | otherwise = error $ "Mismatched spaces when trying to create: " ++ show u

mkFuncDefinition :: HasSpace r => UID -> QuantityDict -> [QuantityDict] -> r -> String -> QDefinition r
mkFuncDefinition u qd inQds d
  | outSp /= space d = error $ "Provided expression for `" ++ show u ++ "` does not match the expected resultant type."
  | all (== True) c && length qdIns == length inQds = QDefinition u qd d inQds
  | otherwise = error $ "Bad types for function creation: " ++ show u
  where
    S.Function qdIns outSp = space qd
    c = zipWith (\a b -> a == space b) (NE.toList qdIns) inQds
