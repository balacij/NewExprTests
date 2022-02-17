module KnowledgeBase.TypedUIDRef
  ( TypedUIDRef,
    mkRef,
    typedFind,
    typedFindOrErr,
  )
where

import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import KnowledgeBase.Chunk (HasChunkRefs, IsChunk)
import KnowledgeBase.ChunkDB (ChunkDB, find)
import KnowledgeBase.UID (HasUID (uid), UID)

newtype TypedUIDRef typ = TypedUIDRef UID

mkRef :: IsChunk t => t -> TypedUIDRef t
mkRef = TypedUIDRef . uid

typedFind :: IsChunk t => TypedUIDRef t -> ChunkDB -> Maybe t
typedFind (TypedUIDRef u) = find u

typedFindOrErr :: (Typeable t, HasUID t, HasChunkRefs t) => TypedUIDRef t -> ChunkDB -> t
typedFindOrErr tu cdb = fromMaybe (error "Typed UID dereference failed.") (typedFind tu cdb)
