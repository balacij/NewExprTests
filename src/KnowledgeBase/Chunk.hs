{-# LANGUAGE ExistentialQuantification #-}

module KnowledgeBase.Chunk
  ( Chunk,
    HasChunkRefs (..),
    mkChunk,
    unChunk,
    chunkType,
  )
where

import Data.Typeable (TypeRep, Typeable, cast, typeOf)
import KnowledgeBase.UID (HasUID (..), UID)

class HasChunkRefs a where
  chunkRefs :: a -> [UID]

data Chunk = forall a. (HasUID a, HasChunkRefs a, Typeable a) => Chunk a

instance Eq Chunk where
  l == r = uid l == uid r

instance HasUID Chunk where
  uid (Chunk t) = uid t

mkChunk :: (HasUID a, HasChunkRefs a, Typeable a) => a -> Chunk
mkChunk = Chunk

unChunk :: Typeable a => Chunk -> Maybe a
unChunk (Chunk c) = cast c

chunkType :: Chunk -> TypeRep
chunkType (Chunk c) = typeOf c
