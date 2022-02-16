{-# LANGUAGE TypeApplications #-}
{- TODO: NoMonomorphismRestriction is a bit of a double-edged sword. We _should
not_ leave Chunks with polymorphic type parameters because references should to
it  should all be able to assume the same monomorphic type. Surprisingly, it's
good to have this flag disabled! But I like it, and I'm aware of it, so I'll keep
it enabled... -}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib where

import qualified Data.List.NonEmpty as NE
import Data.Typeable (Proxy (Proxy))
import Knowledge.Concepts.Definition (Definition, mkDefinition)
import Knowledge.Maths.Expr (Expr, ExprC (..))
import Knowledge.Maths.Literal (LiteralC (..))
import Knowledge.Maths.QuantityDict (QuantityDict, mkQuantityDict)
import Knowledge.Maths.QuantityDict.Definition (QDefinition, mkQDefinition)
import qualified Knowledge.Maths.Space as S
import KnowledgeBase.ChunkDB (ChunkDB, empty, insert')
import KnowledgeBase.TypedUIDRef (mkRef)
import KnowledgeBase.UID (mkUid)

var1 :: QuantityDict
var1 = mkQuantityDict S.Integer (mkUid "var1") "a1" "b1"

var2 :: QuantityDict
var2 = mkQuantityDict S.Boolean (mkUid "var2") "a2" "b2"

func1 :: QuantityDict
func1 = mkQuantityDict (S.Function (S.Integer NE.:| []) S.Integer) (mkUid "func1") "c1" "d1"

func2 :: QuantityDict
func2 = mkQuantityDict (S.Function (NE.fromList [S.Boolean, S.Integer, S.Integer]) S.Integer) (mkUid "func2") "c2" "d2"

varDef1 :: QDefinition Expr
varDef1 = mkQDefinition (mkUid "varDef1") var1 (int 1) "e1"

varDef2 :: QDefinition Expr
varDef2 = mkQDefinition (mkUid "varDef2") var2 (not_ $ bool False) "e2"

cdb :: ChunkDB
cdb =
  insert' varDef2 $
    insert' varDef1 $
      insert' func2 $
        insert' func1 $
          insert' var2 $
            insert' var1 empty

test1 :: IO ()
test1 = do
  putStrLn "Hello world!"
