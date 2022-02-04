{-# LANGUAGE TypeApplications #-}

module Lib where

import Data.Typeable (Proxy (Proxy))
import Knowledge.Concepts.Definition (mkDefinition)
import Knowledge.Maths.Aliases (QDefinition, TFDefinition, UFDefinition)
import Knowledge.Maths.Expr (Expr (Add, IfTE, Lit, Not, TFCall, UFCall))
import Knowledge.Maths.Literal (Literal (Bool, Int))
import Knowledge.Maths.QuantityDict (QuantityDict, mkQuantityDict)
import KnowledgeBase.ChunkDB (ChunkDB, empty, insert')
import KnowledgeBase.TypedUIDRef (mkRef)
import KnowledgeBase.UID (mkUid)

var1 :: QuantityDict Int
var1 = mkQuantityDict (Proxy @Int) (mkUid "var1") "a1" "b1"

var2 :: QuantityDict Bool
var2 = mkQuantityDict (Proxy @Bool) (mkUid "var2") "a2" "b2"

func1 :: QuantityDict (Int -> Int)
func1 = mkQuantityDict (Proxy @(Int -> Int)) (mkUid "func1") "c1" "d1"

func2 :: QuantityDict (Bool -> Int -> Int -> Int)
func2 = mkQuantityDict (Proxy @(Bool -> Int -> Int -> Int)) (mkUid "func2") "c2" "d2"

varDef1 :: QDefinition Expr Int
varDef1 = mkDefinition (mkUid "varDef1") var1 (Lit $ Int 1) "e1" "f1"

varDef2 :: QDefinition Expr Bool
varDef2 = mkDefinition (mkUid "varDef2") var2 (Not $ Lit $ Bool False) "e2" "f2"

funcDef1 :: UFDefinition Expr Int Int
funcDef1 = mkDefinition (mkUid "funcDef1") func1 (\x -> Add x (Lit $ Int 1)) "g1" "h1"

funcDef2 :: TFDefinition Expr Bool Int Int Int
funcDef2 = mkDefinition (mkUid "funcDef2") func2 IfTE "g2" "h2"

callFunc1 :: Expr Int
callFunc1 = UFCall (mkRef funcDef1) (Lit $ Int 1)

callFunc3 :: Expr Int
callFunc3 = TFCall (mkRef funcDef2) (Lit $ Bool True) (Lit $ Int 2) (Lit $ Int 3)

cdb :: ChunkDB
cdb =
  insert' funcDef1 $
    insert' varDef2 $
      insert' varDef1 $
        insert' func2 $
          insert' func1 $
            insert' var2 $
              insert' var1 empty

test1 :: IO ()
test1 = do
  putStrLn "Hello world!"
