{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NoMonomorphismRestriction #-} -- TODO: This flag is a bit of a double-sided sword. We _should not_ leave Chunks with polymorphic type parameters because references should to it should all be able to assume the same monomorphic type. Surprisingly, it's good to have this flag disabled! But I like it, and I'm aware of it, so I'll keep it enabled...

module Lib where

import Data.Typeable (Proxy (Proxy))
import Knowledge.Concepts.Definition (Definition, mkDefinition)
import Knowledge.Maths.Expr (Expr, ExprC (..))
import Knowledge.Maths.Literal (LiteralC (..))
import Knowledge.Maths.QuantityDict (QuantityDict, mkQDefinition, mkQuantityDict)
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

varDef1 :: Definition (QuantityDict Int) (Expr Int)
varDef1 = mkDefinition (mkUid "varDef1") var1 (int 1) "e1" "f1"

varDef2 :: Definition (QuantityDict Bool) (Expr Bool)
varDef2 = mkDefinition (mkUid "varDef2") var2 (not_ $ bool False) "e2" "f2"

funcDef1 :: Definition (QuantityDict (Int -> Int)) (Expr Int -> Expr Int)
funcDef1 = mkDefinition (mkUid "funcDef1") func1 (\x -> add x (int 1)) "g1" "h1"

funcDef2 :: Definition (QuantityDict (Bool -> Int -> Int -> Int)) (Expr Bool -> Expr Int -> Expr Int -> Expr Int)
funcDef2 = mkDefinition (mkUid "funcDef2") func2 ifTE "g2" "h2"

callFunc1 :: Expr Int
callFunc1 = ufCall (mkRef func1) (int 1)

callFunc3 :: Expr Int
callFunc3 = tfCall (mkRef func2) (bool True) (int 2) (int 3)

funcDef3 :: Definition (QuantityDict (Int -> Int)) (Expr (Int -> Int))
funcDef3 = mkDefinition (mkUid "funcDef3") func1 (lam (\x -> add x (int 1))) "g3" "h3"

{-

`mkDefinition` is generally 'more open' to errors (which is okay, because we might
want to use the generic concept of a Definition else where too). For example, the below
is the _inferred_ type signature when using `mkDefinition`, note the problematic `b`s:

funcDef4 :: ExprC r =>
  Definition
    (QuantityDict (Bool -> Int -> Int -> Int))
    (r (Bool -> b -> b -> b))
funcDef4 = mkDefinition (mkUid "funcDef4") func2 (lam (\b -> lam (lam . ifTE b))) "g4" "gh"

Luckily, we can just create a helper function for things that we commonly define with a type param
to straighten up the type nicely: e.g., `mkQDefinition` (QuantityDict Definition)

-}

funcDef4 :: Definition (QuantityDict (Bool -> Int -> Int -> Int)) (Expr (Bool -> Int -> Int -> Int))
funcDef4 = mkQDefinition (mkUid "funcDef4") func2 (lam (\b -> lam (lam . ifTE b))) "g4" "gh"

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
