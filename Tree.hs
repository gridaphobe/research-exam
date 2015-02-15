{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tree where

import Prelude hiding (abs)

import GHC.Generics

import Test.Target
import Test.Target.Targetable
import Test.SmallCheck        as SC
import Test.SmallCheck.Series
import Test.QuickCheck        as QC
import Test.QuickCheck.Gen

data Tree a
  = Leaf
  | Node a (Tree a) (Tree a)
  deriving (Show, Generic)

{-@ type BST a = {v:Tree <{\root v -> v < root}, {\root v -> v > root}> a | balanced v} @-}

{-@ predicate Balanced T = (-1) <= bFac T && bFac T <= 1 @-}

{-@ insert :: Int -> BST Int -> BST Int @-}
insert :: Int -> Tree Int -> Tree Int
insert x t = case t of
  Leaf -> singleton x
  Node y l r -> case compare x y of
    LT -> bal y (insert x l) r
    GT -> bal y l (insert x r)
    EQ -> t

{-@ insert' :: Int -> BST Int -> BST Int @-}
insert' :: Int -> Tree Int -> Tree Int
insert' x t = case t of
  Leaf -> singleton x
  Node y l r -> case compare x y of
    LT -> Node y (insert x l) r
    GT -> bal y l (insert x r)
    EQ -> t

prop_insert_bst_qc  x t = isBST t && size t > 1 QC.==> collect (size t) $ isBST (insert x t)
prop_insert_bst_sc  x t = isBST t SC.==> isBST (insert x t)


{-@ data Tree a <pl :: Int -> Int -> Prop, pr :: Int -> Int -> Prop> =
        Leaf
      | Node { x :: a
             , l :: Tree <pl,pr> (a<pl x>)
             , r :: Tree <pl,pr> (a<pr x>)
             }
  @-}

{- type TreeL X = Tree <{\v -> v < X}> @-}
{- type TreeR X = Tree <{\v -> v > X}> @-}


{- data Tree <p :: Int -> Prop> =
        Leaf
      | Node { x :: Int<p>
             , l :: TreeL x
             , r :: TreeR x
             }
  @-}

{- type TreeL X = Tree <{\v -> v < X}> @-}
{- type TreeR X = Tree <{\v -> v > X}> @-}




instance Targetable a => Targetable (Tree a)

instance Serial IO a => Serial IO (Tree a) where
  series = cons0 Leaf \/ cons3 Node

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = oneof [ leaf, node ]
    where
    leaf = return Leaf
    node = do x <- arbitrary
              l <- arbitrary
              r <- arbitrary
              return (Node x l r)


isBST t = case t of
  Leaf -> True
  Node y l r
    | not (abs (bFac t) <= 1) -> False
    | not (allT (< y) l)      -> False
    | not (allT (> y) r)      -> False
    | not (isBST l)           -> False
    | not (isBST r)           -> False
    | otherwise               -> True


bal v l r
  | leftBig  && bl > 0 = balLL v l r
  | leftBig  && bl < 0 = balLR v l r
  | leftBig            = balL0 v l r
  | rightBig && br > 0 = balRL v l r
  | rightBig && br < 0 = balRR v l r
  | rightBig           = balR0 v l r
  | otherwise          = tree  v l r
  where
    leftBig            = siblDiff     > 1
    rightBig           = siblDiff + 1 < 0
    siblDiff           = htDiff l r
    bl                 = bFac l
    br                 = bFac r

tree = Node

singleton x = Node x Leaf Leaf

{-@ measure balanced @-}
balanced :: Tree a -> Bool
balanced Leaf = True
balanced (Node _ l r) = abs (getHeight l - getHeight r) <= 1 && balanced l && balanced r

{-@ measure getHeight @-}
getHeight :: Tree a -> Int
getHeight Leaf         = 0
getHeight (Node _ l r) = 1 + if hl > hr then hl else hr
  where
    hl        = getHeight l
    hr        = getHeight r

{-@ measure bFac @-}
bFac :: Tree a -> Int
bFac Leaf         = 0
bFac (Node _ l r) = getHeight l - getHeight r 

{-@ inline abs @-}
abs :: Int -> Int
abs x = if x < 0 then 0 - x else x

htDiff l r = getHeight l - getHeight r

size Leaf         = 0
size (Node _ l r) = 1 + size l + size r

allT p Leaf = True
allT p (Node x l r) = p x && allT p l && allT p r

balL0 v (Node lv ll lr) r
  = tree lv ll (tree v lr r)

balLL v (Node lv ll lr) r
  = tree lv ll (tree v lr r)

balLR v (Node lv ll (Node lrv lrl lrr)) r
  = tree lrv (tree lv ll lrl) (tree v lrr r)

balR0 v l (Node rv rl rr)
  = tree rv (tree v l rl) rr

balRR v l (Node rv rl rr)
  = tree rv (tree v l rl) rr

balRL v l (Node rv (Node rlv rll rlr) rr)
  = tree rlv (tree v l rll) (tree rv rlr rr) 
