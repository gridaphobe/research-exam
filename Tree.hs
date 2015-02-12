{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Tree where

import GHC.Generics

-- import Test.SmallCheck
-- import Test.SmallCheck.Series
import Test.QuickCheck
import Test.QuickCheck.Gen

data Tree
  = Leaf
  | Node Int Tree Tree
  deriving (Show, Generic)

-- instance Serial IO Tree where
--   series = cons0 Leaf \/ cons3 Node

instance Arbitrary Tree where
  arbitrary = oneof [ leaf, node ]
    where
    leaf = return Leaf
    node = do x <- arbitrary
              l <- arbitrary
              r <- arbitrary
              return (Node x l r)

insert :: Int -> Tree -> Tree
insert x t = case t of
  Leaf -> singleton x
  Node y l r -> case compare x y of
    LT -> bal y (insert x l) r
    GT -> bal y l (insert x r)
    EQ -> t

isBST t = case t of
  Leaf -> True
  Node y l r
    | not (abs (bFac t) <= 1) -> False
    | not (allT (< y) l)      -> False
    | not (allT (> y) r)      -> False
    | not (isBST l)           -> False
    | not (isBST r)           -> False
    | otherwise               -> True


allT p Leaf = True
allT p (Node x l r) = p x && allT p l && allT p r

prop_insert_bst  x t = isBST t && size t > 1 ==> collect (size t) $ isBST (insert x t)

size Leaf         = 0
size (Node _ l r) = 1 + size l + size r
















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

getHeight Leaf         = 0
getHeight (Node _ l r) = 1 + max hl hr
  where
    hl        = getHeight l
    hr        = getHeight r

bFac Leaf         = 0
bFac (Node _ l r) = getHeight l - getHeight r 

htDiff l r = getHeight l - getHeight r

{-@ balL0 :: x:a -> l:{AVLL a x | NoHeavy l} -> r:{AVLR a x | HtDiff l r 2} -> {t:AVL a | realHeight t = realHeight l + 1 } @-}
balL0 v (Node lv ll lr) r
  = tree lv ll (tree v lr r)

{-@ balLL :: x:a -> l:{AVLL a x | LeftHeavy l } -> r:{AVLR a x | HtDiff l r 2} -> {t:AVL a | EqHt t l} @-}
balLL v (Node lv ll lr) r
  = tree lv ll (tree v lr r)

{-@ balLR :: x:a -> l:{AVLL a x | RightHeavy l } -> r:{AVLR a x | HtDiff l r 2} -> {t: AVL a | EqHt t l } @-}
balLR v (Node lv ll (Node lrv lrl lrr)) r
  = tree lrv (tree lv ll lrl) (tree v lrr r)

{-@ balR0 :: x:a -> l: AVLL a x -> r: {AVLR a x | NoHeavy r && HtDiff r l 2 } -> {t: AVL a | realHeight t = realHeight r + 1} @-}
balR0 v l (Node rv rl rr)
  = tree rv (tree v l rl) rr

{-@ balRR :: x:a -> l: AVLL a x -> r:{AVLR a x | RightHeavy r && HtDiff r l 2 } -> {t: AVL a | EqHt t r } @-}
balRR v l (Node rv rl rr)
  = tree rv (tree v l rl) rr

{-@ balRL :: x:a -> l: AVLL a x -> r:{AVLR a x | LeftHeavy r && HtDiff r l 2} -> {t: AVL a | EqHt t r } @-}
balRL v l (Node rv (Node rlv rll rlr) rr)
  = tree rlv (tree v l rll) (tree rv rlr rr) 
