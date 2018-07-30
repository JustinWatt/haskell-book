module Chapter11 where

import Data.Foldable

data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
  Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
       1
       (Node Leaf 4 Leaf)

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
       2
       (Node Leaf 3 Leaf)

preOrder :: BinaryTree a -> [a]
preOrder Leaf = []
preOrder (Node left a right) =
  fold [[a], preOrder left, preOrder right]

inOrder :: BinaryTree a -> [a]
inOrder Leaf = []
inOrder (Node left a right) =
  fold [inOrder left, [a], inOrder right]

postOrder :: BinaryTree a -> [a]
postOrder Leaf = []
postOrder (Node left a right) =
  fold [postOrder left, postOrder right, [a]]

foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ z Leaf = z
foldTree f z (Node left a right) =
  foldTree f (foldTree f (f a z) left) right
