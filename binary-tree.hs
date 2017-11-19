data BinaryTree a =
    Leaf
  | Node (BinaryTree a) a (BinaryTree a)
  deriving(Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
    | b == a = Node left a right
    | b < a  = Node (insert' b left) a right
    | b > a  = Node left a (insert' b right)

findNum :: Int -> BinaryTree Int -> Bool
findNum x Leaf = False
findNum x (Node left a right)
    | x < a  = findNum x left
    | x > a  = findNum x right
    | x == a = True

insertList :: [Int] -> BinaryTree Int -> BinaryTree Int
insertList [] tree     = tree
insertList [x] tree    = insert' x tree
insertList (x:xs) tree = insertList xs (insert' x tree)

t1 = insertList [1,3..1000000] Leaf
