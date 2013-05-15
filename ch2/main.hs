module Ch2 where

-- ex. 2.1
suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes list@(_:xs) = list : suffixes xs 


data Tree a = Node (Tree a) a (Tree a) 
            | Empty
    deriving (Eq, Read, Show)

type TreeLookup a = a -> Tree a -> Bool
type TreeInsert a = a -> Tree a -> Tree a
-- O(2d) comparisons
member :: Ord a => TreeLookup a
member _ Empty = False
member x (Node l v r)
	| x < v     = member x l
	| x > v     = member x r
	| otherwise = True

-- ex. 2.2
-- O(d+1) comparisons
member2 :: Ord a => TreeLookup a
member2 e root = mem e Nothing root
  where
    mem x m (Node l v r)
        | x <= v             = mem x (Just v) l
        | otherwise          = mem x m r
    mem _ Nothing  Empty     = False
    mem x (Just m) Empty     = x == m

check :: Maybe a -> a -> a
check (Just v) _ = v
check Nothing v  = v

-- ex. 2.3
insert :: Ord a => TreeInsert a
insert x t = insert' x t `check` t
  where
    insert' n (Node l v r)
        | n < v     = insert' n l >>= \l' -> Just $ Node l' v r
        | n > v     = insert' n r >>= \r' -> Just $ Node l v r'
        | otherwise = Nothing
    insert' n Empty = Just $ Node Empty n Empty

-- ex. 2.4
-- Can't catch exceptions within the pure code. So use Maybe monad.
insert2 :: Ord a => TreeInsert a
insert2 x t = insert' x Nothing t `check` t
  where
    empty n = Just $ Node Empty n Empty
    insert' :: Ord a => a -> Maybe a -> Tree a -> Maybe (Tree a)
    insert' n m (Node l v r)
        | n <= v            = insert' n (Just v) l >>= \l' -> Just $ Node l' v r
        | otherwise         = insert' n m r >>= \r' -> Just $ Node l v r'
    insert' n Nothing Empty = empty n
    insert' n (Just m) Empty
        | n == m            = Nothing
        | otherwise         = empty n

-- ex. 2.5.a
complete :: (Num b, Ord b) => a -> b -> Tree a
complete x d
    | d <= 0 = Empty
	| otherwise = Node t x t
	  where
	    t = complete x (d - 1)
