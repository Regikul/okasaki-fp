module Tree where

data Tree a = Node (Tree a) a (Tree a) 
            | Empty
    deriving (Eq, Read, Show)

type TreeLookup a = a -> Tree a -> Bool
type TreeInsert a = a -> Tree a -> Tree a

symmetric :: TreeInsert a
symmetric v t = Node t v t

setR :: Tree a -> Tree a -> Tree a
setR r (Node l v _) = Node l v r
setR _ Empty        = Empty

setL :: Tree a -> Tree a -> Tree a
setL l (Node _ v r) = Node l v r
setL _ Empty        = Empty

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
-- Can't catch exceptions within the pure code. So use Maybe monad.
insert :: Ord a => TreeInsert a
insert x t = insert' x t `check` t
  where
    insert' n (Node l v r)
        | n < v     = insert' n l >>= \l' -> Just $ Node l' v r
        | n > v     = insert' n r >>= \r' -> Just $ Node l v r'
        | otherwise = fail ""
    insert' n Empty = return $ Node Empty n Empty

-- ex. 2.4
-- Can't catch exceptions within the pure code. So use Maybe monad.
insert2 :: Ord a => TreeInsert a
insert2 x t = insert' x Nothing t `check` t
  where
    single n = Just $ symmetric n Empty
    insert' :: Ord a => a -> Maybe a -> Tree a -> Maybe (Tree a)
    insert' n m (Node l v r)
        | n <= v            = insert' n (Just v) l >>= \l' -> 
                              return $ Node l' v r     
        | otherwise         = insert' n m r >>= \r' -> 
                              return $ Node l v r'
    insert' n Nothing Empty = single n
    insert' n (Just m) Empty
        | n == m            = fail ""
        | otherwise         = single n

-- ex. 2.5.a
complete :: (Num b, Ord b) => a -> b -> Tree a
complete x d
    | d <= 0    = Empty
	| otherwise = symmetric x $ complete x (d - 1)

-- ex. 2.5.b
create :: Integral a => a -> b -> Tree b
create m v
    | m <= 0    = Empty
    | odd  m    = symmetric v $ create (m `div` 2) v
    | otherwise = Node l v r
      where
        l = create ((m - 1) `div` 2) v
        r = Node Empty v l

-- ex 2.6
newtype Value k a = Value (k, a)
newtype FiniteMap k a = FiniteMap (Tree (Value k a))

instance Eq k => Eq (Value k a) where
    (Value (l, _)) == (Value (r, _)) = l == r

instance Ord k => Ord (Value k a) where
    (Value (l, _)) `compare` (Value (r, _)) = l `compare` r

mkPair :: k -> a -> Value k a
mkPair k v = Value (k, v)

empty :: FiniteMap k a
empty = FiniteMap Empty

bind :: Ord k => k -> a -> FiniteMap k a -> FiniteMap k a
bind k v (FiniteMap t) = FiniteMap $ insert2 (mkPair k v) t

lookup :: Ord k => k -> FiniteMap k a -> Maybe a
lookup k (FiniteMap t) = lookup' k Nothing t
  where
    lookup' _  m Empty = m
    lookup' k1 m (Node l (Value (k2, v)) r)
        | k1 <= k2     = lookup' k1 (Just v) l
        | otherwise    = lookup' k1 m r

