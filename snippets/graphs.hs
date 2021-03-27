type Graph = Int -> [Int]

-- Represents graph as vector of [Int]
reverseGraph n g = runST $ do
  v <- M.replicate n []
  forM_ [1..n] $ do \i -> forM_ (g i) $ M.modify v (i:)
  V.unsafeFreeze v

-- These algorthms traverses through every path starting from the root node, failing if it finds a directed cycle. It's horribly inefficient since it has to go down the same paths many times.
dfsFoldWithPathsM :: MonadPlus m => (Int -> a -> m a) -> m a -> Graph -> Int -> m a
dfsFoldWithPathsM f z g root = z >>= (\a -> go [] a root) where
  go s a x
    | x `elem` s = mzero
    | otherwise = f x a >>= (\a' -> foldM (go (x:s)) a' (g x))

dfsFoldWithPathsM' :: MonadPlus m => ([Int] -> a -> m a) -> m a -> Graph -> Int -> m a
dfsFoldWithPathsM' f z g root = z >>= (\a -> go [] a root) where
  go s a x
    | x `elem` s = mzero
    | otherwise = f (x:s) a >>= (\a' -> foldM (go (x:s)) a' (g x))

-- from doisinkidney.com
-- A breadth first traversal of a graph that uses continuation passing style.
-- Here CPS is used to allow f to pass onto the continuation without doing anything when it reaches a visited node.
-- The parameter passed around in the continuation is a list of lists, which is
-- used as a queue as in the imperative breadth first traversal.
-- In the continuation (b) `foldl (foldr f) b qs` is building up a continuation
-- of the same type, which they then call on parameters [] and s, representing 
-- queue and the visited set
bfs :: Graph -> Int -> [Int]
bfs g ts = f ts b [] S.empty
  where
    -- Continuation, takes fw and applies it to bw and s
    f :: Int -> ([[Int]] -> S.IntSet -> [Int]) -> [[Int]] -> S.IntSet -> [Int]
    f x fw bw s
      | S.member x s = fw bw s
      | otherwise      = x : fw (g x : bw) (S.insert x s)
    -- The result function of the continuation
    b :: [[Int]] -> S.IntSet -> [Int]
    b [] _ = []
    b qs s = foldl (foldr f) b qs [] s

-- Depth first search (does nothing)
-- Stack style
dfs :: Graph -> Int -> ()
dfs g root = go S.empty [root] where
  go _ [] = ()
  go visited (x:xs)
    | S.member x visited = go visited xs
    | otherwise = go(S.insert x visited) (g x ++ xs)

-- It's like a forM on graph nodes. This is a stack style, traverses in preorder.
dft :: Monad m => (Int -> m a) -> m a -> Graph -> Int -> m a
dft f z g root = go S.empty [root] where
  go _ [] = z
  go visited (x:xs)
    | S.member x visited = go visited xs
    | otherwise = go(S.insert x visited) (g x ++ xs) >> f x
 
-- Depth first postorder monadic fold on directed graph
dfpostFoldM :: Monad m => (Int -> a -> m a) -> m a -> Graph -> Int -> m a
dfpostFoldM f z g root = snd $ go root (S.empty, z) where
  go x (visited, a)
    | S.member x visited = (visited, a)
    | otherwise = let (v', a') = foldr go (S.insert x visited, a) (g x) in
                      (v', a' >>= f x)

-- Depth first preorder monadic fold on directed graph from source node
dfpreFoldM :: Monad m => (Int -> a -> m a) -> m a -> Graph -> Int -> m a
dfpreFoldM f z g root = snd $ go (S.empty, z) root where
  go (visited, a) x
    | S.member x visited = (visited, a)
    | otherwise = foldl go (S.insert x visited, a >>= f x) (g x)

-- Depth first reverse postorder (topological sort) on graph from source
dfrp g root = snd $ go root (S.empty, []) where
  go x (visited, a)
    | S.member x visited = (visited, a)
    | otherwise = let (v', a') = foldr go (S.insert x visited, a) (g x) in
                      (v', x:a')

dfrpFoldM f z g root = foldM f z $ dfrp g root

--
dfs2 g r = go b r [] S.empty where
  go k x q s
    | S.member x s = k q s
    | otherwise = x : k (g x ++ q) (S.insert x s)
  b [] _ = []
  b q s = foldl' go b q [] s

--
dfs3 g r = go b r [] S.empty where
  go k x q s
    | S.member x s = k q s
    | otherwise = x : k (reverse (g x) ++ q) (S.insert x s)
  b [] _ = []
  b q s = foldl' go b q [] s

bfs' g root = go root b [] S.empty where
  go x k = \q s ->
    if S.member x s then k q s
    else x : k (reverse (g x) ++ q) (S.insert x s)
  b :: [Int] -> S.IntSet -> [Int]
  b [] _ = []
  b q s = foldr go b q [] s -- This appears to be a fold of continuations
