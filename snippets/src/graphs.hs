{-# LANGUAGE LambdaCase #-}

module Graph (fromVector
             ,Graph
             ,Tree
             ,Forest
             ,dfePreorder
             ,dfePreorder'
             ,dfePostorder
             ,dfePostorder'
             ) where

import Data.List
import Data.Monoid
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Array.Unboxed as UA
import qualified Data.Array.ST as MUA
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Mutable as M
import qualified Data.Vector.Unboxed.Mutable as MU
import qualified Data.IntSet as S
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.ST

main = getLine

type Graph = Int -> [Int]
data Tree a = Node a (Forest a)
instance Show a => Show (Tree a) where
  show (Node a []) = show a
  show (Node a ts) = show a ++ "->(" ++ unwords (map show ts) ++ ")"
type Forest a = [Tree a]

-- CONSTRUCTING GRAPHS REPRESENTATIONS FROM LISTS OF EDGES

-- |creating an adjacency list from a list of edges indexed from [1..n]
adjacencyList :: Int -> [(Int, Int)] -> V.Vector [Int]
adjacencyList vertices edges = V.generate (vertices + 1) neighbors where
  neighbors v = map fst $ filter ((==v) . snd) edges'
  edges' = edges ++ reverse edges

-- | graph from the adjacency list
fromAdjacencyList adjacencyList = (adjacencyList V.!)

-- |creating an adjacency matrix from a list of edges indexed from [1..n]
undirectedAdjacencyMatrix :: Int -> [(Int,Int)] -> UA.UArray (Int, Int) Bool
undirectedAdjacencyMatrix n edges = MUA.runSTUArray $ do
  a <- MUA.newArray ((1,1), (n,n)) False
  forM_ edges $ \i -> do
    MUA.writeArray a i True
    MUA.writeArray a (swap i) True
  return a

-- |creating an adjacency matrix for a directed graph
directedAdjacencyMatrix :: Int -> [(Int,Int)] -> UA.UArray (Int, Int) Bool
directedAdjacencyMatrix n edges = MUA.runSTUArray $ do
  a <- MUA.newArray ((1,1), (n,n)) False
  forM_ edges $ \i -> MUA.writeArray a i True
  return a

-- | Creating an adjacency matrix for a directed graph.
-- | Here double can be substituted for any "unboxed array" type: all of the
-- | sized integral types and floats and doubles.
weightedAdjacencyMatrix :: Int -> [(Int,Int,Double)] -> UA.UArray (Int, Int) Double
weightedAdjacencyMatrix n edges = MUA.runSTUArray $ do
  a <- MUA.newArray_ ((1,1), (n,n))
  forM_ edges $ \(i,j,x) -> MUA.writeArray a (i,j) x
  return a

-- | Also importable from Data.Tuple
swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

fromVector :: V.Vector [Int] -> Graph
fromVector = (V.!)

-- GRAPH TRAVERSALS

-- Recursive style (passing visited set by return)
dfePreorder :: Graph -> Int -> [Int]
dfePreorder g = reverse . fst . go ([], S.empty) where
  go (xs, v) x | S.member x v = (xs, v)
               | otherwise = foldl' go (x:xs, S.insert x v) (g x)

-- StateT monad style
dfePreorder' :: Graph -> Int -> [Int]
dfePreorder' g r = evalState (go r) S.empty where
  go x = gets (S.member x) >>= \case
    True -> return []
    False -> modify (S.insert x) >> (x:) . concat <$> mapM go (g x)

-- Stack (non-recursive) style
dfePreorder''' :: Graph -> Int -> [Int]
dfePreorder''' g r = go S.empty [r] where
  go v [] = []
  go v (x:xs) | S.member x v = go v xs
              | otherwise = x : go (S.insert x v) (g x ++ xs)


-- Recursive style (passing visited set by return)
dfePostorder :: Graph -> Int -> [Int]
dfePostorder g = reverse . fst . go ([], S.empty) where
  go (xs, v) x | S.member x v = (xs, v)
               | otherwise = let (xs',v') = foldl' go (xs, S.insert x v) (g x)
                              in (x:xs',v')

-- StateT style, using list append
dfePostorder' :: Graph -> Int -> [Int]
dfePostorder' g r = evalState (go r) S.empty where
  go x = gets (S.member x) >>= \case
    True -> return [] 
    False -> modify (S.insert x) >> (++[x]) . concat <$> mapM go (g x)

-- Using difference lists, aka (Endo [Int])
dfePostorder'' :: Graph -> Int -> [Int]
dfePostorder'' g r = flip appEndo [] $ evalState (go r) S.empty where
  go x = gets (S.member x) >>= \case
    True -> return $ Endo id
    False -> modify (S.insert x) >> mapM go (g x)
               >>= \ks -> return $ mconcat ks `mappend` Endo (x:)

dfsTree :: Graph -> Int -> Tree Int
dfsTree g r = fromJust $ evalState (go r) S.empty where
  go :: Int -> State S.IntSet (Maybe (Tree Int))
  go x = gets (S.member x) >>= \case
    True -> return Nothing
    False -> modify (S.insert x) >> Just . Node x . catMaybes <$> mapM go (g x)

dfsTree' :: Graph -> Int -> Tree Int
dfsTree' g r = evalState (go r) S.empty where
  go :: Int -> State S.IntSet (Tree Int)
  go x = modify (S.insert x)
           >> gets (\v -> S.toList $ S.fromList (g x) S.\\ v)
           >>= mapM go >>= return . Node x

