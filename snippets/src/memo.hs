{-# LANGUAGE LambdaCase #-}

import qualified Data.HashMap.Strict as HM
import Control.Monad.Trans.State
import Data.Hashable

-- Memoization is a strategy for solving Dynamic Programming algorithms
-- where a solution is defined with a recursive function and the intermediate
-- results are cached. We have a few approaches to this in Haskell.

-- First we write our recursive function in open recursive form.

type In = Int
type Out = Int


fb :: Int -> Int
fb 0 = 1
fb 1 = 1
fb n = fb (n - 1) + fb (n - 2)

fib :: (In -> Out) -> In -> Out
fib _ 0 = 1
fib _ 1 = 1
fib mf n = mf (n - 1) + mf (n - 2)

-- |The non-memoized solution is the result of passing the function to fix
fix f = let x = f x in x
unmemoizedFib = fix fib

-- |A basic memoized solution using a list to lazily store the function values
-- |and list indexing to retrieve memoized values.
lf = map (fib memoizedFib) [0..]
memoizedFib = (lf !!)

-- |We see a huge improvement from the basic non-memoized solution but indexing
-- |a list is a O(n) operation. We can improve this by defining an infinite tree
-- |of natural numbers to hold the intermediate results
data BT a = BTNode (BT a) a (BT a)
instance Functor BT where
  fmap f (BTNode l m r) = BTNode (fmap f l) (f m) (fmap f r)

indexBT :: BT a -> Int -> a
indexBT (BTNode _ x _) 0 = x
indexBT (BTNode l _ r) n = case (n - 1) `divMod` 2 of
                             (q, 0) -> indexBT l q
                             (q, 1) -> indexBT r q

nats :: BT Int
nats = go 0 1 where
  go n s = n `seq` s `seq` BTNode (go l s') n (go r s') where
    l = n + s
    r = l + s
    s' = s * 2

-- The intermediate data structure
fibTree = fmap (fib btMemoizedFib) nats

btMemoizedFib :: In -> Out
btMemoizedFib = indexBT fibTree

-- |Here is the same thing written slightly differently, from https://wiki.haskell.org/Memoization
data NT a = NTNode a (NT a) (NT a)

-- | A function to index into nodes
NTNode a tl tr !!! 0 = a 
NTNode a tl tr !!! n =
   if odd n
     then tl !!! top
     else tr !!! (top-1)
        where top = n `div` 2

-- | A functor instance so we can fmap f over our tree
instance Functor NT where
   fmap f (NTNode a tl tr) = NTNode (f a) (fmap f tl) (fmap f tr)

-- | The naturals
naturals r n =
   NTNode n
     ((naturals $! r2) $! (n+r))
     ((naturals $! r2) $! (n+r2))
        where r2 = 2*r

-- | Our function f applied over the naturals
overNats f = go 1 0 where
  go r n = NTNode (f n) ((go $! r2) $! (n + r)) ((go $! r2) $! (n + r2))
    where r2 = 2*r

-- | Written slightly differently?
overNats2 f = go 1 0 where
  go r n = r2 `seq` NTNode (f n) (go r2 $! n + r) (go r2 $! n + r2)
    where r2 = 2*r

fibTreeNT = overNats2 $ fib ntMemoizedFib
ntMemoizedFib = (fibTreeNT !!!)

-- |If you can't think of a good way to index your data from an infinite lazy
-- |data structure, we can use the State monad with a HashMap to store partial
-- |results. In this case we need to write our function in "open recursive
-- |monadic form":

fibMonadic :: Monad m => (In -> m Out) -> In -> m Out
fibMonadic _ 0 = return 1
fibMonadic _ 1 = return 1
fibMonadic mf n = do
  a <- mf (n - 1)
  b <- mf (n - 2)
  return $ a + b

-- |Now we have to run this function in the State monad, storing all values in a HashMap.
-- memoStHashMap :: (Hashable a, Eq a, Monad m) => ((a -> m a) -> a -> m a) -> (a -> a)
memoStHashMap f x = evalState (f getF x) HM.empty where
  getF i = gets (HM.lookup i) >>=
    \case Just v -> return v
          Nothing -> do
            v <- f getF i
            modify $ HM.insert i v
            return v

-- | Our monadically memoized HashMap is the following

monadicallyMemoizedFib :: In -> Out
monadicallyMemoizedFib = memoStHashMap fibMonadic
