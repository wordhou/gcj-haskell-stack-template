-- Monadic style
main = readLn >>= \t -> forM_ [1..t] $ \t' ->
  getParams >>= putStrLn . cs t' . disp . solve

cs t xs = "Case #" ++ show t ++ ": " ++ xs

disp (x,y) = unwords $ map show [x,y]

getParams = do
  n <- readLn
  ls <- replicateM n getLine
  gs <- getLine
  return (n, ls, map read $ words gs :: [Int])

-- Non-monadic style
main = interact $ unlines 
  . zipWith (++) ["Case #" ++ show t ++ ": " | t <- [1..]]
  . map (disp . solve . parse) . tail . lines
main = interact $ unlines
  . zipWith (:) ["Case #" ++ show t ++ ": " | t <- [1..]]
  . map (disp . solve . read) . tail . lines
main = interact $ unlines -- or concat (if strings already unlined)
  . zipWith (++) ["Case #" ++ show t ++ ": " | t <- [1..]]
  . map (disp . solve) . sp . tail . lines -- solution :: String already unlined
-- Cases multiple lines
sp [] = []
sp (x:xs) = let n = read x in take n xs : sp (drop n xs)

sp [] = [] -- Extra problem parameters
sp (x:xs) = (k,m, take n xs) : sp (drop n xs) where [n,k,m] = map read $ words x

where sp [] = [] -- pairs
      sp (x:xs) = let n = read x in map r (take n xs) : sp (drop n xs)
      r l = let [a,b] = map read $ words l in (a,b)

sp (x:c:d:xs) = (n,k,r c, r d) : sp xs where r = map read . words; [n,k] = r x
-- Interactive Problem
import System.IO
import Control.Monad
main = getLine >>= flip replicateM_ runTest . read
send xs = putStrLn xs >> hFlush stdout >> getLine
-- Possible or not
disp :: S -> String
disp Nothing = "IMPOSSIBLE"
disp (Just i) = show i
-- Data Types
data Dir = N | W | E | S deriving (Show, Read, Eq)
data Dir = U | D | L | R deriving (Show, Read, Eq)
data Suit = S | C | H | D deriving (Show, Read, Eq)
data Element = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P |
  Q | R | S | T | U | V | W | X | Y | Z deriving (Eq, Show, Read)
-- Parsing
import Text.ParserCombinator.Parsec


-- Stateful memoization
-- memoStHashMap :: (Hashable a, Eq a, Monad m) => ((a -> m a) -> a -> m a) -> (a -> a)
memoStHashMap f x = evalState (f getF x) HM.empty where
  getF i = gets (HM.lookup i) >>=
    \case Just v -> return v
          Nothing -> do
            v <- f getF i
            modify $ HM.insert i v
            return v
