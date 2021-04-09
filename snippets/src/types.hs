-- ContinuedFraction
import Data.List

data ContinuedFraction = CF Integer [Integer] --correct data type
instance Show ContinuedFraction where
  show (CF x xs) = '[' : show x ++ ";" ++ tail (show xs)

cf (a,b) = CF x xs where 
  (x:xs) = unfoldr f (a,b)
  f (_,0) = Nothing
  f (a,b) = let (q,r) = a `quotRem` b in Just (q,(b,r))

-- OpenInterval type
data OpenInterval = Interval Ratio Ratio | Empty
interval r r' = if r' <= r then Empty else Interval r r'
instance Semigroup OpenInterval where
  Interval a b <> Interval c d = interval (max a c) (min b d)
  _ <> _ = Empty
instance Monoid OpenInterval where mempty = Interval (ratio (-1) 0) (ratio 1 0)

instance Show OpenInterval where
  show Empty = "∅"
  show (Interval a b) = "(" ++ show a ++ "," ++ show b ++ ")"

-- Simple ratio type
data Ratio = R Int Int
instance Eq Ratio where R a b == R c d = a*d == c*b
instance Ord Ratio where
  R a 0 <= R c 0 = signum a <= signum c
  R a b <= R c d = a*d <= c*b
instance Show Ratio where
  show (R a 0) = (if a > 0 then "" else "-") ++ "∞"
  show (R a 1) = show a
  show (R a b) = show a ++ '%' : show b
instance Num Ratio where
  abs (R a b) = R (abs a) b
  negate (R a b) = R (negate a) b
  signum (R a _) = R (signum a) 1
  fromInteger i = R (fromInteger i) 1
  R a b * R c d = ratio (a*c) (b*d)
  R a b + R c d = ratio (a*d + c*b) (b*d)
