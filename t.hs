import           Control.Monad

doubleMe x = x + x
doubleUs x y = x * x + y * y

first (a,_,_) = a
second (_,a,_) = a
third (_,_,a) = a
genTri x y z = [(a,b,c) | a <- [1..x], b<-[1..y], c<-[1..z], a <= b, b <= c, a + b > c]
rightTriangles xs = [x | x <- xs, (first x)^2 + (second x)^2 == (third x)^2 ]

factorial :: (Integral a) => a -> a
factorial n
  | n == 0 = 1
  | otherwise = n * factorial(n - 1)

withLen []     = False
withLen (x:xs) = x == length xs

bmiVerdict weight height
  | bmi <= 18.5 = "Freak"
  | bmi <= 25 = "Fit"
  | bmi <= 30 =  "Fatty"
  where bmi = weight / height ^ 2

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
{-quicksort (x:xs) = before ++ [x] ++ after
  where before = quicksort [a | a <- xs, a <= x]
        after  = quicksort [a | a <- xs, a > x]-}
quicksort (x:xs) =
  let before = quicksort [a | a <- xs, a <= x]
      after  = quicksort [a | a <- xs, a > x]
  in before ++ [x] ++ after

apply3 f x = (f (f (f x)))

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | odd n = n: collatz n3
  | otherwise = n : collatz nh
  where n3 = n * 3 + 1
        nh = div n 2

longCollatz n = [(xs !! 0, length xs) | xs <- filter (\xs -> length xs >= 100) $ map collatz [1..n]]

sumFold xs = foldl (\acc x -> acc + x) 0 xs

infixr 5 :.:
data List a = Empty | a :.: (List a) deriving (Show, Read, Eq, Ord)

biang :: Maybe Char
biang = do
  (x:xs) <- Just ""
  return x
