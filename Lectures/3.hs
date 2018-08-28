import Test.QuickCheck

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
   where
        smaller = [a | a <- xs, a <= x]
        larger = [b | b <-xs, b > x]

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:x':xs) = x <= x' && sorted(x':xs)

-- quickCheck(\xs -> sorted (qsort xs))

len :: [a] -> Int
len xs = sum [1 | _ <- xs]
