-- 3
product' :: [Int] -> Int
product' xs = foldr (*) 1 xs

-- 4
qsort [] = []
qsort (x:xs) = qsort larger ++ [x] ++ qsort smaller
               where
                    smaller = [a | a <- xs, a <= x]
                    larger = [b | b <-xs, b > x]
