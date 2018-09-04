-- 1 and 3 : Chapter 4

-- 2
f :: Int -> Int
f n = (sum . map (^2)) [1 .. n]

f' :: Int -> Int
f' n = sum [x^2 | x <- [1 .. n]]

f'' :: Int -> Int
f'' n = sum (map (\x -> x^2) [1 .. n])

-- 4
toList :: Int -> [Int]
toList 0 = []
toList n = toList number ++ [mod n 10]
            where 
                number = div (n - mod n 10) 10

toList' :: Int -> [Int]
toList' n = reverse [number x | x <- [0 .. limit]]
            where
                number x = div (mod n (10^(x + 1))) (10^x)
                limit = (floor . logBase 10 . fromIntegral) n
