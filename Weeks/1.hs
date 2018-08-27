-- B
    -- 1
plu :: [Int] -> Int -> [Int]
plu k n = map (+n) k

    -- 2
pali :: String -> Bool
pali x = x == reverse x

-- C
reverseTwo :: [[a]] -> [[a]]
reverseTwo xs = map reverse xs

-- D
    -- 1
del :: Int -> [Int]
del n = filter (\x -> mod x 3 == 0 || mod x 5 == 0) [1 .. n]

    -- 2
dell :: Int -> [Int] -> [Int]
dell n ks = filter (\x -> any (\k -> mod x k == 0) ks) [1 .. n]

