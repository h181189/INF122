-- 1
sumSquared :: Int -> Int
sumSquared n = sum [x^2 | x <- [1 .. n]]

-- 2
grid :: Int -> Int -> [(Int, Int)]
grid x y = [(x', y') | x' <- [0 .. x], y' <- [0 .. y]]

-- 3
square :: Int -> [(Int, Int)]
square n = [(x, y) | (x, y) <- grid n n, x /= y]

-- 4
replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1 .. n]]

-- 5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x^2 + y^2 == z^2]

-- 6
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n - 1], mod n x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], x == sum (factors x)]

-- 7
-- test = [(x, y) | x <- [1, 2], y <- [3, 4]]
-- test' = [x | x <- [1 .. [y | y <- [1..4] ] ] ]

-- 8

-- 9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x * y | (x, y) <- zip xs ys]
