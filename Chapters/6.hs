-- 1
fac :: Int -> Int
fac 0 = 1
fac n   | n < 0     = error "n must be a positive number"
        | otherwise = n * fac (n - 1)

-- 2
sumdown :: Int -> Int
sumdown 1 = 1

sumdown n   | n < 0     = error "n must be a positive number"
            | otherwise = n + sumdown (n - 1)

-- 3
exp' :: Int -> Int -> Int
exp' _ 0 = 1
exp' n x = n * exp' n (x - 1)


-- 4
euclid :: Int -> Int -> Int
euclid a 0 = a
euclid 0 b = b
euclid a b = euclid b (mod a b)

-- 6
    -- a
and' :: [Bool] -> Bool
and' (x:xs) = if x then and' xs else x

    -- b
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

    -- c
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n a = a : replicate' (n - 1) a

    -- d
get :: [a] -> Int -> a
get (x:xs) 0 = x
get (x:xs) i = xs `get` (i - 1)

    -- e
elem' :: Eq a => a -> [a] -> Bool
elem' y [] = False
elem' y (x:xs) = if x == y then True else elem' y xs

-- 7
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y    = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 8
halve :: [a] -> ([a], [a])
halve xs = splitAt (div (length xs) 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort left) (msort right)
            where
                left = fst a
                right = snd a
                a = halve xs

-- 9
    -- a
rsum :: Num a => [a] -> a
rsum [] = 0
rsum (x:xs) = x + rsum xs

    -- b
rtake :: Int -> [a] -> [a]
rtake 0 _ = []
rtake n (x:xs) = x : rtake (n - 1) xs

    -- c
rlast :: [a] -> a
rlast [x] = x
rlast (x:xs) = rlast xs
