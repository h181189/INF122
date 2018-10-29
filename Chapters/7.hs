-- 1
higherOrder :: (a -> b) -> (a -> Bool) -> [a] -> [b]
higherOrder f p = map f . filter p

-- 2
    -- a
all' :: (a -> Bool) -> [a] -> Bool
all' p xs = length xs == (length . filter p) xs

    -- b
any' :: (a -> Bool) -> [a] -> Bool
any' p xs = (length . filter p) xs > 0

    -- c
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p (x:xs) = if p x then x : takeWhile' p xs else []

    -- d
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p (x:xs) = if p x then dropWhile' p xs else (x:xs)

-- 3
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr ((:) . f) [] xs

addIf :: (a -> Bool) -> a -> [a]
addIf p x = if p x then [x] else []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p (x:xs) = foldr ((++) . addIf p) [] xs

-- 4
dec2int :: [Int] -> Int
dec2int xs = foldl (\x y -> x * 10 + y) 0 xs

-- 5
curry' :: ((a, b) -> c) -> a -> b -> c
curry' f a b = f (a, b)

uncurry' :: (a -> b -> c) -> (a, b) -> c
uncurry' f (a, b) = f a b

-- 6
unfold :: (a -> Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x  | p x       = []
                | otherwise = h x : unfold p h t (t x)

uMap :: (a -> b) -> [a] -> [b]
uMap f xs = unfold (\ks -> length ks == 0) (f . head) (drop 1) xs

uIterate :: (a -> a) -> a -> [a]
uIterate f x = x : unfold (\a -> False) f f x

uChop8 :: [Int] -> [[Int]]
uChop8 bits = unfold (\xs -> length xs == 0) (take 8) (drop 8) bits

-- 9
altMap :: [(a -> b)] -> [a] -> [b]
altMap f xs = foldr (\x ys -> snd x (fst x) : ys) [] (zip xs (cycle f))

-- 10
luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9    = x * 2 - 9
             | otherwise    = x * 2

luhn :: [Int] -> Bool
luhn xs = mod (sum (altMap [luhnDouble, (\x -> x)] xs)) 10 == 0
