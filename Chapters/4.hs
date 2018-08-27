-- 1
halve :: [a] -> ([a], [a])
halve l | even (length l)   = (take (length l `div` 2) l,
                                drop (length l `div` 2) l)

-- 2
third :: [a] -> a
-- third l | length l >= 3     = head (tail (tail l))
-- third l | length l >= 3    = l !! 2
third (_ : _ : x : _) = x

-- 3
safetail :: [a] -> [a]
safetail l = if (null l) then l else tail l
-- safetail l | null l     = l
--            | otherwise  = tail l
-- safetail [] = []
-- safetail x = tail x

-- 4
-- (||) :: Bool -> Bool -> Bool
-- True || True == True
-- True || False == True
-- False || True == True
-- False || False == False

-- 5
and' :: Bool -> Bool -> Bool
-- and' x y = if x then if y then True else False else False

-- 6
and' x y = if x then y else False;

-- 7
mult :: Int -> Int -> Int -> Int
mult = \x -> \y -> \z -> x * y * z

-- 8
luhnDouble :: Int -> Int
luhnDouble x | x * 2 > 9    = x * 2 - 9
             | otherwise    = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn a b c d = mod (luhnDouble a +  b + luhnDouble c + d) 10 == 0
