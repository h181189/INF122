-- 3
n = div a (length xs)
    where
        a = 10
        xs = [1 .. 5]

-- 4
last' :: [a] -> a
last' xs = (head . reverse) xs

-- 5
init' :: [a] -> [a]
init' xs = (reverse . tail . reverse) xs

init'' :: [a] -> [a]
init'' [] = error "empty list"
init'' xs = take (length xs - 1) xs
