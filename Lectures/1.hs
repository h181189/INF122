fac :: Int -> Int
fac 1 = 1
fac n = n * fac (n - 1)

fac2 :: Int -> Int
fac2 n = if n <= 0 then 1 else n * fac2 (n - 1)
