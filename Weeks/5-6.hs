-- 3
data Exp = S | U | Og Exp Exp | El Exp Exp | Ik Exp

    -- a
folde :: a -> a -> (a -> a -> a) -> (a -> a -> a) -> (a -> a) -> Exp -> a
folde s u o e i S = s
folde s u o e i U = u
folde s u o e i (Og x y) = o (folde s u o e i x) (folde s u o e i y)
folde s u o e i (El x y) = e (folde s u o e i x) (folde s u o e i y)
folde s u o e i (Ik x) = i (folde s u o e i x)

    -- b
evb :: Exp -> Bool
evb exp = folde True False (&&) (||) (not) exp

    -- c
evi :: Exp -> Int
evi exp = folde 1 5 (+) (*) negate exp

    -- d
succMax x y = succ (max x y) 

evh :: Exp -> Int
evh exp = folde 1 1 succMax succMax succ exp
