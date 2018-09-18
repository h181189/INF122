-- Imports
import Data.Char

-- 1
sub :: Int -> Int -> [a] -> [a]
sub a b xs = [x | (x, i) <- zip xs [0 ..], a <= i, i <= b - 1]

-- 2
remove :: Eq t => [t] -> t -> [t]
remove ks k = [x | x <- ks, x /= k]

remove' :: Eq t => [t] -> t -> [t]
remove' ks k = filter (/= k) ks

-- 3
charPos :: Char -> String -> [Int]
charPos k ks = [i | (x, i) <- zip ks [0 ..], x == k]

-- 4 - Chapter 6.6

-- 5
eqli :: Eq t => [t] -> [t] -> Bool
eqli [] [] = True
eqli _ [] = False
eqli [] _ = False
eqli (x:xs) ys  | elem x ys = eqli (filter (/= x) xs) (filter (/= x) ys) 
                | otherwise = False

remeqli :: Eq t => [[t]] -> [[t]]
remeqli [] = []
remeqli (xs:list) = xs : remeqli rest
    where
        rest = map (filter (\l -> not (elem l xs))) list

-- 6
clean :: String -> String
clean [] = []
clean (x:xs)    | isAlpha x = toLower x : clean xs
                | otherwise = clean xs

-- 7
tokenize :: String -> String -> String -> [String]
tokenize [] _ _ = [[]]
tokenize (s:str) imp rem    | elem s imp    = [[], [s]] ++ rest
                            | elem s rem    = [] ++ rest
                            | otherwise     = [s : head rest] ++ tail rest
                                where rest = tokenize str imp rem

-- 8
opener :: Char -> [(Char, Char)] -> Bool
opener c xs = any ((==) c . fst) xs

closer :: Char -> [(Char, Char)] -> Bool
closer c xs = any ((==) c . snd) xs

findOpener :: Char -> [(Char, Char)] -> Char
findOpener c [] = error "no opener for char"
findOpener c (x:xs) = if c == snd x then fst x else findOpener c xs

isMatch :: String -> [(Char, Char)] -> String -> Bool
isMatch [] _ opened = null opened
isMatch (s:str) ps pars    | null pars && closes                = False
                                | isOpener                      = isMatch str ps (s : pars)
                                | closes && closesOpened        = isMatch str ps (tail pars)
                                | not (closes && closesOpened)  = False
                                | otherwise                     = isMatch str ps pars
                                where
                                    closes = closer s ps
                                    isOpener = opener s ps
                                    closesOpened = findOpener s ps == head pars

validate :: String -> Bool
validate str = isMatch str [('(', ')'), ('[',']'), ('{','}')] ""
