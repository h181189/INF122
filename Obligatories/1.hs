module Oblig1 where
    -- Johansen

    -- 1.1

    data AST = C String | Seq AST AST | Star AST | Plus AST AST
        deriving Show

    {-
        If the expression is empty, it means everything has been parsed.
        If there is something left in the expression, it throws an exception. It
        should never come to this, because everything else is either covered
        or thrown an exception to in parseEl.
    -}
    parse :: String -> AST
    parse ex = case parseRe ex of
        (tree, "")  -> tree
        (_, rest)   -> error ("Unexpected end of line: " ++ rest)

    {-
        Simply checks if the returned expression starts with '+', and if so
        it acts accordingly. If not, then it returns the pair.
    -}
    parseRe :: String -> (AST, String) -- Plus
    parseRe ex = case parseSq ex of
        (tree, '+':rest)    ->  let (tree2, rest2) = parseRe rest
                                in (Plus tree tree2, rest2)
        pair                ->  pair

    {-
        If the returned expression starts with +, it should be parsed by parseRe.
        If it starts with ), then it should be parsed by parseEl, which is
        two levels higher in the recursion stack.
    -}
    parseSq :: String -> (AST, String) -- Seq
    parseSq ex = case parseBa ex of
        (tree, '+':rest)    -> (tree, '+':rest)
        (tree, ')':rest)    -> (tree, ')':rest)
        (tree, "")          -> (tree, "")
        (tree, rest)        ->  let (tree2, rest2) = parseSq rest
                                in (Seq tree tree2, rest2)

    {-
        A convenient way of removing reoccuring stars is to drop them. It makes
        no sense to have multiple stars after one another, rather than just one.
    -}
    parseBa :: String -> (AST, String) -- Star
    parseBa ex = case parseEl ex of
        (tree, '*':rest)    -> (Star tree, dropWhile (=='*') rest)
        pair                -> pair

    {-
        parseEl first checks for parenthesis, and parses the sub-expression if
        found. If there are any terminal characters here that aren't
        recognized as one in our language, it will throw an error.
    -}
    parseEl :: String -> (AST, String) -- C
    parseEl (e:ex) | e == '('  = case parseRe ex of
                        (tree, ')':rest)    -> (tree, rest)
                        _ -> error "Parenthesis mismatch"
                    | isTerm    = (C [e], ex)
                    | otherwise = error ("Unexpected character: " ++ [e])
                    where
                        isTerm = elem e (['0' .. '9'] ++ ['a' .. 'z'])

    -- 1.2

    type Rules = [(String, String)]
    type Gr = (String, Rules)

    {-
        Compute the next non-terminal symbol for numbers and characters.
        The max number of possible nonterminals are 26 (A .. Z).
    -}
    nextNonterm :: String -> String
    nextNonterm nts = if length nts >= length ['A' .. 'Z']
        then error "no more nonterminals"
        else [(['A' .. 'Z'] !! (length nts))]

    {-
        Filters out rules where the result is empty. E.g. ("A", "")
    -}
    removeBlanks :: Rules -> Rules
    removeBlanks rules = filter (\(_, rule) -> not $ null rule) rules

    {-
        This function and the one beneath fixes the rules for a sequence.
    -}
    fixSeqRule :: String -> String -> (String, String) -> (String, String)
    fixSeqRule nt nts (x, rule) | null rule             = (x, nt)
                                | elem (last rule) nts  = (x, rule)
                                | otherwise             = (x, rule ++ nt)

    fixSeqRules :: String -> String -> Rules -> Rules
    fixSeqRules nt nts rs = map (\r -> fixSeqRule nt nts r) rs

    {-
        This function and the one beneath are used to modify the rules for
        stars. It checks if a rule is terminal, and if so, it adds the start
        symbol to the end of the result.
    -}
    fixStarRule :: String -> String -> (String, String) -> (String, String)
    fixStarRule nt nts (x, rule)    | elem (last rule) nts  = (x, rule)
                                    | otherwise             = (x, rule ++ nt)

    fixStarRules :: String -> String -> Rules -> Rules
    fixStarRules nt nts rs = foldr (\r a -> (fixStarRule nt nts r) : a) [] rs

    {-
        Creates a grammar using the AST.
    -}
    gr :: AST -> Gr
    gr tree = fst $ grMap tree ""

    {-
        Evaluates a tree and returns the exact result. It wouldn't need to keep
        the type when evaluating it further, but removing it would only cause
        an unnecessary risk.
    -}
    grMap :: AST -> String -> (Gr, String)
    grMap (Plus tree1 tree2) nts    = grPlus (Plus tree1 tree2) nts
    grMap (Seq tree1 tree2) nts     = grSeq (Seq tree1 tree2) nts
    grMap (Star tree) nts           = grStar (Star tree) nts
    grMap (C c) nts                 = grChar (C c) nts

    {-
        Creates a new nonterminal start symbol which is mapped to each
        alternative path to the subtrees.
    -}
    grPlus :: AST -> String -> (Gr, String)
    grPlus (Plus tree1 tree2) nts = ((start, rules), finalTs)
                            where
                                ((start1, rules1), nts1) = grMap tree1 nts
                                ((start2, rules2), nts2) = grMap tree2 nts1
                                start = nextNonterm nts2
                                finalTs = start ++ nts2
                                newRules = [(start, start1), (start, start2)]
                                rules = newRules ++ rules1 ++ rules2

    {-
        Evaluates its subtrees and makes sure every path leads to the start of
        the next subtree.
    -}
    grSeq :: AST -> String -> (Gr, String)
    grSeq (Seq tree1 tree2) nts = ((start1, rules), nts2)
                                where
                                    ((start1, rules1), nts1) = grMap tree1 nts
                                    ((start2, rules2), nts2) = grMap tree2 nts1
                                    rules = (fixSeqRules start2 nts1 rules1) ++ rules2

    {-
        Maps the start element to an empty string, as well as any removing any
        stub in the subtree by adding the start symbol to the end. This makes
        it easier when creating a sequence, and it does not have any other side
        effects.
    -}
    grStar :: AST -> String -> (Gr, String)
    grStar (Star tree) nts = ((start, rules), finalNts)
                                where
                                    ((start, oldRules), nts1) = grMap tree nts
                                    rules = (start, "") : fixStarRules start nts1 oldRules
                                    finalNts = nts1


    {-
        The leaf in our AST. Very simply gets the next nonterminal symbol and
        maps it to the given character.
    -}
    grChar :: AST -> String -> (Gr, String)
    grChar (C c) nts = ((nonTerm, [(nonTerm, c)]), nts ++ nonTerm)
        where
            nonTerm = nextNonterm nts

    -- 1.3

    {-
        Creates a grammar of the parsed expression and checks the given string
        against the grammar.
    -}
    mem :: String -> String -> Bool
    mem str ex = gpr str (gr $ parse ex)

    {-
        A generic parser which accepts a grammar and a string. It will use
        the testPaths function to test each path, and return true if any of
        the paths return true.
    -}
    gpr :: String -> Gr -> Bool
    gpr str (start, rs) = or [testPaths str r rs | r <- rs, fst r == start]

    {-
        Tests the current character. If it is valid, it call the gpr function
        to check if the rest of the string is valid.
    -}
    testPaths :: String -> (String, String) -> Rules -> Bool
    testPaths str (_, "") _ = null str
    testPaths "" (_, _) _ = False
    testPaths (s:str) (_, x:"") rs  | nonterm   = gpr (s:str) ([x], rs)
                                    | null str  = s == x
                                    | otherwise = False
                                    where
                                        nonterm = elem x ['A' .. 'Z']
    testPaths (s:str) (_, x:y:[]) rs    | s /= x    = False
                                        | otherwise = gpr str ([y], rs)
