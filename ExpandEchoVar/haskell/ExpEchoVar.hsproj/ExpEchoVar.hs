-- Problem:
--   fully implement echo's string expansion in a simple text editor. Example outputs:
--   abc -> abc
--   {a,b}{c,d} -> ac ad bc bd
--   {a,b}{c,g{e,m}}p{q,r} -> acpq acpr agepq agepr agmpq agmpr bcpq bcpr bgepq bgepr bgmpq bgmpr

import Data.List

expEchoExpr :: String -> [String]
expEchoExpr [] = [[]]

expEchoExpr ('{':cs) = [ xs ++ ys | xs <- (expChoicesExpr choices), ys <- (expEchoExpr rest)]
  where (choices, rest) = scan2ChoicesEnd 0 [] cs

expEchoExpr cs = [ xs ++ ys | xs <- (expTextExpr fstExpr), ys <- (expEchoExpr restExpr)]
  where (fstExpr, restExpr) = break (=='{') cs

expTextExpr :: String -> [String]
expTextExpr s = [s]

expChoicesExpr :: String -> [String]
expChoicesExpr choicesStr = concat expandedChoices
  where choiceList = splitChoices choicesStr
        expandedChoices = foldr (\s acc -> (expEchoExpr s : acc)) [[]] choiceList

splitChoices :: String -> [String]
splitChoices [] = [""]
splitChoices cs = splitChoices' 0 "" cs
  where splitChoices' depth seen [] = [reverse seen]
        splitChoices' depth seen (c:cs) =
          case c of
            ',' -> case depth of
                     0 -> reverse seen : splitChoices' 0 "" cs
                     _ -> splitChoices' depth (c:seen) cs
            '{' -> splitChoices' (depth + 1) (c:seen) cs
            '}' -> splitChoices' (depth - 1) (c:seen) cs
            _   -> splitChoices' depth (c:seen) cs


-- Extract the content of a choices expression, plus the rest of string
scan2ChoicesEnd :: Int -> String -> String -> (String, String)
scan2ChoicesEnd depth seen [] = error "not found matching '}'"
scan2ChoicesEnd depth seen (c:cs) =
  case c of
    '{' -> scan2ChoicesEnd (depth + 1) (c:seen) cs
    '}' -> case depth of
             0 -> (reverse seen, cs) -- discard ending '}'
             _ -> scan2ChoicesEnd (depth - 1) (c:seen) cs
    _   -> scan2ChoicesEnd depth (c:seen) cs

--
-- Test cases
--

--splitChoices ""
--splitChoices "a,b"
--splitChoices "c,g{e,m}"
--
--scan2ChoicesEnd 0 "" "a,b}{c,g{e,m}}p{q,r}"
--scan2ChoicesEnd 0 "" "c,g{e,m}}p{q,r}"
--scan2ChoicesEnd 0 "" "}"
--
--expTextExpr ""
--expTextExpr "abc"
--
--expChoicesExpr ""
--expChoicesExpr "a,b,c"
--
--expEchoExpr ""
--expEchoExpr "abc"
--expEchoExpr "{a,b,c}"
--expEchoExpr "{a,b,c{e,m}}"
--expEchoExpr "{a,b}{c,d}"
--expEchoExpr "{a,b}{c,g{e,m}}p{q,r}"


--data ExpType = RawTextExp
--             | ChoicesExp
--             | ConcatExp
--
--data ExpData = ExpData ExpType String
--
---- Form a list of String, discarding empty strings.
--mergeSplits :: String -> String -> [String]
--mergeSplits [] [] = []
--mergeSplits [] ys = [ys]
--mergeSplits xs [] = [xs]
--mergeSplits xs ys = [xs,ys]
--
--adjustDepth :: Char -> Char -> Char -> Int
--adjustDepth openChar closeChar char curDepth
--  | openChar == char  = curDepth + 1
--  | closeChar == char = curDepth - 1
--  | otherwise         = curDepth
--
---- Find the end of current choice string
--splitAtMatching :: Int  -- depth
--                -> (Char -> Bool) -- Return ture for open char
--                -> (Char -> Bool) -- Return ture for close char
--                -> (Char -> Int)  -- return new depth
--                -> String         -- already processed part in reverse order
--                -> String         -- string to proces
--                -> (String, String) -- two sub expressions
--
--splitAtMatching depth isOpen isClose depthFun seen [] = (reverse seen, []) -- not found end of choice expression.
--
--splitAtMatching depth isOpen isClose depthFun seen [c:cs]
--  = if isOpen c
--       then splitAtMatching () cs (c:fstExp)
--
--  = case c of
--      -- Found embeded choices
--      '{' -> splitAtMatching (depth + 1) cs (c:fstExp)
--      -- Found end of a choice expr
--      '}' -> case depth of
--               -- Found the matching '}'
--               0 -> (reverse (c:fstExp), cs)
--               --  Found matching '}' of a nested choice
--               _ -> splitAtMatching (depth - 1) cs (c:fstExp)
--      -- Cointue to search
--      _   -> splitAtMatching depth cs (c:fstExp)
--
--
---- Split the first expression from the rest
--splitExpStr :: String -> [ExpData]
--splitExpStr [] = []
--
----splitExpStr ['{'] = error "not found matching '}'"
--
--splitExpStr '{':cs = mergeSplits xs ys
--  where (xs, ys) = splitAtMatching 0 cs []
--
--splitExpStr cs = mergeSplits cs1 cs2
--  where (cs1, cs2) = break (=='{') cs
--        cs3 = case cs2 of
--                [] -> ExpData Choices []
--                -  -> ExpData Choices (tail cs2)
