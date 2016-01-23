module ExpEchoVar1 where

import Prelude hiding (exp)
import Data.List

-- exp means expand (or expansion) in this code snippet

exp :: String -> [String]
exp "" = [""] -- Serves as identity list in cartesian product
              -- L1 x ["] = L1

exp ('{':cs) = [ xs ++ ys | xs <- (expOpts opts), ys <- (exp rest)]
  where (opts, rest) = extractOptStr 0 [] cs

exp cs = [ xs ++ ys | xs <- (expText fstExp), ys <- (exp restExp)]
  where (fstExp, restExp) = break (=='{') cs

expText :: String -> [String]
expText s = [s] -- Pure text as singleton list

expOpts :: String -> [String]
expOpts cs = concat expandedOpts
  where optList = splitOpts cs
        expandedOpts = foldr (\s acc -> (exp s : acc)) [] optList

-- Split an option list string into a list of its option strings
splitOpts :: String -> [String]
splitOpts [] = [""]

splitOpts cs = splitOpts' 0 "" cs
  where splitOpts' depth seen [] = [reverse seen]
        splitOpts' depth seen (c:cs) =
          case c of
            ',' -> case depth of
                     0 -> reverse seen : splitOpts' 0 "" cs
                     _ -> splitOpts' depth (c:seen) cs
            '{' -> splitOpts' (depth + 1) (c:seen) cs
            '}' -> splitOpts' (depth - 1) (c:seen) cs
            _   -> splitOpts' depth (c:seen) cs

-- Extract the content between '{' and '}', plus the rest of string
extractOptStr :: Int -> String -> String -> (String, String)
extractOptStr depth seen [] = error "not found matching '}'"

extractOptStr depth seen (c:cs) =
  case c of
    '{' -> extractOptStr (depth + 1) (c:seen) cs
    '}' -> case depth of
             0 -> (reverse seen, cs) -- discard ending '}'
             _ -> extractOptStr (depth - 1) (c:seen) cs
    _   -> extractOptStr depth (c:seen) cs
