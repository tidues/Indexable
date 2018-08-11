module ExtraFuncs where

import Data.Text(pack, unpack, strip)

-- | test if the string is a integral number
isDigit :: Char -> Bool
isDigit c = elem c "0123456789"

isIntegral :: String -> Bool
isIntegral str 
    | length str == 0 = False 
    | (str!!0) == '0' && length str == 1 = True
    | (str!!0) == '0' = False
    | (str!!0) == '-' = all isDigit (tail str)
    | otherwise = all isDigit str



-- | String to all readable types
load ::(Read a) => String -> a -> a
load str v = asTypeOf (read str) v


-- | strip strings
strStrip :: String -> String
strStrip = unpack.strip.pack


-- | concat with
concatWith :: String -> [String] -> String
concatWith sep strLst =
    let res = foldr (\x y -> x ++ sep ++ y) "" strLst
    in  take ((length res)-1) res
