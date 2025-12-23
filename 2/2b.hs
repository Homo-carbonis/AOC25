import System.IO
import Data.List
import Data.Maybe 

main = sum . filter invalid . concat . fmap readRange . tok ',' <$> withFile "input" ReadMode hGetLine

readRange :: String -> [Int]
readRange str = [(read start)..(read stop)]
    where [start,stop] = tok '-' str


invalid id = 
    any (alleq . (`split` str)) [1..mid]
    where str :: String = show id
          mid ::Int = length str `div` 2

split :: Int -> String -> [String]
split n = unfoldr (splitToMaybe . splitAt n)

splitToMaybe ([],_) = Nothing
splitToMaybe pair = Just pair


tok :: Char -> String -> [String]
tok c s = case dropWhile (==c) s of
  "" -> []
  s' -> w : tok c s''
    where
      (w, s'') =
        break (==c) s'

alleq list = all (== head list) list
