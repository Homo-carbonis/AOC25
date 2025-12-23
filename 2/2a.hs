import System.IO 

main = sum . filter invalid . concat . fmap readRange . split ',' <$> withFile "input" ReadMode hGetLine

readRange :: String -> [Int]
readRange str = [(read start)..(read stop)]
    where [start,stop] = split '-' str


invalid id = 
    a == b
    where str = show id
          mid = length str `div` 2
          (a,b) = splitAt mid str

split c s = case dropWhile (==c) s of
  "" -> []
  s' -> w : split c s''
    where
      (w, s'') =
        break (==c) s'
