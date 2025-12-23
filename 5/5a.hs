import Data.Ix
main = countFresh . lines <$> readFile "input"

countFresh lns = length $ filter (\id -> any (`inRange` id) ranges) ids
    where (rangeLns, _:idLns) = break  (=="") lns
          ranges :: [(Int,Int)] = readRange <$> rangeLns
          ids :: [Int] = read <$> idLns
          
readRange str = (read a, read b)
    where (a, _:b) = break (=='-') str