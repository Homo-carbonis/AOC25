import System.IO
import Data.List
import Data.Maybe 

main = sum . fmap maximumJoltage . lines <$> readFile "input"

maximumJoltage :: String -> Int
maximumJoltage str = read [d1,d2]
    where d1 = maximum $ init str
          d2 = maximum $ drop (fromJust (elemIndex d1 str) + 1) str
