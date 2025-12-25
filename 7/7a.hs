import Data.List

main = fst . foldl split (0,[70]) . fmap (elemIndices '^') . lines <$> readFile "input"

split (count, beams) splitters = (count + length splits,  pass `union` splitted)
    where pass = beams \\ splitters
          splits = beams `intersect` splitters
          splitted = concatMap (\x -> [x+1,x-1]) splits
