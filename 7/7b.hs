import Data.List

main = countPaths . lines <$> readFile "input"

countPaths lns = sum $ foldl split beam splitters
    where splitters = [[fromEnum (c=='^') | c <- ln] | ln <- lns]
          beam = [fromEnum (c=='S') | c <- head lns]

split beams splitters = [s1 * b1 + (1-s2) * b2 + s3 * b3 | ((b1,s1), (b2,s2), (b3,s3)) <- threes (0,0) $ zip beams splitters]

threes zero x = threes' $ [zero] ++ x ++ [zero] 
threes' [a,b,c] = [(a,b,c)]
threes' (a:b:c:d) = (a,b,c):threes' (b:c:d)
