import System.IO 

main = readFile "input" >>= return . countzeros . lines

countzeros:: [String] -> Int
countzeros = snd . foldl f (50,0)

f:: (Int,Int) -> String -> (Int,Int)
f (x, count) (dir:dx)  =
    (fx, fcount)
    where
        fx = mod (x + sign dir * read dx) 100
        fcount = count + fromEnum (fx == 0)

sign 'L' = -1
sign 'R' = 1
