import System.IO 

main = readFile "input" >>= return . countzeros . lines

countzeros:: [String] -> Int
countzeros = snd . foldl f (50,0)

f:: (Int,Int) -> String -> (Int,Int)
f (x, count) (sign_str:dx_str) =
    g dx x count
    where
        sgn = sign sign_str
        dx = read dx_str
        g 1 0 count = ((mod sgn 100), count+1)
        g 1 x count = ((mod (x+sgn) 100), count)
        g dx 0 count = g (dx-1) (mod sgn 100) (count+1)
        g dx x count = g (dx-1) (mod (x+sgn) 100) count

sign 'L' = -1
sign 'R' = 1
