type Peg = String
type Move = (Peg, Peg)
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi num a b c 
                | num == 1 = [(a,c)]
                | otherwise = hanoi (num - 1) a b c

main = do
           print $ hanoi 2 "a" "b" "c"
