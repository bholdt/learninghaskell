type Peg = String
type Move = (Peg, Peg)
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi num source spare destination 
                | num == 0 = [(source,destination)]
                | otherwise = hanoi (num - 1) spare source destination ++ [(source, destination)] ++ hanoi (num - 1) spare destination source

main = do
           print $ hanoi 3 "a" "b" "c"
