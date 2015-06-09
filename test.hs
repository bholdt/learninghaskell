myLast :: [a] -> a
myLast [] = error "NOTHING"
myLast [x] = x
myLast (_:xs) = myLast xs

lastButOne :: [a] -> a
lastButOne (x1:[x2]) = x1
lastButOne (x:xs) = lastButOne xs

elementAt :: [b] -> Int -> b
elementAt (x:xs) n = 
  if length xs == n 
    then x
    else elementAt xs n

main = do
  print $ elementAt (reverse ["test", "something", "again", "why"]) 0
