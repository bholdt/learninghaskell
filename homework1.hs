toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (n `quot` 10) ++ [n `mod` 10]
toDigitsRev n
    | n <= 0 = []
    | otherwise = n `mod` 10 : toDigitsRev (n `quot` 10)

doubleEverySecond :: [Integer] -> [Integer]
doubleEverySecond [] = []
doubleEverySecond [x] = [x]
doubleEverySecond (x:y:zs) = x : (y + y) : doubleEverySecond zs


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse(doubleEverySecond(reverse(xs)))

main = do
    print (toDigits(123))
    print (toDigitsRev(123))
    print (doubleEveryOther([8,7,6,5]))​
