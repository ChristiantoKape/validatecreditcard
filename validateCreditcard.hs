-- VALIDATE CREDIT CARD

-- function 1 = remove last digit
dropLastInit :: [Int] -> [Int]
dropLastInit = init 

dropLast :: [Int] -> [Int]
dropLast [] = []
dropLast [x] = []
dropLast (x:xs) = x:dropLast xs

-- function 1b = save last digit sbg check digit
checkDigitAlt :: [Int] -> Int
checkDigitAlt x = head (reverse x)

checkDigitAlt2 :: [Int] -> Int
checkDigitAlt2 x = last x

checkDigit :: [Int] -> Int
checkDigit [x] = x
checkDigit (_:xs) = checkDigit xs

-- function 2 = reverse list
reverseListAlt :: [Int] -> [Int]
reverseListAlt = reverse

reverseList :: [Int] -> [Int]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

-- function 3 = kalikan digit di posisi ganjil (1,3,5)
multiplyOddPlacesby2 :: [Int] -> [Int]
multiplyOddPlacesby2 [] = []
multiplyOddPlacesby2 [x] = [x*2]
multiplyOddPlacesby2 (x:y:xs) = (2*x): y : multiplyOddPlacesby2 xs

-- function 4 = kurangi dengan 9 semua nomor yang nilainya lebih dari 9
subtract9 :: [Int] -> [Int]
subtract9 [] = []
subtract9 (x:xs)
        | x > 9 = (x-9) : subtract9 xs
        | otherwise = x : subtract9 xs

-- function 5 = jumlahkan semua
addAll :: [Int] -> Int
addAll = sum

-- function 6 = cek apakah bisa di bagi 10
isDivisibleby10 :: Int -> Bool
isDivisibleby10 n
        | n `mod` 10 == 0 = True
        | otherwise = False

-- function 7 = validate
validate :: [Int] -> Bool
validate xs = let withoutLastElement = dropLast xs
                  reversedList = reverseList withoutLastElement
                  oddPlacesDoubled = multiplyOddPlacesby2 reversedList
                  nineSubtracted = subtract9 oddPlacesDoubled
                  sumAll = addAll nineSubtracted
                  sumPlusCheckDigit = sumAll + (checkDigit xs)
                in isDivisibleby10 sumPlusCheckDigit


