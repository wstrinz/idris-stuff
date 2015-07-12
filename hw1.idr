-- Based on https://github.com/Haskallywags/school-of-haskell/tree/master/cis194/homework-1

CCNumber : Type
CCNumber = Integer

toS : Char -> String
toS x = cast x

toDigits : CCNumber -> List Integer
toDigits 0 = []
toDigits cc =  map (cast . toS) $ unpack $ show cc

toDigitsRev : CCNumber -> List Integer
toDigitsRev = reverse . toDigits

doubleEveryOtherHelper : List Integer -> List Integer
doubleEveryOtherHelper (x::y::xs) = x :: 2 * y :: doubleEveryOtherHelper xs
doubleEveryOtherHelper a = a

doubleEveryOther : List Integer -> List Integer
doubleEveryOther = reverse . doubleEveryOtherHelper . reverse

sumDigits : List Integer -> Integer
sumDigits = sum . map (sum . toDigits)

validate : CCNumber -> Bool
validate = (0 ==) . flip mod 10 . sumDigits . doubleEveryOther . toDigits

-- -- Without pointfree silliness:
--
-- validate x = (0 ==) $ ccCalc x `mod` 10 where
--   ccCalc = sumDigits . doubleEveryOther . toDigits
