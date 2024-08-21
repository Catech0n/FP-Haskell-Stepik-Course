{-
Реализуйте функцию, находящую сумму и количество цифр десятичной записи заданного целого числа.

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count x = undefined

GHCi> sum'n'count (-39)
(12,2)
-}

sum'n'count :: Integer -> (Integer, Integer)
sum'n'count 0 = (0, 1)
sum'n'count x = f 0 0 (abs x) where
                f sum count 0 = (sum, count)
                f sum count x = f (sum + x `mod` 10) (count + 1) (x `div` 10)
