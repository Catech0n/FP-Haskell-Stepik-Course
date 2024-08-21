{-
Реализуйте функцию seqA, находящую элементы следующей рекуррентной последовательности

a_0 = 1; a_1 = 2; a_2 = 3; a_{k + 3} = a_{k + 2} + a_{k + 1} − 2a_k

Попытайтесь найти эффективное решение.

GHCi> seqA 301
1276538859311178639666612897162414
-}

seqA :: Integer -> Integer
seqA n | n >= 3 = let
                    f a b c 0 = a
                    f a b c 1 = b
                    f a b c 2 = c
                    f a b c k = f b c (c + b - 2 * a) (k - 1)
                  in f 1 2 3 n
       | otherwise = error "k must be >= 0"
