{-
Реализуйте оператор |-|, который возвращает модуль разности переданных ему аргументов:

GHCi>  5 |-| 7
2
-}
x |-| y = if x - y >= 0 then x - y else y - x
-- x |-| y = abs (x - y)
