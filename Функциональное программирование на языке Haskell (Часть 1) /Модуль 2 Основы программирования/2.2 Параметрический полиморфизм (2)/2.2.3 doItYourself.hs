{-
Функция одной переменной doItYourself выбирает наибольшее из переданного ей аргумента и числа 42, 
затем возводит результат выбора в куб и, наконец, вычисляет логарифм по основанию 2 от полученного числа. 
Эта функция реализована в виде:

doItYourself = f . g . h
Напишите реализации функций f, g и h. Постарайтесь сделать это в бесточечном стиле.

f = undefined

g = undefined

h = undefined
-}

doItYourself = f . g . h

h :: Double -> Double
f = logBase 2

h :: Double -> Double
g = (^ 3)

h :: Double -> Double
h = max 42 
