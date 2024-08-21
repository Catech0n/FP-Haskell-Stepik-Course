{-
Последовательность чисел Фибоначчи 0, 1, 1, 2, 3, 5, 8, 13, 21,... легко определить рекурсивно, 
задав два первых терминирующих значения и определив любое последующее как сумму двух непосредственно предыдущих: 
F_0 = 0, F_1 = 1, F_n = F_{n-1} + F_{n-2}.

На Haskell данное определение задаётся следующей функцией:

fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n - 1) + fibonacci (n - 2)

Эта функция определена лишь для неотрицательных чисел. 
Однако, из данного выше определения можно вывести формулу для вычисления чисел Фибоначчи при отрицательных индексах, 
при этом последовательность будет следующей: F_{-1} = 1, F_{-2} = -1, ..., F_{-10} = -55.

Измените определение функции `fibonacci` так, чтобы она была определена для всех целых чисел и 
порождала при отрицательных аргументах указанную последовательность.﻿
-}
fibonacci :: Integer -> Integer
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n | n > 0 = fibonacci (n - 1) + fibonacci (n - 2)
            | n < 0 = fibonacci (n + 2) - fibonacci (n + 1)
