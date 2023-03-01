module Prime (nth) where

nth :: Int -> Maybe Integer
nth n = if n < 1 then Nothing else Just $ last $ take n [x | x <- [2..], isPrime x]

-- Alternative solution (without list comprehension)
-- nth n = if n < 1 then Nothing else Just (prime n 1)
prime :: Int -> Integer -> Integer
prime n cur = if n > 0 
           then if isPrime cur then prime (n-1) (cur+1) else prime n (cur+1)
           else cur-1

isPrime :: Integer -> Bool
isPrime n
    | n < 2 = False
    | length multiples > 2 = False
    | otherwise = True
    where multiples = [x | x <- [1..n], n `mod` x == 0]
