
-- 6.8.1
-- Function application diverges if a negative argument is passed to the definition of factorial over natural numbers.

fac :: Int -> Int
fac n   | n < 0 = error "negative number passed to fac"
        | n == 0 = 1
        | otherwise = n * fac (n-1)

-- 6.8.2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

-- 6.8.3
-- Note: Overlaps with Prelude.^
-- Usage: 3 Main.^ 4
(^) :: Int -> Int -> Int
(^) _ 0 = 1
(^) n m = n * (Main.^) n (m-1)

-- 6.8.4
euclid :: Int -> Int -> Int
euclid n m  | n == m = m
            | n > m = euclid (n-m) m
            | otherwise = euclid n (m-n)

-- 6.8.5
-- length [1,2,3]
-- 1 + (length [2,3])
-- 1 + (1 + (length [3]))
-- 1 + (1 + (1 + (length [])))
-- 1 + (1 + (1 + (0)))
-- 1 + (1 + (1))
-- 1 + (2)
-- 3
--
-- drop 3 [1,2,3,4,5]
-- drop 2 [2,3,4,5]
-- drop 1 [3,4,5]
-- drop 0 [4,5]
-- [4,5]
--
-- init [1,2,3]
-- 1 : init [2,3]
-- 1 : 2 : init [3]
-- 1 : 2 : []
-- 1 : [2]
-- [1, 2]

-- 6.8.6.a
and :: [Bool] -> Bool
and [] = True
and (True:xs) = Main.and xs
and _ = False

-- 6.8.6.b
concat :: [[a]] -> [a]
concat [] = []
concat (xs:xss) = xs ++ Main.concat xss

-- 6.8.6.c
replicate :: Int -> a -> [a]
replicate 0 _ = []
replicate n x = x : Main.replicate (n-1) x 

-- 6.8.6.d
(!!) :: [a] -> Int -> a
(!!) (x:_) 0 = x
(!!) (_:xs) n = (Main.!!) xs (n-1)

-- 6.8.6.e
elem :: Eq a => a -> [a] -> Bool
elem _ [] = False
elem n (x:xs) | x == n = True
              | otherwise = Main.elem n xs

--6.8.7
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- 6.8.8
halve :: [a] -> ([a], [a])
halve xs = splitAt (length xs `div` 2) xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort (fst xs')) (msort (snd xs')) where xs' = halve xs

-- 6.8.9.a
sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + Main.sum xs

-- 6.8.9.b
take :: Int -> [a] -> [a]
take 0 _ = []
take n (x:xs) = x : Main.take (n-1) xs

-- 6.8.9.c
last :: [a] -> a
last [a] = a
last (_:xs) = Main.last xs
