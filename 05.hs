

-- 5.7.1
first_100_sqs = sum [i^2 | i <- [1..100]]

-- 5.7.2
grid :: Int -> Int -> [(Int, Int)]
grid m n = [(x,y) | x <- [0..m], y <- [0..n]]

-- 5.7.3
square :: Int -> [(Int, Int)]
square n = [(x,y) | (x,y) <- grid n n, x /= y]

-- 5.7.4
replicate :: Int -> a -> [a]
replicate n v = [v | _ <- [1..n]]

-- 5.7.5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x, y, z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- 5.7.6
factors :: Int -> [Int]
factors n = [m | m <- [1..n], n `mod` m == 0]

perfects :: Int -> [Int]
perfects n = [m | m <- [1..n], sum (init (factors m)) == m ]

-- 5.7.7
nested_gen = concat [[(x,y) | y <- [3,4]] | x <- [1,2]]

-- 5.7.8
find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k', v) <- t, k == k']

positions :: Eq a => a -> [a] -> [Int]
positions x xs = find x (zip xs [0..])

-- 5.7.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 5.7.10
-- TBD
