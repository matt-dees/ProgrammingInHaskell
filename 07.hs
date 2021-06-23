module Exercises where

import Data.Char

-- 7.9.1
comprehension f p xs = [f x | x <- xs, p x]

comprehension' f p = Prelude.map f . Prelude.filter p 

-- 7.9.2.a
all :: (a -> Bool) -> [a] -> Bool
all p = and . Prelude.map p

-- 7.9.2.b
any :: (a -> Bool) -> [a] -> Bool
any p = or . Prelude.map p

-- 7.9.2.c
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) | p x = x : Exercises.takeWhile p xs
                   | otherwise = []

-- 7.9.2.d
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) | p x = Exercises.dropWhile p xs
                   | otherwise = x:xs

-- 7.9.3
map f = foldr (\x xs -> f x : xs) []

filter p = foldr (\x xs -> if p x then x:xs else xs) []

-- 7.9.4
dec2int :: [Int] -> Int
dec2int = foldl (\v x -> 10*v + x) 0

-- 7.9.5
curry :: ((a,b) -> c) -> (a -> (b -> c))
curry f = \x y -> f (x,y)

uncurry :: (a -> (b -> c)) -> ((a,b) -> c)
uncurry f = \(x,y) -> f x y

-- 7.9.6

unfold :: (a->Bool) -> (a -> b) -> (a -> a) -> a -> [b]
unfold p h t x | p x       = []
  | otherwise = h x : unfold p h t (t x)

chop8 :: [a] -> [[a]]
chop8 = unfold null (take 8) (drop 8)

unfoldMap :: (a -> b) -> [a] -> [b]
unfoldMap f = unfold null (f . head) tail

iterate = unfold (const False) id

-- 7.9.7
type Bit = Int
bin2int :: [Bit] -> Int
bin2int = foldr (\x y -> x + 2*y) 0

chop :: Int -> [Bit] -> [[Bit]]
chop _ [] = []
chop 0 _ = []
chop n bits = take n bits : chop n (drop n bits)

int2bin :: Int -> [Bit]
int2bin 0 = []
int2bin n = n `mod` 2 : int2bin (n `div` 2)

make8 :: [Bit] -> [Bit]
make8 bits = take 8 (bits ++ repeat 0)

encode :: String -> [Bit]
encode = concat . Prelude.map (addParityBit . make8 . int2bin . ord)

decode :: [Bit] -> String
decode = Prelude.map (chr . bin2int. verify) . chop 9

transmit :: String -> String
transmit = decode . channel . encode

channel :: [Bit] -> [Bit]
channel = id

addParityBit :: [Bit] -> [Bit]
addParityBit bs = bs ++ [sum bs `mod` 2]

verify :: [Bit] -> [Bit]
verify bits | even (sum bits) = init bits
            | otherwise = error "verification failed!"

-- 7.9.8
faultyChannel = tail
faultyTransmit = decode . faultyChannel . encode

-- 7.9.9.
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap f g xs = [if even i then f x else g x | (x, i) <- zip xs [0..]] 

-- 7.9.10
sub9 x = if x > 9 then x - 9 else x
luhn :: [Int] -> Bool
luhn = div10 . sum . Prelude.map sub9 . altMap (*2) id where div10 = \x -> x `mod` 10 == 0


