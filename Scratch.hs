module Scratch where

type Pos = (Int, Int)
data Move = North | South | West | East
move :: Move -> Pos -> Pos
move North (x,y) = (x, y+1)
move South (x,y) = (x, y-1)
move East (x,y) = (x+1, y)
move West (x,y) = (x-1, y)

data Nat = Zero | Succ Nat

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + nat2int n

int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (add m n)

class Eq a where
    (==), (/=) :: a -> a -> Bool
    x /= y = not (x Scratch.== y)

instance Scratch.Eq Bool where
    False == False  = True
    True == True    = True
    _ == _          = False


class Scratch.Eq a => Ord a where
    (<), (<=), (>), (>=)    :: a -> a -> Bool
    min, max                :: a -> a -> a

    min x y | x Scratch.<= y    = x
            | otherwise = y

    max x y | x Scratch.<= y    = y
            | otherwise = x
    

bools :: Int -> [[Bool]]
bools 0 = [[]]
bools n = map (False:) bss ++ map (True:) bss where bss = bools (n-1)


data Expr = Val Int | Add Expr Expr

value :: Expr -> Int

type Cont = [Op]
data Op = EVAL Expr | ADD Int

eval :: Expr -> Cont -> Int
eval (Val n) c = exec c n
eval (Add x y) c = eval x (EVAL y: c)

exec :: Cont -> Int -> Int
exec [] n = n
exec (EVAL y:c) n = eval y (ADD n :c)
exec (ADD n: c) m = exec c (n+m)

value e = eval e []


split xs = [splitAt i xs | i <- [1.. (length xs - 1)]]

data List a = EmptyList
            | ListElement a (List a) deriving Show

instance Functor List where
    fmap g EmptyList = EmptyList
    fmap g (ListElement x xs) = ListElement (g x) (fmap g xs)

blah xs = do x <- xs
             return x
