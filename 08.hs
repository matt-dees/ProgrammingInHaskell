module Exercises where

-- 8.9.1
data Nat = Zero | Succ Nat deriving Show

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ n) m = Succ (add n m)
            
mult :: Nat -> Nat -> Nat
mult Zero m = Zero
mult (Succ n) m = add m (mult n m)

-- 8.9.2
data Tree a = Leaf a | Node (Tree a) a (Tree a)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf v) = x == v
occurs x (Node l v r) = case compare x v of
                        EQ -> True
                        LT -> occurs x l
                        GT -> occurs x r

-- 8.9.3
data Tree' a =  Leaf' a | Node' (Tree' a) (Tree' a) deriving Show

balanced :: Tree' a -> Bool
balanced (Leaf' v) = True
balanced (Node' l r) = abs (numLeaves l - numLeaves r) <= 1 && balanced l && balanced r

numLeaves :: Tree' a -> Int
numLeaves (Leaf' _) = 1
numLeaves (Node' l r) = numLeaves l + numLeaves r 

-- 8.9.4
balance :: [a] -> Tree' a
balance [x] = Leaf' x
balance xs = Node' (balance l) (balance r) where (l,r) = halve xs 

halve :: [a] -> ([a],[a])
halve xs = splitAt (length xs `div` 2) xs

-- 8.9.5
data Expr = Val Int | Add Expr Expr

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add x y) = g (folde f g x) (folde f g y)

-- 8.9.6
eval' :: Expr -> Int
eval' = folde id (+)

size :: Expr -> Int
size = folde (const 1) (+)

-- 8.9.7
data Maybe' a = Just a | Nothing
instance Eq a => Eq (Maybe' a) where
    Exercises.Nothing == Exercises.Nothing = True
    Exercises.Just x == Exercises.Just y = x == y
    _ == _ = False
{-
instance Eq a => Eq [a] where
    [] == [] = True
    [_] == [] = False
    [] == [_] = False
    (x:xs) == (y:ys) = x == y && xs == ys
-}

-- 8.9.8

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

data Prop = Const Bool
          | Var Char
          | Not Prop
          | And Prop Prop
          | Or Prop Prop
          | Imply Prop Prop
          | Equiv Prop Prop

type Subst = Assoc Char Bool

eval :: Subst -> Prop -> Bool
eval _ (Const b) = b
eval s (Var x) = find x s
eval s (Not p) = not (eval s p)
eval s (And p q) = eval s p && eval s q
eval s (Or p q) = eval s p || eval s q
eval s (Imply p q) = eval s p <= eval s q
eval s (Equiv p q) = eval s p == eval s q

-- 8.9.9
-- TBD
