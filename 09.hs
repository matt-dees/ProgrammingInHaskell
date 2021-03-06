module Main where

-- Countdown solution
data Op  = Add | Sub | Mul | Div 

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mul = "*"
    show Div = "/" 

valid :: Op -> Int -> Int -> Bool
valid Add x y = x <= y
valid Sub x y = x > y
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = x /= 1 && x `mod` y == 0

apply :: Op -> Int -> Int -> Int
apply Add x y = x + y
apply Sub x y = x - y
apply Mul x y = x * y
apply Div x y = x `div` y

data Expr = Val Int | App Op Expr Expr

instance Show Expr where
    show (Val n) = show n
    show (App o l r) = brak l ++ show o ++ brak r
                        where
                            brak (Val n) = show n
                            brak e = "(" ++ show e ++ ")"


values :: Expr -> [Int]
values (Val n) = [n]
values (App _ l r) = values l ++ values r

eval :: Expr -> [Int]
eval (Val n) = [n | n > 0]
eval (App o l r) = [apply o l' r' | l' <- eval l, r' <- eval r, valid o l' r']

subs :: [a] -> [[a]]
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss where yss = subs xs

interleave :: a -> [a] -> [[a]]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
perms [] = [[]]
perms (x:xs) = concat (map (interleave x) (perms xs))

choices :: [a] -> [[a]]
choices = concat . map perms . subs

solution :: Expr -> [Int] -> Int -> Bool
solution e ns n =
    elem (values e) (choices ns) && eval e == [n]

split :: [a] -> [([a], [a])]
split [] = []
split [_] = []
split (x:xs) = ([x], xs) : [(x:ls,rs) | (ls,rs) <- split xs]

ops :: [Op]
ops = [Add, Sub, Mul, Div]

combine :: Expr -> Expr -> [Expr]
combine l r = [App o l r | o <- ops]

exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs ns = [e | (ls,rs) <- split ns, l <- exprs ls, r <- exprs rs, e <- combine l r]

solutions :: [Int] -> Int -> [Expr]
solutions ns n = [e | ns' <- choices ns, e <- exprs ns', eval e == [n]]

combine' :: Result -> Result -> [Result]
combine' (l,x) (r,y) = [(App o l r, apply o x y) | o <- ops, valid o x y]

type Result = (Expr, Int)
results [] = []
results [n] = [(Val n,n) | n > 0]
results ns = [res | (ls, rs) <- split ns,
                    lx       <- results ls,
                    ry       <- results rs,
                    res      <- combine' lx ry]

solutions' :: [Int] -> Int -> [Expr]
solutions' ns n = [e | ns' <- choices ns, (e,m) <- results ns', m == n]

main :: IO ()
main = print (solutions' [1,3,7,10,25,50] 765)

-- 9.11.1
choices' :: [a] -> [[a]]
choices' xs = [ p | s <- subs xs, p <- perms s]

-- 9.11.2
isChoice :: Eq a => [a] -> [a] -> Bool
isChoice [] _ = True
isChoice _ [] = False
isChoice (x:xs) ys | x `notElem` ys  = False
                   | otherwise = isChoice xs ys'
                        where
                            ys' = remove x ys

remove :: Eq a => a -> [a] -> [a]
remove _ [] = []
remove x (y:ys) | x == y = ys
                | otherwise = y: remove x ys

-- 9.11.3 
-- Infinite loop because then the invariant that Expr reduces the size of each expression
-- would be violated.

-- 9.11.4

nums = [1, 3, 7, 10, 25, 50] :: [Int]

possibleExpressions :: [Int] -> [Expr]
possibleExpressions xs = concatMap exprs (choices xs)

numExprs = length (possibleExpressions nums)

successfulEvals :: [Expr] -> [Int]
successfulEvals es = [r | expr <- es, r <- eval expr ]
numSuccessfulEvals = length . successfulEvals

answer = numSuccessfulEvals (possibleExpressions nums)

-- 9.11.5

valid' :: Op -> Int -> Int -> Bool
valid' Add x y = True
valid' Sub x y = True
valid' Mul x y = True
valid' Div x y = True

-- 9.11.6
-- TBD
