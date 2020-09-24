import System.IO
--Zhixuan Fang
--7:00pm-9:00pm

--1a
keep :: [a] -> [a]
keep [] = []
keep (x:y:xs) = x:keep xs
keep [y,z] = [y]
keep [y] = [y]
keep [] = []

--1b

keepLess :: [a]->[a]
keepLess xs = keep (keep xs)

--2a

factors ::Integer->[Integer]
factors x = [y|y<-[1..x],x `mod` y==0]

--2b

p_1   :: Integer -> Bool
p_1 s = factors s == [1,s]

p_2 :: Integer -> Bool
p_2 s = or [ p_1 (s `div` x) | x<-(factors s), p_1 x]

pseudoprime :: Integer -> [Integer]
pseudoprime s = [x | x<- [1..s], p_2 x]

--2c

selectRange :: Int -> Int -> [a] -> [a]
selectRange m n xs
  | m > n = []
  | otherwise = [ v | (i,v) <- zip [1..] xs, i >= m, i <= n  ]

--3a
--mapThem [(+2), (*3)] [1,2,3,4]
--mapThem [(*3), (+2)] [1,2,3,4]

mapThem:: [a->a]->[a]->[a]
mapThem [] ys = ys
mapThem (f:fs) ys = mapThem fs (map f ys)

--3b
combine:: Num a => (a->a)->(a->a)->(a->a)
combine f g =(\x -> 2*(f x) + 3*(g x))

--4a
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)
tree_1 = Node 4 (Node 4 Empty Empty) (Node 4 Empty Empty)  --almost_balanced
tree_2 = Node 4 Empty (Node 4 (Node 4 Empty Empty) (Node 4 Empty Empty)) --not_almost
--4b

noOfNodes :: Tree a->Int
noOfNodes Empty = 0
noOfNodes (Node n left right) = 1+ (noOfNodes left)+ (noOfNodes right)

--4c
abalanced :: Tree a->Bool
abalanced Empty = True
abalanced (Node a Empty Empty) = True
abalanced (Node a left right) 
  | ((noOfNodes left) > (noOfNodes right)) && (noOfNodes left) - (noOfNodes right)<=2 = True
  | ((noOfNodes left) <= (noOfNodes right)) && (noOfNodes right) - (noOfNodes left)<=2 = True
  | otherwise = False

--5
fun :: [Integer]->[Integer]
fun xs = foldr(\y ys -> y:(map(+y)ys))[]xs
--6a
suffixsum ::[Int]->[Int]
suffixsum [] = []
suffixsum [a] = [a]
suffixsum (x:xs) = x+head(suffixsum xs):suffixsum xs

--6b
printSufSum ::[Int]->IO()
printSufSum xs = do sequence_[putStrLn (show y)|y<-suffixsum xs]

--7a
data Prop = Const Bool
            | Var Char
            | Not Prop
            | And Prop Prop
            | Or Prop Prop
            | Imply Prop Prop

pa :: Prop

pa = Imply (And(Var 'A') (Or (Var 'A') (Var 'B'))) (Not (Var 'B')) 
 
--7b

countSym :: Prop -> Int
countSym (Const _) = 0
countSym (Var _) = 0
countSym (Not p) = 1+ countSym p
countSym (And p1 p2) = 1 + countSym p1 + countSym p2
countSym (Or p1 p2) = 1 + countSym p1 + countSym p2
countSym (Imply p1 p2) = 1 + countSym p1 + countSym p2