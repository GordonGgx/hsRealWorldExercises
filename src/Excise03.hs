module Excise03 where
import Data.List as List

length' :: (Num a)=>[a]->Integer
length' []=0
length' (_:xs)=1+length' xs

avg :: (Fractional a)=>[a]->a
avg []=0
avg xs= xsSum / xsLength
    where xsSum = sum xs
          xsLength=fromInteger $ length' xs

huiwen []=[]
huiwen xs=xs ++ rever xs []
    where rever [] a=a
          rever (x:xs) a=rever xs (x:a)

isHuiwen []=True
isHuiwen [a]=True
isHuiwen xs | length xs `mod` 2 /=0 =False
isHuiwen xs = head xs == last xs && isHuiwen ((init.tail) xs)

sortByLength :: [[a]]->[[a]]
sortByLength =List.sortBy (\l r -> compare (length l) (length r))

myIntersperse :: a-> [[a]]->[a]
myIntersperse _ []=[]
myIntersperse _ [x]=x
myIntersperse spec (x:xs)=x++spec:myIntersperse spec xs


data Tree a = Node a (Tree a) (Tree a) |Empty deriving(Show)


treeLength Empty =0
treeLength (Node _ left right)
                    | ll>rl = ll
                    | ll <= rl = rl
                    where ll = 1+treeLength left
                          rl=  1+ treeLength right

data Direction =DLeft | DRight | DLine  deriving(Show)

data Point =Point Double Double

instance Show Point where
    show (Point x y)='{':show x++',':show y++"}"

direction :: Point -> Point ->Point ->Direction
direction p1 p2 p3
    | angle p1 p2 < angle p2 p3=DLeft
    | angle p1 p2 > angle p2 p3=DRight
    | angle p1 p2 == angle p2 p3 = DLine
    where angle (Point ax ay) (Point bx by)=let y=by-ay
                                                x=bx-ax
                                            in atan (y/x)

directionList :: [Point]->[Direction]
directionList [x,y,z]=[direction x y z]
directionList (x:y:z:xs)=direction x y z :directionList (y:z:xs)