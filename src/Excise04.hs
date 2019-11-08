module Excise04 where

import Data.Char as Ch
import Data.List

-- 分割换行符兼容windows和unix
splitLines cs=
    let (pre,suf)=break (\c->c=='\r'||c=='\n') cs
    in pre:case suf of 
                '\r':'\n':xs->splitLines xs
                '\n':xs->splitLines xs
                '\r':xs->splitLines xs
                _->[]

-- 该函数可以转换windows下的文件换行符到\n
fixLines xs=unlines $ splitLines xs

safeHead :: [a]->Maybe a
safeHead []=Nothing 
safeHead (x:_)=Just x

safeTail :: [a] -> Maybe [a]
safeTail [] =Nothing 
safeTail  (_:xs)=Just xs

safeLast :: [a] -> Maybe a
safeLast [] =Nothing
safeLast [x] =Just x
safeLast (_:xs)= safeLast xs

safeInit :: [a] -> Maybe [a]
safeInit [] =Nothing 
safeInit xs=Just (getHead xs)
        where getHead [x]=[]
              getHead (x:cs)=x:getHead cs 

myWorlds::String->[String]
myWorlds s =case dropWhile isSpace s of 
                ""->[]
                s'->w:myWorlds s''
                    where (w,s'')=break isSpace s'

splitWith ::(a->Bool)->[a]->[[a]]
splitWith fun s=case dropWhile fun s of 
                    []->[]
                    s'->w:splitWith fun s''
                        where (w,s'')=break fun s'

-- square []=[]
-- square (x:xs)=x*x:square xs

square :: (Num a)=>[a]->[a]
square =myMap (\x->x*x)

myMap ::(a->b)->[a]->[b]
myMap f (x:xs)=f x :myMap f xs
myMap _ []=[]

myFilter ::(a->Bool)->[a]->[a]
myFilter f (x:xs)
            | f x =x:myFilter f xs
            | otherwise =myFilter f xs
myFilter _ []=[]

asInt ::String->Integer
asInt []=0
asInt "-"=0
asInt (x:xs)
    | x=='-'= toInteger (-1*value xs)
    |otherwise =toInteger (value (x:xs))
    where value =foldl'  (\acc x->acc*10+digitToInt x) 0

myContact::[[a]]->[a]
myContact []=[]
myContact (x:xs)=x++myContact xs

myContact_Fold::[[a]]->[a]
myContact_Fold=foldl' (++) [] 

myTakeWhile _ []=[]
myTakeWhile f (x:xs)
        | f x=x:myTakeWhile f xs
        |otherwise =[]

{--
foldl表示向左遍历即从右向左折叠
Foldable t =>
(b -> a -> b) -> b -> t a -> b
--}
myTakeWhileFoldl f =foldl step []
    where step xs x| f x=x:xs
                     |otherwise =[]
{--foldr表示向右遍历即从左向右折叠
Foldable t =>
(a -> b -> b) -> b -> t a -> b
--}
myTakeWhileFoldr f =foldr step []
    where step x xs | f x=x:xs
                     |otherwise =[]


myGroupBy :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupBy _ []=[]
myGroupBy f (x:y:xs) 
            | f x y=[x,y]:myGroupBy f xs
            |otherwise=[x]:[y]:myGroupBy f xs


myGroupByFold :: (a -> a -> Bool) -> [a] -> [[a]]
myGroupByFold _ []=[]
myGroupByFold fn (x : xs) = foldl step [[x]] xs
                    where   step acc x' = if all (\gx -> fn x' gx) lastGroup
                                        then previousGroups ++ [lastGroup ++ [x']]
                                        else acc ++ [[x']]
                                            where
                                                lastGroup = last acc
                                                previousGroups = init acc

myAny _ []=False
myAny fn (x:xs)=let is=fn x
                in is || myAny fn xs

myAnyFold _ []=False
myAnyFold f (x:xs)=foldr step (f x) xs
                where step x' acc=acc || f x'
            
myWords []=[]
myWords xs=let xs'=dropWhile isSpace xs
               (pre,suf)=break isSpace xs'
           in pre:myWords suf

myWordsFold []=[]
myWordsFold xs=foldl step [[]] xs
                where step acc x
                            |isSpace x=acc++[[]]
                            |otherwise =pre++[lastGroup++[x]]
                            where pre =init acc
                                  lastGroup=last acc
