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


