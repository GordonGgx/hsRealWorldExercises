module Prettify where

import Numeric(showHex)
import Data.Bits (shiftR, (.&.))
import Data.Char(ord)

data Doc =Empty
          |Char Char
          |Text String
          |Line
          |Concat Doc Doc
          |Union Doc Doc 
            deriving(Show,Eq)


string::String->Doc
string str=enclose '"' '"'  (hcat (map oneChar str))

-- 该函数将一个Doc值用起始字符和终止字符包裹起来
enclose::Char ->Char ->Doc ->Doc
enclose left right x=char left Prettify.<> x Prettify.<> char right

--将一组Doc列表合并成一个
hcat::[Doc]->Doc
hcat =fold (Prettify.<>)

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

--oneChar 函数将一个单独的字符进行转义或转换
oneChar::Char->Doc
oneChar c=case lookup c simpleEscapes of 
            Just r->text r
            Nothing | mustEscape c->hexEscape c
                    |otherwise ->char c
            where mustEscape c=c<' '||c=='\x7f'||c>'\xff'

simpleEscapes::[(Char,String)]
simpleEscapes=zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
    where ch a b=(a,['\\',b])

hexEscape::Char->Doc
hexEscape c | d<0x10000=smallHex d
            |otherwise =astral (d-0x10000)
            where d=ord c

smallHex::Int->Doc
smallHex x= text "\\u"
            Prettify.<> text (replicate (4-length h) '0')
            Prettify.<> text h
            where h=showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xd800) Prettify.<> smallHex (b + 0xdc00)
    where a = (n `shiftR` 10) .&. 0x3ff
          b = n .&. 0x3ff

--处理json数组和对象打印，它们看起来很像：以起始字符开头，中间是用逗号隔开的一系列值，以终止字符结束
series :: Char -> Char -> (a -> Doc) -> [a] -> Doc
series open close f = enclose open close
                    . fsep . punctuate (char ',') . map f

fsep :: [Doc] -> Doc
fsep  = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x Prettify.<> softline Prettify.<> y

softline :: Doc
softline = group line

group :: Doc -> Doc
group x = flatten x `Union` x

--将 Line 替换为一个空格，把两行变成一行。
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x `Concat` flatten y
flatten Line           = Char ' '
flatten (x `Union` _)  = flatten x
flatten other          = other

punctuate :: Doc -> [Doc] -> [Doc]
punctuate p []     = []
punctuate p [d]    = [d]
punctuate p (d:ds) = (d Prettify.<> p) : punctuate p ds

--Doc类型的连接器函数类似与++函数
(<>)::Doc->Doc->Doc
Empty <> y=y
x <> Empty= x
x <> y=x `Concat` y

empty ::Doc
empty=Empty

char ::Char->Doc
char =Char  

text::String->Doc
text ""=Empty
text str=Text str

double::Double->Doc
double num=text $ show num

line :: Doc
line = Line

-- 紧凑转换
compact :: Doc -> String
compact x = transform [x]
    where transform [] = ""
          transform (d:ds) =
                case d of
                    Empty        -> transform ds
                    Char c       -> c : transform ds
                    Text s       -> s ++ transform ds
                    Line         -> '\n' : transform ds
                    a `Concat` b -> transform (a:b:ds)
                    _ `Union` b  -> transform (b:ds)