module SimpleJSON(JValue(..)) where

import Data.List

data JValue=JString String
            |JNumber Double
            |JBool Bool
            |JNull
            |JObject [(String,JValue)]
            |JArray [JValue]
            deriving (Show ,Eq,Ord)

getString (JString s)=Just s
getString _=Nothing

{--truncat函数返回浮点数的或者有理数的整数部分，不会四舍五入--}
getInt (JNumber n)=Just $ truncate n
getInt _=Nothing

getDouble (JNumber n) = Just n
getDouble _ = Nothing

getBool (JBool b) = Just b
getBool _ = Nothing

getObject (JObject o) = Just o
getObject _ = Nothing

getArray (JArray a) = Just a
getArray _ = Nothing

isNull v= v == JNull


renderJValue::JValue->String
renderJValue (JString s)=s
renderJValue (JNumber n)   = show n
renderJValue (JBool True)  = "true"
renderJValue (JBool False) = "false"
renderJValue JNull         = "null"
renderJValue (JObject o)="{"++pairs o ++"}"
        where pairs []=""
              pairs ps=intercalate "," (map renderPair ps)
              renderPair (k,v)=show k++":"++ renderJValue v
renderJValue (JArray a) = "[" ++ values a ++ "]"
        where values [] = ""
              values vs = intercalate ", " (map renderJValue vs)


putJValue :: JValue -> IO ()
putJValue v = putStrLn (renderJValue v)