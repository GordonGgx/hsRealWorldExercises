module PrettyJSON  where

import SimpleJSON
import Prettify(Doc, (<>), string, series, char, double, fsep, hcat, punctuate, text, compact)


renderJValue :: JValue -> Doc
renderJValue (JBool True)  = text "true"
renderJValue (JBool False) = text "false"
renderJValue JNull         = text "null"
renderJValue (JNumber num) = double num
renderJValue (JString str) = text str
renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
    where field (name,val) = text name
                          Prettify.<> text ": "
                          Prettify.<> renderJValue val