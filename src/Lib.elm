module Lib exposing (..)

find : (a -> Bool) -> List a -> Maybe a
find p l = case l of
    [] -> Nothing
    (x::xs) -> if p x then Just x else find p xs


ifEmpty : a -> List a -> List a
ifEmpty x xs = case xs of
    [] -> [x]
    _ -> xs
