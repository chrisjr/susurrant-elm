module Common where

import Array exposing (Array)
import Maybe exposing (Maybe, withDefault)
import Result
import String
import List

last : Array a -> Maybe a
last arr = Array.get ((Array.length arr) - 1) arr

toList : Maybe a -> List a
toList x =
    case x of
      Just x_ -> [x_]
      Nothing -> []

nth : Int -> Array Float -> Float
nth i arr = withDefault 0.0 <| Array.get i arr

argsort : Array comparable -> List Int
argsort arr =
    let idxs = Array.toIndexedList arr
    in List.map fst <| List.sortBy snd idxs

argmax : Array comparable -> Int
argmax = withDefault -1 << List.head << List.reverse << argsort

orElse : Result x a -> a -> a
orElse res x = withDefault x <| Result.toMaybe res

roundPct : Float -> String
roundPct x =
    let pct = toString <| x * 100.0
    in  String.left 4 pct ++ "%"