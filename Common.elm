module Common where

import Array
import Array (Array)
import Maybe (Maybe, withDefault)
import List

last : Array a -> Maybe a
last arr = Array.get ((Array.length arr) - 1) arr

nth : Int -> Array Float -> Float
nth i arr = withDefault 0.0 <| Array.get i arr

argsort : Array comparable -> List Int
argsort arr =
    let idxs = Array.toIndexedList arr
    in List.map fst <| List.sortBy snd idxs

argmax : Array comparable -> Int
argmax = List.head << List.reverse << argsort
    