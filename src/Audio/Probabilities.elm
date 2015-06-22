module Audio.Probabilities where

import Debug exposing (crash)
import Dict exposing (Dict)
import Model exposing (Probable, TokenOffset)
import Random exposing (Generator, generate, float, list, customGenerator)
import Html exposing (text)

mapAccumL : (acc -> x -> (acc, y)) -> acc -> List x -> (acc, List y)
mapAccumL f s xs =
    case xs of
      [] -> (s, [])
      x::xs -> let (s', y ) = f s x
                   (s'',ys) = mapAccumL f s' xs
               in  (s'',y::ys)

type CDF a = CDF (List (Probable a))

toCDF : List (Probable a) -> CDF a
toCDF =
    let f s x = (s + x.prob, { x | prob <- s + x.prob})
    in CDF << snd << mapAccumL f 0.0

probability : Generator Float
probability = float 0 1

map : (a -> b) -> Generator a -> Generator b
map f g =
  customGenerator <| \seed ->
    let (result , seed' ) = generate g seed
    in (f result, seed')

selectFrom : CDF a -> Float -> Probable a
selectFrom cdf s =
    case cdf of
      CDF [] -> crash "Empty CDF passed to selectFrom"
      CDF [x] -> x
      CDF (x::xs) -> if s <= x.prob then x else selectFrom (CDF xs) s

sampleOffsets : CDF TokenOffset -> Generator Float
sampleOffsets cdf = map (.offset << selectFrom cdf) probability

{-
mkProb : Int -> Float -> Probable TokenOffset
mkProb i x = { offset = toFloat i, prob = x}

probs : List (Probable TokenOffset)
probs = List.indexedMap mkProb [0.1, 0.5, 0.4]

cdf : CDF TokenOffset
cdf = toCDF probs

samples = fst <| generate (list 1000 (sampleOffsets cdf)) (Random.initialSeed 1)

count : List comparable -> Dict comparable Int
count lst =
    let f v = case v of
                Nothing -> Just 1
                Just v' -> Just (v' + 1)
        up k d = Dict.update k f d
    in List.foldl up Dict.empty lst


main = text <| toString <| count samples
-}