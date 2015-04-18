module Matrix where

{-| Library for 2D matrices, modeled as an Array of Arrays. Contains
some functions specific to numeric matrices.

# Creation
@docs initialize, zeros, fromList

# Get and Set
@docs getRow, get, set

# Mapping, Flattening
@docs map, indexedMap, flatten

# Information
@docs size, maximum, minimum

# Numeric Operations
## Arithmetic 
@docs addScalar, mulScalar, constrain
-}

import Array as A
import Array (Array)
import Maybe (Maybe(..), andThen, withDefault)
import List

type alias Coords = (Int, Int)
type alias Matrix a = Array (Array a)

{-| Initializes a matrix. `initialize (x, y) f` creates a matrix with
x rows and y columns, with the element at index `(i, j)` initialized
to `(f (i, j))`.

    initialize (1, 3) identity   == fromList [[(0,0), (0,1), (0,2)]]
    initialize (2, 3) (always 1) == fromList [[1, 1, 1], [1, 1, 1]]
    initialize (3, 3) (\(i, j) -> if i == j then 1 else 0)
        == fromList [[1, 0, 0], [0, 1, 0], [0, 0, 1]]
-}
initialize : Coords -> (Coords -> a) -> Matrix a
initialize (x, y) f =
    let f' = curry f
    in A.initialize x (\i -> A.initialize y (f' i))

{-| Create a matrix with x rows and y columns, filled with zeros. -}
zeros : Coords -> Matrix number
zeros dims = initialize dims (always 0)

{-| Create a matrix from a list of lists. -}
fromList : List (List a) -> Matrix a
fromList lsts =
    A.fromList (List.map A.fromList lsts)

{-| Retrieve row `i` of a matrix, or Nothing if `i` is out of bounds. -}
getRow : Int -> Matrix a -> Maybe (Array a)
getRow = A.get

{-| Retrieve the element at `(i, j)` of a matrix, or Nothing if `i` is out of bounds. -}
get : Coords -> Matrix a -> Maybe a
get (i, j) mat = getRow i mat `andThen` A.get j

{-| Set the element at `(i, j)` to a. If the index is out of bounds,
returns the original matrix.
-}
set : Coords -> a -> Matrix a -> Matrix a
set (i, j) a mat =
    case (getRow i mat) of
      Just row -> A.set i (A.set j a row) mat
      Nothing -> mat

{-| Apply a function `f` to each element of the matrix. -}
map : (a -> b) -> Matrix a -> Matrix b
map f =
    A.map (A.map f)

{-| Apply a function `f` to each element of the matrix, with its
coordinates as the first argument.

    indexedMap (\(i, j) a -> )
-}
indexedMap : (Coords -> a -> b) -> Matrix a -> Matrix b
indexedMap f =
    A.indexedMap (\i row -> A.indexedMap (\j a -> f (i, j) a) row)

size : Matrix a -> Coords
size mat =
    let x = A.length mat
        y = withDefault 0 (getRow 0 mat `andThen` (A.length >> Just))
    in (x, y)

{-| Flatten a matrix into a one-dimensional list (row-major order). -}
flatten : Matrix a -> List a
flatten = List.concatMap A.toList << A.toList

{-| Find the maximum element in the matrix. -}
maximum : Matrix comparable -> comparable
maximum = List.maximum << flatten

{-| Find the minimum element in the matrix. -}
minimum : Matrix comparable -> comparable
minimum = List.minimum << flatten

{-| Add a scalar value to each element of the matrix. -}
addScalar : number -> Matrix number -> Matrix number
addScalar v mat =
    map (\x -> x + v) mat

{-| Multiply the elements of matrix by a scalar value. -}
mulScalar : number -> Matrix number -> Matrix number
mulScalar v mat =
    map (\x -> x * v) mat

{-| Scale the max and min of the matrix to the range [0.0, 1.0]. -}
constrain : Matrix number -> Matrix number
constrain mat =
    let min = minimum mat
        max = maximum mat
        invRange = 1.0 / (max - min)
    in mulScalar invRange (addScalar -min mat)