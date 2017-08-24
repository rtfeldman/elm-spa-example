module Pair exposing ((=>), Pair(..), first, map2, mapFirst, mapSecond, second, toTuple)


type Pair a b
    = Pair a b


first : Pair a b -> a
first (Pair a _) =
    a


second : Pair a b -> b
second (Pair _ b) =
    b


map2 : (a -> b -> c) -> Pair a b -> c
map2 transform (Pair a b) =
    transform a b


mapFirst : (a -> c) -> Pair a b -> c
mapFirst transform (Pair a _) =
    transform a


mapSecond : (b -> c) -> Pair a b -> c
mapSecond transform (Pair _ b) =
    transform b


toTuple : Pair a b -> ( a, b )
toTuple (Pair a b) =
    ( a, b )


(=>) : a -> b -> Pair a b
(=>) =
    Pair
