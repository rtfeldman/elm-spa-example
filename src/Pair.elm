module Pair exposing ((=>), Pair(Pair), first, map2, mapFirst, mapSecond, second, toTuple)


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


mapFirst : (a -> c) -> Pair a b -> Pair c b
mapFirst transform (Pair a b) =
    Pair (transform a) b


mapSecond : (b -> c) -> Pair a b -> Pair a c
mapSecond transform (Pair a b) =
    Pair a (transform b)


toTuple : Pair a b -> ( a, b )
toTuple (Pair a b) =
    ( a, b )


(=>) : a -> b -> Pair a b
(=>) =
    Pair
