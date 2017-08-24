module Data.Session exposing (Session, attempt)

import Data.AuthToken exposing (AuthToken)
import Data.User as User exposing (User)
import Pair exposing ((=>), Pair(Pair))


type alias Session =
    { user : Maybe User }


attempt : String -> (AuthToken -> Cmd msg) -> Session -> Pair (List String) (Cmd msg)
attempt attemptedAction toCmd session =
    case Maybe.map .token session.user of
        Nothing ->
            [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "." ] => Cmd.none

        Just token ->
            [] => toCmd token
