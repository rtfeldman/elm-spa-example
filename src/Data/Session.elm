module Data.Session exposing (Session, attempt)

import Data.User exposing (User)
import Data.AuthToken exposing (AuthToken)
import Util exposing ((=>))


type alias Session =
    { user : Maybe User }


attempt : String -> (AuthToken -> Cmd msg) -> Session -> ( List String, Cmd msg )
attempt attemptedAction toCmd session =
    case Maybe.map .token session.user of
        Nothing ->
            [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "." ] => Cmd.none

        Just token ->
            [] => toCmd token
