port module Session exposing (Session, attempt, onSessionChange)

import Data.AuthToken exposing (AuthToken)
import Data.User exposing (Me, User)
import Time


type alias Session =
    { user : Maybe (User Me)
    , timeZone : Time.Zone
    }


attempt : String -> (AuthToken -> Cmd msg) -> Session -> ( List String, Cmd msg )
attempt attemptedAction toCmd session =
    case Maybe.map .token session.user of
        Nothing ->
            ( [ "You have been signed out. Please sign back in to " ++ attemptedAction ++ "." ], Cmd.none )

        Just token ->
            ( [], toCmd token )


port onSessionChange : (Value -> msg) -> Sub msg
