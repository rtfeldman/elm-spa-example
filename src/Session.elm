port module Session exposing (Session, attempt, changes, me, store, timeZone, token, withTimeZone)

import Data.AuthToken exposing (AuthToken)
import Data.User exposing (Me, User)
import Time



-- TYPES


type Session
    = Session
        { user : Maybe ( Me, AuthToken )
        , timeZone : Time.Zone
        }



-- MODIFY


withTimeZone : Time.Zone -> Session -> Session
withTimeZone timeZone (Session session) =
    Session { session | timeZone = timeZone }



-- ACCESS


me : Session -> Maybe Me
me (Session session) =
    Maybe.map Tuple.first session.user


token : Session -> Maybe AuthToken
token (Session session) =
    Maybe.map Tuple.second session.user


timeZone : Session -> Time.Zone
timeZone (Session session) =
    session.timeZone



-- ATTEMPT


attempt : String -> (AuthToken -> Cmd msg) -> Session -> Result String (Cmd msg)
attempt attemptedCmd toCmd session =
    case token session of
        Nothing ->
            ( [ "You are signed out. Please sign in to " ++ attemptedCmd ++ "." ], Cmd.none )

        Just authToken ->
            ( [], toCmd authToken )



-- STORE


store : Me -> AuthToken -> Cmd msg
store me authToken =
    Encode.object
        [ ( "email", Encode.string (email me) )
        , ( "username", Username.encode (username me) )
        , ( "bio", Maybe.withDefault Encode.null (Maybe.map Encode.string (bio me)) )
        , ( "image", UserPhoto.encode (image me) )
        , ( "token", AuthToken.encode (token me) )
        ]
        |> Encode.encode 0
        |> Just
        |> storeSession


port storeSession : Maybe String -> Cmd msg



-- CHANGES


changes : Sub (Maybe Me)
changes =
    onSessionChange
        (\value -> Result.toMaybe (Decode.decodeValue User.decoder value))


port onSessionChange : (Value -> msg) -> Sub msg
