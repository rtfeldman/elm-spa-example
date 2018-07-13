port module Session exposing (Session, attempt, changes, clear, init, isLoggedIn, logout, me, store, timeZone, token, withTimeZone)

import AuthToken exposing (AuthToken)
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value)
import Me exposing (Me)
import Time
import UserPhoto
import Username



-- TYPES


type Session
    = Session
        { user : Maybe ( Me, AuthToken )
        , timeZone : Time.Zone
        }



-- CREATE


init : Time.Zone -> Maybe ( Me, AuthToken ) -> Session
init zone user =
    Session
        { user = user
        , timeZone = zone
        }



-- MODIFY


withTimeZone : Time.Zone -> Session -> Session
withTimeZone zone (Session info) =
    Session { info | timeZone = zone }



-- ACCESS


isLoggedIn : Session -> Bool
isLoggedIn (Session info) =
    case info.user of
        Just _ ->
            True

        Nothing ->
            False


me : Session -> Maybe Me
me (Session info) =
    Maybe.map Tuple.first info.user


token : Session -> Maybe AuthToken
token (Session info) =
    Maybe.map Tuple.second info.user


timeZone : Session -> Time.Zone
timeZone (Session info) =
    info.timeZone



-- MODIFY


clear : Session -> Session
clear (Session info) =
    Session { info | user = Nothing }



-- ATTEMPT


attempt : String -> (AuthToken -> Cmd msg) -> Session -> Result String (Cmd msg)
attempt attemptedCmd toCmd session =
    case token session of
        Nothing ->
            Err ("You are signed out. Please sign in to " ++ attemptedCmd ++ ".")

        Just authToken ->
            Ok (toCmd authToken)



-- STORE


store : Me -> AuthToken -> Cmd msg
store myself authToken =
    Encode.object
        [ ( "email", Encode.string (Me.email myself) )
        , ( "username", Username.encode (Me.username myself) )
        , ( "bio", Maybe.withDefault Encode.null (Maybe.map Encode.string (Me.bio myself)) )
        , ( "image", UserPhoto.encode (Me.image myself) )
        , ( "token", AuthToken.encode authToken )
        ]
        |> Encode.encode 0
        |> Just
        |> storeSession


logout : Cmd msg
logout =
    storeSession Nothing


port storeSession : Maybe String -> Cmd msg



-- CHANGES


changes : Time.Zone -> Sub Session
changes zone =
    onSessionChange (fromValue zone)


port onSessionChange : (Value -> msg) -> Sub msg


fromValue : Time.Zone -> Value -> Session
fromValue zone value =
    Session
        { user = Result.toMaybe (Decode.decodeValue Me.decoderWithToken value)
        , timeZone = zone
        }
