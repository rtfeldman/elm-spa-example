module Me exposing (Me, bio, decoder, decoderWithToken, edit, email, image, login, register, username)

import AuthToken exposing (AuthToken, withAuthorization)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import UserPhoto exposing (UserPhoto)
import Username exposing (Username)
import Util exposing (apiUrl)


{-| The currently signed-in user.

This is used for things like login, logout, and settings.

Contrast with Profile, which is a user whose profile you're viewing.

-}
type Me
    = Me MeRecord


type alias MeRecord =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , email : String
    }



-- ACCESS


username : Me -> Username
username (Me info) =
    info.username


bio : Me -> Maybe String
bio (Me info) =
    info.bio


image : Me -> UserPhoto
image (Me info) =
    info.image


email : Me -> String
email (Me info) =
    info.email



-- SESSION MANAGEMENT


login : { r | email : String, password : String } -> Http.Request ( Me, AuthToken )
login params =
    let
        user =
            Encode.object
                [ ( "email", Encode.string params.email )
                , ( "password", Encode.string params.password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Decode.field "user" decoderWithToken
        |> Http.post (apiUrl "/users/login") body



-- REGISTER


register : { r | username : String, email : String, password : String } -> Http.Request ( Me, AuthToken )
register params =
    let
        user =
            Encode.object
                [ ( "username", Encode.string params.username )
                , ( "email", Encode.string params.email )
                , ( "password", Encode.string params.password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Decode.field "user" decoderWithToken
        |> Http.post (apiUrl "/users") body



-- EDIT


edit :
    AuthToken
    ->
        { r
            | username : String
            , email : String
            , bio : String
            , password : Maybe String
            , image : Maybe String
        }
    -> Http.Request Me
edit authToken params =
    let
        updates =
            [ Just ( "username", Encode.string params.username )
            , Just ( "email", Encode.string params.email )
            , Just ( "bio", Encode.string params.bio )
            , Just ( "image", Maybe.withDefault Encode.null (Maybe.map Encode.string params.image) )
            , Maybe.map (\pass -> ( "password", Encode.string pass )) params.password
            ]
                |> List.filterMap identity

        body =
            ( "user", Encode.object updates )
                |> List.singleton
                |> Encode.object
                |> Http.jsonBody

        expect =
            Decode.field "user" decoder
                |> Http.expectJson
    in
    apiUrl "/user"
        |> HttpBuilder.put
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withBody body
        |> withAuthorization (Just authToken)
        |> HttpBuilder.toRequest



-- SERIALIZATION


decoder : Decoder Me
decoder =
    Decode.succeed MeRecord
        |> required "username" Username.decoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
        |> required "email" Decode.string
        |> Decode.map Me


decoderWithToken : Decoder ( Me, AuthToken )
decoderWithToken =
    Decode.succeed Tuple.pair
        |> custom decoder
        |> required "token" AuthToken.decoder
