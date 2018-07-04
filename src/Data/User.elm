module Data.User exposing (User, decoder, encode)

import Data.AuthToken as AuthToken exposing (AuthToken)
import Data.User.Photo as UserPhoto exposing (UserPhoto)
import Data.User.Username as Username exposing (Username)
import Html exposing (Html)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Url.Parser


type alias User =
    { email : String
    , token : AuthToken
    , username : Username
    , bio : Maybe String
    , image : UserPhoto
    , createdAt : String
    , updatedAt : String
    }



-- SERIALIZATION


decoder : Decoder User
decoder =
    Decode.succeed User
        |> required "email" Decode.string
        |> required "token" AuthToken.decoder
        |> required "username" Username.decoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
        |> required "createdAt" Decode.string
        |> required "updatedAt" Decode.string


encode : User -> Value
encode user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "token", AuthToken.encode user.token )
        , ( "username", Username.encode user.username )
        , ( "bio", Maybe.withDefault Encode.null (Maybe.map Encode.string user.bio) )
        , ( "image", UserPhoto.encode user.image )
        , ( "createdAt", Encode.string user.createdAt )
        , ( "updatedAt", Encode.string user.updatedAt )
        ]
