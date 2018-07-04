module Data.Profile exposing (Profile, decoder)

import Data.User as User
import Data.User.Photo as UserPhoto exposing (UserPhoto)
import Data.User.Username as Username exposing (Username)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type alias Profile =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , following : Bool
    }


decoder : Decoder Profile
decoder =
    Decode.succeed Profile
        |> required "username" Username.decoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
        |> required "following" Decode.bool
