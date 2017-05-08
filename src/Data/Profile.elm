module Data.Profile exposing (Profile, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Data.User as User exposing (Username)


type alias Profile =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , following : Bool
    }


decoder : Decoder Profile
decoder =
    decode Profile
        |> required "username" User.usernameDecoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
        |> required "following" Decode.bool
