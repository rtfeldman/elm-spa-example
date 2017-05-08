module Data.Article.Author exposing (Author, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, custom)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Data.User as User exposing (Username)


decoder : Decoder Author
decoder =
    decode Author
        |> required "username" User.usernameDecoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
        |> required "following" Decode.bool


type alias Author =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , following : Bool
    }
