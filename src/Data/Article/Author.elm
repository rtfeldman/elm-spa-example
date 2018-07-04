module Data.Article.Author exposing (Author, decoder)

import Data.User as User
import Data.User.Photo as UserPhoto exposing (UserPhoto)
import Data.User.Username as Username exposing (Username)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)


decoder : Decoder Author
decoder =
    Decode.succeed Author
        |> required "username" Username.decoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
        |> required "following" Decode.bool


type alias Author =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , following : Bool
    }
