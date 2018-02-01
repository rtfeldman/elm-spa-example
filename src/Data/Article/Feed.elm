module Data.Article.Feed exposing (Feed, decoder)

import Data.Article as Article exposing (Article)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (decode, required)


type alias Feed =
    { articles : List (Article ())
    , articlesCount : Int
    }



-- SERIALIZATION --


decoder : Decoder Feed
decoder =
    decode Feed
        |> required "articles" (Decode.list Article.decoder)
        |> required "articlesCount" Decode.int
