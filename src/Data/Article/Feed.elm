module Data.Article.Feed exposing (Feed, decoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required)
import Data.Article as Article exposing (Article)


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
