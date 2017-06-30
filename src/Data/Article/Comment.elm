module Data.Article.Comment exposing (Comment, CommentId, commentIdDecoder, decoder, idToString)

import Data.Article.Author as Author exposing (Author)
import Date exposing (Date)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Extra
import Json.Decode.Pipeline exposing (custom, decode, required)


type alias Comment =
    { id : CommentId
    , body : String
    , createdAt : Date
    , updatedAt : Date
    , author : Author
    }



-- SERIALIZATION --


decoder : Decoder Comment
decoder =
    decode Comment
        |> required "id" commentIdDecoder
        |> required "body" Decode.string
        |> required "createdAt" Json.Decode.Extra.date
        |> required "updatedAt" Json.Decode.Extra.date
        |> required "author" Author.decoder



-- IDENTIFIERS --


type CommentId
    = CommentId Int


idToString : CommentId -> String
idToString (CommentId id) =
    toString id


commentIdDecoder : Decoder CommentId
commentIdDecoder =
    Decode.map CommentId Decode.int
