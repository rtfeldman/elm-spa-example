module Data.Article.Comment exposing (Comment, CommentId, decoder, idToString, commentIdDecoder)

import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as Pipeline exposing (decode, required, custom)
import Json.Decode.Extra
import Date exposing (Date)
import Data.Article.Author as Author exposing (Author)


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
