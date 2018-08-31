module Article.Comment exposing (Comment, author, body, createdAt, delete, id, list, post)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article)
import Article.Slug as Slug exposing (Slug)
import Author exposing (Author)
import CommentId exposing (CommentId)
import Http
import Iso8601
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Time



-- TYPES


type Comment
    = Comment Internals


type alias Internals =
    { id : CommentId
    , body : String
    , createdAt : Time.Posix
    , author : Author
    }



-- INFO


id : Comment -> CommentId
id (Comment comment) =
    comment.id


body : Comment -> String
body (Comment comment) =
    comment.body


createdAt : Comment -> Time.Posix
createdAt (Comment comment) =
    comment.createdAt


author : Comment -> Author
author (Comment comment) =
    comment.author



-- LIST


list : Maybe Cred -> Slug -> Http.Request (List Comment)
list maybeCred articleSlug =
    Decode.field "comments" (Decode.list (decoder maybeCred))
        |> Api.get (Endpoint.comments articleSlug) maybeCred



-- POST


post : Slug -> String -> Cred -> Http.Request Comment
post articleSlug commentBody cred =
    let
        bod =
            encodeCommentBody commentBody
                |> Http.jsonBody
    in
    Decode.field "comment" (decoder (Just cred))
        |> Api.post (Endpoint.comments articleSlug) (Just cred) bod


encodeCommentBody : String -> Value
encodeCommentBody str =
    Encode.object [ ( "comment", Encode.object [ ( "body", Encode.string str ) ] ) ]



-- DELETE


delete : Slug -> CommentId -> Cred -> Http.Request ()
delete articleSlug commentId cred =
    Api.delete (Endpoint.comment articleSlug commentId) cred Http.emptyBody (Decode.succeed ())



-- SERIALIZATION


decoder : Maybe Cred -> Decoder Comment
decoder maybeCred =
    Decode.succeed Internals
        |> required "id" CommentId.decoder
        |> required "body" Decode.string
        |> required "createdAt" Iso8601.decoder
        |> required "author" (Author.decoder maybeCred)
        |> Decode.map Comment
