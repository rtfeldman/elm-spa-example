module Article.Comment
    exposing
        ( Comment
        , author
        , body
        , createdAt
        , id
        )

import CommentId exposing (CommentId)
import Data.Article as Article exposing (Article)
import Data.Article.Author as Author exposing (Author)
import Data.Article.Slug as Slug exposing (Slug)
import Data.AuthToken exposing (AuthToken, withAuthorization)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Time
import Util exposing (apiUrl)



-- TYPES


type Comment
    = Comment CommentRecord


type alias CommentRecord =
    { id : CommentId
    , body : String
    , createdAt : Time.Posix
    , author : Author
    }



-- ACCESS


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


list : Maybe AuthToken -> Slug -> Http.Request (List Comment)
list maybeToken slug =
    apiUrl ("/articles/" ++ Slug.toString slug ++ "/comments")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "comments" (Decode.list Comment.decoder)))
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- POST


post : Slug -> String -> AuthToken -> Http.Request Comment
post slug body token =
    apiUrl ("/articles/" ++ Slug.toString slug ++ "/comments")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.jsonBody (encodeCommentBody body))
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "comment" Comment.decoder))
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest


encodeCommentBody : String -> Value
encodeCommentBody body =
    Encode.object [ ( "comment", Encode.object [ ( "body", Encode.string body ) ] ) ]



-- DELETE


delete : Slug -> CommentId -> AuthToken -> Http.Request ()
delete slug commentId token =
    apiUrl ("/articles/" ++ Slug.toString slug ++ "/comments/" ++ Comment.idToString commentId)
        |> HttpBuilder.delete
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest



-- INTERNAL


decoder : Decoder Comment
decoder =
    Decode.succeed CommentRecord
        |> required "id" CommentId.decoder
        |> required "body" Decode.string
        |> required "createdAt" Util.dateStringDecoder
        |> required "author" Author.decoder
