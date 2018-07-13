module Article.Comment
    exposing
        ( Comment
        , author
        , body
        , createdAt
        , delete
        , id
        , list
        , post
        )

import Article exposing (Article)
import Article.Slug as Slug exposing (Slug)
import AuthToken exposing (AuthToken, withAuthorization)
import CommentId exposing (CommentId)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Time
import Util exposing (apiUrl)



-- TYPES


type Comment
    = Comment CommentRecord


type alias CommentRecord =
    { id : CommentId
    , body : String
    , createdAt : Time.Posix
    , author : Profile
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


author : Comment -> Profile
author (Comment comment) =
    comment.author



-- LIST


list : Maybe AuthToken -> Slug -> Http.Request (List Comment)
list maybeToken slug =
    apiUrl ("/articles/" ++ Slug.toString slug ++ "/comments")
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "comments" (Decode.list decoder)))
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- POST


post : Slug -> String -> AuthToken -> Http.Request Comment
post articleSlug commentBody token =
    apiUrl ("/articles/" ++ Slug.toString articleSlug ++ "/comments")
        |> HttpBuilder.post
        |> HttpBuilder.withBody (Http.jsonBody (encodeCommentBody commentBody))
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "comment" decoder))
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest


encodeCommentBody : String -> Value
encodeCommentBody str =
    Encode.object [ ( "comment", Encode.object [ ( "body", Encode.string str ) ] ) ]



-- DELETE


delete : Slug -> CommentId -> AuthToken -> Http.Request ()
delete slug commentId token =
    apiUrl ("/articles/" ++ Slug.toString slug ++ "/comments/" ++ CommentId.toString commentId)
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
        |> required "author" Profile.decoder
        |> Decode.map Comment
