module Article
    exposing
        ( Article
        , addBody
        , author
        , body
        , create
        , createdAt
        , delete
        , description
        , favorited
        , favoritesCount
        , get
        , slug
        , tags
        , title
        , toggleFavorite
        , update
        , updatedAt
        )

{-| The interface to the Article data structure.

This includes:

  - The Article type itself
  - Ways to make HTTP requests to retrieve and modify articles
  - Ways to access parts of an article
  - Converting between various types

The constructor for Article is not exposed, and neither are its encoders and decoders. This means it's only possible to obtain an Article by
using the functions exposed in this module - the HTTP requests and such.

-}

import Article.Body as Body exposing (Body)
import Data.Article as Article exposing (Article, Body)
import Data.Article.Author as Author exposing (Author)
import Data.Article.Feed as Feed exposing (Feed)
import Data.Article.Slug as Slug exposing (Slug)
import Data.Article.Tag as Tag exposing (Tag)
import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.User as User
import Data.User.Username as Username exposing (Username)
import Html exposing (Attribute, Html)
import Http
import HttpBuilder exposing (RequestBuilder, withBody, withExpect, withQueryParams)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, hardcoded, required)
import Json.Encode as Encode
import Markdown
import Request.Helpers exposing (apiUrl)
import Time
import Url.Parser
import Util



-- TYPES


{-| An article, optionally with an article body.

To see the difference between { body : body } and { body : Maybe Body },
consider the difference between the "view individual article" page (which
renders one article, including its body) and the "article feed" -
which displays multiple articles, but without bodies.

This definition for `Article` means we can write:

viewArticle : Article Body -> Html msg
viewFeed : List (Article ()) -> Html msg

This indicates that `viewArticle` requires an article _with a `body` present_,
wereas `viewFeed` accepts articles with no bodies. (We could also have written
it as `List (Article a)` to specify that feeds can accept either articles that
have `body` present or not. Either work, given that feeds do not attempt to
read the `body` field from articles.)

This is an important distinction, because in Request.Article, the `feed`
function produces `List (Article ())` because the API does not return bodies.
Those articles are useful to the feed, but not to the individual article view.

-}
type Article a
    = Article
        { description : String
        , slug : Slug
        , title : String
        , tags : List String
        , createdAt : Time.Posix
        , updatedAt : Time.Posix
        , favorited : Bool
        , favoritesCount : Int
        , author : Author
        , body : a
        }



-- ACCESS


description : Article a -> String
description (Article article) =
    article.description


slug : Article a -> Slug
slug (Article article) =
    article.slug


title : Article a -> String
title (Article article) =
    article.title


tags : Article a -> List String
tags (Article article) =
    article.tags


createdAt : Article a -> Time.Posix
createdAt (Article article) =
    article.createdAt


updatedAt : Article a -> Time.Posix
updatedAt (Article article) =
    article.updatedAt


favorited : Article a -> Bool
favorited (Article article) =
    article.favorited


favoritesCount : Article a -> Int
favoritesCount (Article article) =
    article.favoritesCount


author : Article a -> Author
author (Article article) =
    article.author


body : Article Body -> Body
body (Article article) =
    article.body



-- SERIALIZATION


decoder : Decoder (Article ())
decoder =
    baseArticleDecoder
        |> hardcoded ()


decoderWithBody : Decoder (Article Body)
decoderWithBody =
    baseArticleDecoder
        |> required "body" Body.decoder


baseArticleDecoder : Decoder (a -> Article a)
baseArticleDecoder =
    Decode.succeed Article
        |> required "description" (Decode.map (Maybe.withDefault "") (Decode.nullable Decode.string))
        |> required "slug" Slug.decoder
        |> required "title" Decode.string
        |> required "tagList" (Decode.list Decode.string)
        |> required "createdAt" Util.dateStringDecoder
        |> required "updatedAt" Util.dateStringDecoder
        |> required "favorited" Decode.bool
        |> required "favoritesCount" Decode.int
        |> required "author" Author.decoder


addBody : Body -> Article () -> Article Body
addBody body article =
    { description = article.description
    , slug = article.slug
    , title = article.title
    , tags = article.tags
    , createdAt = article.createdAt
    , updatedAt = article.updatedAt
    , favorited = article.favorited
    , favoritesCount = article.favoritesCount
    , author = article.author
    , body = body
    }



-- SINGLE


get : Maybe AuthToken -> Slug -> Http.Request (Article Body)
get maybeToken slug =
    let
        expect =
            Article.decoderWithBody
                |> Decode.field "article"
                |> Http.expectJson
    in
    apiUrl ("/articles/" ++ Slug.toString slug)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect expect
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- FAVORITE


toggleFavorite : Article a -> AuthToken -> Http.Request (Article ())
toggleFavorite article authToken =
    if article.favorited then
        unfavorite article.slug authToken

    else
        favorite article.slug authToken


favorite : Slug -> AuthToken -> Http.Request (Article ())
favorite =
    buildFavorite HttpBuilder.post


unfavorite : Slug -> AuthToken -> Http.Request (Article ())
unfavorite =
    buildFavorite HttpBuilder.delete


buildFavorite :
    (String -> RequestBuilder a)
    -> Slug
    -> AuthToken
    -> Http.Request (Article ())
buildFavorite builderFromUrl slug token =
    let
        expect =
            Article.decoder
                |> Decode.field "article"
                |> Http.expectJson

        url =
            String.join "/"
                [ apiUrl "/articles"
                , Slug.toString slug
                , "favorite"
                ]
    in
    builderFromUrl url
        |> withAuthorization (Just token)
        |> withExpect expect
        |> HttpBuilder.toRequest



-- CREATE


type alias CreateConfig record =
    { record
        | title : String
        , description : String
        , body : String
        , tags : List String
    }


type alias EditConfig record =
    { record
        | title : String
        , description : String
        , body : String
    }


create : CreateConfig record -> AuthToken -> Http.Request (Article Body)
create config token =
    let
        expect =
            Article.decoderWithBody
                |> Decode.field "article"
                |> Http.expectJson

        article =
            Encode.object
                [ ( "title", Encode.string config.title )
                , ( "description", Encode.string config.description )
                , ( "body", Encode.string config.body )
                , ( "tagList", Encode.list Encode.string config.tags )
                ]

        body =
            Encode.object [ ( "article", article ) ]
                |> Http.jsonBody
    in
    apiUrl "/articles"
        |> HttpBuilder.post
        |> withAuthorization (Just token)
        |> withBody body
        |> withExpect expect
        |> HttpBuilder.toRequest



-- UPDATE


update : Slug -> EditConfig record -> AuthToken -> Http.Request (Article Body)
update slug config token =
    let
        expect =
            Article.decoderWithBody
                |> Decode.field "article"
                |> Http.expectJson

        article =
            Encode.object
                [ ( "title", Encode.string config.title )
                , ( "description", Encode.string config.description )
                , ( "body", Encode.string config.body )
                ]

        body =
            Encode.object [ ( "article", article ) ]
                |> Http.jsonBody
    in
    apiUrl ("/articles/" ++ Slug.toString slug)
        |> HttpBuilder.put
        |> withAuthorization (Just token)
        |> withBody body
        |> withExpect expect
        |> HttpBuilder.toRequest



-- DELETE


delete : Slug -> AuthToken -> Http.Request ()
delete slug token =
    apiUrl ("/articles/" ++ Slug.toString slug)
        |> HttpBuilder.delete
        |> withAuthorization (Just token)
        |> HttpBuilder.toRequest
