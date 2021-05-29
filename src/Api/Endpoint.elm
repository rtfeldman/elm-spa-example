module Api.Endpoint exposing (Endpoint, article, articles, comment, comments, favorite, feed, follow, login, profiles, request, tags, user, users)

import Article.Slug as Slug exposing (Slug)
import CommentId exposing (CommentId)
import Http
import Url.Builder exposing (QueryParameter)
import Username exposing (Username)


{-| Http.request, except it takes an Endpoint instead of a Url.
-}
request config =
    Http.request
        { body = config.body
        , expect = config.expect
        , headers = config.headers
        , method = config.method
        , timeout = config.timeout
        , url = unwrap config.url
        , withCredentials = config.withCredentials
        }



-- TYPES


{-| Get a URL to the Conduit API.

This is not publicly exposed, because we want to make sure the only way to get one of these URLs is from this module.

-}
type Endpoint
    = Endpoint String


unwrap (Endpoint str) =
    str


url paths queryParams =
    -- NOTE: Url.Builder takes care of percent-encoding special URL characters.
    -- See https://package.elm-lang.org/packages/elm/url/latest/Url#percentEncode
    Url.Builder.crossOrigin "https://conduit.productionready.io"
        ("api" :: paths)
        queryParams
        |> Endpoint



-- ENDPOINTS


login =
    url [ "users", "login" ] []


user =
    url [ "user" ] []


users =
    url [ "users" ] []


follow uname =
    url [ "profiles", Username.toString uname, "follow" ] []



-- ARTICLE ENDPOINTS


article slug =
    url [ "articles", Slug.toString slug ] []


comments slug =
    url [ "articles", Slug.toString slug, "comments" ] []


comment slug commentId =
    url [ "articles", Slug.toString slug, "comments", CommentId.toString commentId ] []


favorite slug =
    url [ "articles", Slug.toString slug, "favorite" ] []


articles params =
    url [ "articles" ] params


profiles uname =
    url [ "profiles", Username.toString uname ] []


feed params =
    url [ "articles", "feed" ] params


tags =
    url [ "tags" ] []
