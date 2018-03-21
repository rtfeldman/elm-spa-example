module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Browser.Navigation as Nav
import Data.Article as Article
import Data.User as User exposing (Username)
import Html exposing (Attribute)
import Html.Attributes as Attr
import Url.Parser as Url exposing ((</>), Parser, Url, oneOf, s, string)


-- ROUTING --


type Route
    = Home
    | Root
    | Login
    | Logout
    | Register
    | Settings
    | Article Article.Slug
    | Profile Username
    | NewArticle
    | EditArticle Article.Slug


route : Parser (Route -> a) a
route =
    oneOf
        [ Url.map Home (s "")
        , Url.map Login (s "login")
        , Url.map Logout (s "logout")
        , Url.map Settings (s "settings")
        , Url.map Profile (s "profile" </> User.usernameParser)
        , Url.map Register (s "register")
        , Url.map Article (s "article" </> Article.slugParser)
        , Url.map NewArticle (s "editor")
        , Url.map EditArticle (s "editor" </> Article.slugParser)
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Root ->
                    []

                Login ->
                    [ "login" ]

                Logout ->
                    [ "logout" ]

                Register ->
                    [ "register" ]

                Settings ->
                    [ "settings" ]

                Article slug ->
                    [ "article", Article.slugToString slug ]

                Profile username ->
                    [ "profile", User.usernameToString username ]

                NewArticle ->
                    [ "editor" ]

                EditArticle slug ->
                    [ "editor", Article.slugToString slug ]
    in
    "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Route -> Cmd msg
replaceUrl =
    routeToString >> Nav.replaceUrl


fromUrl : Url -> Maybe Route
fromUrl url =
    case url.fragment of
        Nothing ->
            Just Root

        Just fragment ->
            fragmentToRoute fragment


fragmentToRoute : String -> Maybe Route
fragmentToRoute fragment =
    case Url.toUrl fragment of
        Nothing ->
            Nothing

        Just segments ->
            Url.parse route segments
