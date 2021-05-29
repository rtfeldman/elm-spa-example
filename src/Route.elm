module Route exposing (Route(..), fromUrl, href, replaceUrl)

import Article.Slug as Slug exposing (Slug)
import Browser.Navigation as Nav
import Html exposing (Attribute)
import Html.Attributes as Attr
import Profile exposing (Profile)
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, oneOf, s, string)
import Username exposing (Username)



-- ROUTING


type Route
    = Home
    | Root
    | Login
    | Logout
    | Register
    | Settings
    | Article Slug
    | Profile Username
    | NewArticle
    | EditArticle Slug


parser =
    oneOf
        [ Parser.map Home Parser.top
        , Parser.map Login (s "login")
        , Parser.map Logout (s "logout")
        , Parser.map Settings (s "settings")
        , Parser.map Profile (s "profile" </> Username.urlParser)
        , Parser.map Register (s "register")
        , Parser.map Article (s "article" </> Slug.urlParser)
        , Parser.map NewArticle (s "editor")
        , Parser.map EditArticle (s "editor" </> Slug.urlParser)
        ]



-- PUBLIC HELPERS


href targetRoute = Attr.href (routeToString targetRoute)


replaceUrl key route = Nav.replaceUrl key (routeToString route)


fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser



-- INTERNAL


routeToString page = "#/" ++ String.join "/" (routeToPieces page)


routeToPieces page =
    case page of
        Home -> []
        Root -> []
        Login -> [ "login" ]

        Logout -> [ "logout" ]

        Register ->
            [ "register" ]

        Settings ->
            [ "settings" ]

        Article slug ->
            [ "article", Slug.toString slug ]

        Profile username ->
            [ "profile", Username.toString username ]

        NewArticle ->
            [ "editor" ]

        EditArticle slug ->
            [ "editor", Slug.toString slug ]
