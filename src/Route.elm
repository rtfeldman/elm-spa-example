module Route exposing (Route(..), fromUrl, href, matchArticle, matchEditor, matchHome, matchLogin, matchLogout, matchProfile, matchRegister, matchSettings, replaceUrl, toString)

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


parser : Parser (Route -> a) a
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


href : Route -> Attribute msg
href targetRoute =
    Attr.href (routeToString targetRoute)


replaceUrl : Nav.Key -> Route -> Cmd msg
replaceUrl key route =
    Nav.replaceUrl key (routeToString route)


fromUrl : Url -> Maybe Route
fromUrl url =
    -- The RealWorld spec treats the fragment like a path.
    -- This makes it *literally* the path, so we can proceed
    -- with parsing as if it had been a normal path all along.
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
        |> Parser.parse parser


toString : Route -> String
toString =
    routeToString



-- INTERNAL


routeToString : Route -> String
routeToString page =
    "#/" ++ String.join "/" (routeToPieces page)


routeToPieces : Route -> List String
routeToPieces page =
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
            [ "article", Slug.toString slug ]

        Profile username ->
            [ "profile", Username.toString username ]

        NewArticle ->
            [ "editor" ]

        EditArticle slug ->
            [ "editor", Slug.toString slug ]



-- Route Matchers


matchBasic : Route -> Route -> Maybe ()
matchBasic route value =
    if value == route then
        Just ()

    else
        Nothing


matchHome : Route -> Maybe ()
matchHome =
    matchBasic Home


matchSettings : Route -> Maybe ()
matchSettings =
    matchBasic Settings


matchLogin : Route -> Maybe ()
matchLogin =
    matchBasic Login


matchProfile : Route -> Maybe Username
matchProfile route =
    case route of
        Profile username ->
            Just username

        _ ->
            Nothing


matchRegister : Route -> Maybe ()
matchRegister =
    matchBasic Register


matchArticle : Route -> Maybe Slug
matchArticle route =
    case route of
        Article slug ->
            Just slug

        _ ->
            Nothing


matchEditor : Route -> Maybe (Maybe Slug)
matchEditor route =
    case route of
        NewArticle ->
            Just Nothing

        EditArticle slug ->
            Just (Just slug)

        _ ->
            Nothing


matchLogout : Route -> Maybe ()
matchLogout =
    matchBasic Logout
