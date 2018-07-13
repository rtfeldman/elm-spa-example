module Views.Page exposing (ActivePage(..), frame)

{-| The frame around a typical page - that is, the header and footer.
-}

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Me exposing (Me)
import Profile exposing (Profile)
import Route exposing (Route)
import UserPhoto exposing (UserPhoto)
import Username exposing (Username)
import Util
import Views.Spinner exposing (spinner)


{-| Determines which navbar link (if any) will be rendered as active.

Note that we don't enumerate every page here, because the navbar doesn't
have links for every page. Anything that's not part of the navbar falls
under Other.

-}
type ActivePage
    = Other
    | Home
    | Login
    | Register
    | Settings
    | Profile Username
    | NewArticle


{-| Take a page's Html and frame it with a header and footer.

The caller provides the current user, so we can display in either
"signed in" (rendering username) or "signed out" mode.

isLoading is for determining whether we should show a loading spinner
in the header. (This comes up during slow page transitions.)

-}
frame : Bool -> Maybe Me -> ActivePage -> { title : String, content : Html msg } -> Document msg
frame isLoading maybeMe page { title, content } =
    { title = title ++ " — Conduit"
    , body =
        [ viewHeader page maybeMe isLoading
        , content
        , viewFooter
        ]
    }


viewHeader : ActivePage -> Maybe Me -> Bool -> Html msg
viewHeader page user isLoading =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ text "conduit" ]
            , ul [ class "nav navbar-nav pull-xs-right" ] <|
                (if isLoading then
                    spinner

                 else
                    text ""
                )
                    :: navbarLink page Route.Home [ text "Home" ]
                    :: viewSignIn page user
            ]
        ]


viewSignIn : ActivePage -> Maybe Me -> List (Html msg)
viewSignIn page maybeMe =
    let
        linkTo =
            navbarLink page
    in
    case maybeMe of
        Nothing ->
            [ linkTo Route.Login [ text "Sign in" ]
            , linkTo Route.Register [ text "Sign up" ]
            ]

        Just me ->
            [ linkTo Route.NewArticle [ i [ class "ion-compose" ] [], text " New Post" ]
            , linkTo Route.Settings [ i [ class "ion-gear-a" ] [], text " Settings" ]
            , linkTo
                (Route.Profile (Me.username me))
                [ img [ class "user-pic", UserPhoto.src (Me.image me) ] []
                , Username.toHtml (Me.username me)
                ]
            , linkTo Route.Logout [ text "Sign out" ]
            ]


viewFooter : Html msg
viewFooter =
    footer []
        [ div [ class "container" ]
            [ a [ class "logo-font", href "/" ] [ text "conduit" ]
            , span [ class "attribution" ]
                [ text "An interactive learning project from "
                , a [ href "https://thinkster.io" ] [ text "Thinkster" ]
                , text ". Code & design licensed under MIT."
                ]
            ]
        ]


navbarLink : ActivePage -> Route -> List (Html msg) -> Html msg
navbarLink page route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive page route ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


isActive : ActivePage -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Home, Route.Home ) ->
            True

        ( Login, Route.Login ) ->
            True

        ( Register, Route.Register ) ->
            True

        ( Settings, Route.Settings ) ->
            True

        ( Profile pageUsername, Route.Profile routeUsername ) ->
            pageUsername == routeUsername

        ( NewArticle, Route.NewArticle ) ->
            True

        _ ->
            False
