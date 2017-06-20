module Views.Page exposing (ActivePage(..), bodyId, frame)

{-| The frame around a typical page - that is, the header and footer.
-}

import Color
import Data.User as User exposing (User, Username)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Element exposing (el, row)
import Element.Attributes exposing (alignBottom, alignLeft, justify, paddingXY, spacing, spacingXY, verticalCenter)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Lazy exposing (lazy2)
import Route exposing (Route)
import Style exposing (..)
import Style.Border as Border
import Style.Color as Color
import Style.Font as Font
import Style.Transition as Transition
import Util exposing ((=>))
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
frame : Bool -> Maybe User -> ActivePage -> Html msg -> Html msg
frame isLoading user page content =
    div [ class "page-frame" ]
        [ viewHeader page user isLoading
        , content
        , viewFooter
        ]


viewHeader : ActivePage -> Maybe User -> Bool -> Html msg
viewHeader page user isLoading =
    nav [ class "navbar navbar-light" ]
        [ div [ class "container" ]
            [ a [ class "navbar-brand", Route.href Route.Home ]
                [ text "conduit" ]
            , ul [ class "nav navbar-nav pull-xs-right" ] <|
                lazy2 Util.viewIf isLoading spinner
                    :: navbarLink (page == Home) Route.Home [ text "Home" ]
                    :: viewSignIn page user
            ]
        ]


viewSignIn : ActivePage -> Maybe User -> List (Html msg)
viewSignIn page user =
    case user of
        Nothing ->
            [ navbarLink (page == Login) Route.Login [ text "Sign in" ]
            , navbarLink (page == Register) Route.Register [ text "Sign up" ]
            ]

        Just user ->
            [ navbarLink (page == NewArticle) Route.NewArticle [ i [ class "ion-compose" ] [], text " New Post" ]
            , navbarLink (page == Settings) Route.Settings [ i [ class "ion-gear-a" ] [], text " Settings" ]
            , navbarLink
                (page == Profile user.username)
                (Route.Profile user.username)
                [ img [ class "user-pic", UserPhoto.src user.image ] []
                , User.usernameToHtml user.username
                ]
            , navbarLink False Route.Logout [ text "Sign out" ]
            ]


viewFooter : Html msg
viewFooter =
    Element.root stylesheet <|
        row Footer
            [ verticalCenter, spacing 10, paddingXY 77.5 21 ]
            [ Element.link "/" <|
                el Logo [] (Element.text "conduit")
            , Element.textLayout Attribution
                []
                [ el None [ alignLeft ] (Element.text "An interactive learning project from ")
                , Element.link "https://thinkster.io" <|
                    el Link [ alignLeft ] (Element.text "Thinkster")
                , el None [ alignLeft ] (Element.text ". Code & design licensed under MIT.")
                ]
            ]


type Styles
    = None
    | Footer
    | Logo
    | Attribution
    | Link


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.stylesheet
        [ Style.style None []
        , Style.style Logo
            [ Color.text (Color.rgb 92 184 92)
            , Font.typeface [ "Titillium Web", "sans-serif" ]
            , Font.size 16
            ]
        , Style.style Footer
            [ Color.background (Color.rgb 243 243 243)
            , Font.typeface [ "Source Sans Pro", "sans-serif" ]
            , Font.size 12
            ]
        , Style.style Attribution
            [ Color.text (Color.rgb 187 187 187) ]
        , Style.style Link
            [ Color.text (Color.rgb 92 184 92)
            , cursor "pointer"
            , hover
                [ Color.text (Color.rgb 61 139 61)
                ]
            ]
        ]


navbarLink : Bool -> Route -> List (Html msg) -> Html msg
navbarLink isActive route linkContent =
    li [ classList [ ( "nav-item", True ), ( "active", isActive ) ] ]
        [ a [ class "nav-link", Route.href route ] linkContent ]


{-| This id comes from index.html.

The Feed uses it to scroll to the top of the page (by ID) when switching pages
in the pagination sense.

-}
bodyId : String
bodyId =
    "page-body"
