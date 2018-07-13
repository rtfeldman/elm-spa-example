module Views.Author exposing (view)

{-| View an author. We basically render their username and a link to their
profile, and that's it.
-}

import Html exposing (Html, a)
import Html.Attributes exposing (attribute, class, href, id, placeholder)
import Route exposing (Route)
import Username exposing (Username)


view : Username -> Html msg
view username =
    a [ class "author", Route.href (Route.Profile username) ]
        [ Username.toHtml username ]
