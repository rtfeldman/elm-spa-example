module Views.Author exposing (view)

{-| View an author. We basically render their username and a link to their
profile, and that's it.
-}

import Data.User as User exposing (Username)
import Html exposing (Html, a)
import Html.Attributes exposing (attribute, class, href, id, placeholder)
import Route exposing (Route)


view : Username -> Html msg
view username =
    a [ class "author", Route.href (Route.Profile username) ]
        [ User.usernameToHtml username ]
