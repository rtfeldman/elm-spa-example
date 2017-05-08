module Views.Author exposing (view)

{-| View an author. We basically render their username and a link to their
profile, and that's it.
-}

import Route exposing (Route)
import Html exposing (Html, a)
import Html.Attributes exposing (class, href, id, placeholder, attribute)
import Data.User as User exposing (Username)


view : Username -> Html msg
view username =
    a [ class "author", Route.href (Route.Profile username) ]
        [ User.usernameToHtml username ]
