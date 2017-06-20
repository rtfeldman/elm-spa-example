module Views.User.Follow exposing (State, button)

{-| The Follow button.

This API accepts a "toggle follow" message and the current state of whether
the user is already being followed. It's very lightweight!

It would be overkill to give something this simple its own Model, Msg, and
update. That would make it way more work to use than it needed to be,
and for no benefit.

-}

import Data.User as User exposing (Username)
import Html exposing (Html, i, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)


type alias State record =
    { record | following : Bool, username : Username }


button : (Username -> msg) -> State record -> Html msg
button toggleFollow { following, username } =
    let
        ( prefix, secondaryClass ) =
            if following then
                ( "Unfollow", "btn-secondary" )
            else
                ( "Follow", "btn-outline-secondary" )

        classes =
            [ "btn", "btn-sm", secondaryClass, "action-btn" ]
                |> String.join " "
                |> class
    in
    Html.button [ classes, onClick (toggleFollow username) ]
        [ i [ class "ion-plus-round" ] []
        , text (" " ++ prefix ++ " " ++ User.usernameToString username)
        ]
