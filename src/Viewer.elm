module Viewer exposing (Viewer, avatar, cred, decoder, minPasswordChars, store, username)

{-| The logged-in user currently viewing this page. It stores enough data to
be able to render the menu bar (username and avatar), along with Cred so it's
impossible to have a Viewer if you aren't logged in.
-}

import Api exposing (Cred)
import Avatar exposing (Avatar)
import Email exposing (Email)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Username exposing (Username)



-- TYPES


type Viewer = Viewer Avatar Cred



-- INFO


cred (Viewer _ val) = val


username (Viewer _ val) = Api.username val


avatar (Viewer val _) = val


{-| Passwords must be at least this many characters long!
-}
minPasswordChars = 6



-- SERIALIZATION


decoder =
    Decode.succeed Viewer
        |> custom (Decode.field "image" Avatar.decoder)


store (Viewer avatarVal credVal) = Api.storeCredWith credVal avatarVal
