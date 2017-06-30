module Request.Profile exposing (get, toggleFollow)

import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.Profile as Profile exposing (Profile)
import Data.User as User exposing (Username)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect, withQueryParams)
import Json.Decode as Decode
import Request.Helpers exposing (apiUrl)


-- GET --


get : Username -> Maybe AuthToken -> Http.Request Profile
get username maybeToken =
    apiUrl ("/profiles/" ++ User.usernameToString username)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "profile" Profile.decoder))
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- FOLLOWING --


toggleFollow : Username -> Bool -> AuthToken -> Http.Request Profile
toggleFollow username following authToken =
    if following then
        unfollow username authToken
    else
        follow username authToken


follow : Username -> AuthToken -> Http.Request Profile
follow =
    buildFollow HttpBuilder.post


unfollow : Username -> AuthToken -> Http.Request Profile
unfollow =
    buildFollow HttpBuilder.delete


buildFollow :
    (String -> RequestBuilder a)
    -> Username
    -> AuthToken
    -> Http.Request Profile
buildFollow builderFromUrl username token =
    [ apiUrl "/profiles", User.usernameToString username, "follow" ]
        |> String.join "/"
        |> builderFromUrl
        |> withAuthorization (Just token)
        |> withExpect (Http.expectJson (Decode.field "profile" Profile.decoder))
        |> HttpBuilder.toRequest
