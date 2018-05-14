module Request.Profile exposing (get, toggleFollow)

import Data.AuthToken exposing (AuthToken, withAuthorization)
import Data.Profile as Profile exposing (Profile)
import Data.User as User
import Data.User.Username as Username exposing (Username)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Decode as Decode
import Request.Helpers exposing (apiUrl)



-- GET --


get : Username -> Maybe AuthToken -> Http.Request Profile
get username maybeToken =
    apiUrl ("/profiles/" ++ Username.toString username)
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
    [ apiUrl "/profiles", Username.toString username, "follow" ]
        |> String.join "/"
        |> builderFromUrl
        |> withAuthorization (Just token)
        |> withExpect (Http.expectJson (Decode.field "profile" Profile.decoder))
        |> HttpBuilder.toRequest
