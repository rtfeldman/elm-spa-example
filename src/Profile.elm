module Profile exposing (Profile, bio, edit, email, following, image, login, profile, register, storeSession, toggleFollow, username)

import AuthToken exposing (AuthToken, withAuthorization)
import Html exposing (Html)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode exposing (Value)
import Ports
import Request.Helpers exposing (apiUrl)
import Url.Parser
import UserPhoto exposing (UserPhoto)
import Username exposing (Username)



-- TYPES


type Profile
    = Profile ProfileRecord


type alias ProfileRecord =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , following : Bool
    }



-- ACCESS


username : Profile -> Username
username (User profile) =
    profile.profilename


bio : Profile -> Maybe String
bio (User profile) =
    profile.bio


image : Profile -> UserPhoto
image (User profile) =
    profile.image


following : Profile -> Bool
following (Profile profile) =
    profile.following



-- PROFILE


profile : Username -> Maybe AuthToken -> Http.Request Profile
profile username maybeToken =
    apiUrl ("/profiles/" ++ Username.toString username)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "profile" profileDecoder))
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



-- SERIALIZATION


decoder : Decoder Profile
decoder =
    Decode.succeed ProfileRecord
        |> required "username" Username.decoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
        |> required "following" Decode.bool
        |> Decode.map Profile
