module Profile exposing (Profile, bio, decoder, follow, following, get, image, toggleFollow, username)

{-| A user's profile - potentially your own!

Contrast with Me, which is the currently signed-in user.

-}

import AuthToken exposing (AuthToken, withAuthorization)
import Http
import HttpBuilder exposing (RequestBuilder, withExpect)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import UserPhoto exposing (UserPhoto)
import Username exposing (Username)
import Util exposing (apiUrl)



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
username (Profile info) =
    info.username


bio : Profile -> Maybe String
bio (Profile info) =
    info.bio


image : Profile -> UserPhoto
image (Profile info) =
    info.image


following : Profile -> Bool
following (Profile info) =
    info.following



-- PROFILE


get : Username -> Maybe AuthToken -> Http.Request Profile
get uname maybeToken =
    apiUrl ("/profiles/" ++ Username.toString uname)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "profile" decoder))
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- FOLLOWING --


follow : Bool -> Profile -> Profile
follow isFollowing (Profile info) =
    Profile { info | following = isFollowing }


toggleFollow : Username -> Bool -> AuthToken -> Http.Request Profile
toggleFollow uname isFollowing authToken =
    if isFollowing then
        requestUnfollow uname authToken

    else
        requestFollow uname authToken


requestFollow : Username -> AuthToken -> Http.Request Profile
requestFollow =
    buildFollow HttpBuilder.post


requestUnfollow : Username -> AuthToken -> Http.Request Profile
requestUnfollow =
    buildFollow HttpBuilder.delete


buildFollow :
    (String -> RequestBuilder a)
    -> Username
    -> AuthToken
    -> Http.Request Profile
buildFollow builderFromUrl uname token =
    [ apiUrl "/profiles", Username.toString uname, "follow" ]
        |> String.join "/"
        |> builderFromUrl
        |> withAuthorization (Just token)
        |> withExpect (Http.expectJson (Decode.field "profile" decoder))
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
