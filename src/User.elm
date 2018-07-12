port module User exposing (Me, Profile, User, bio, edit, email, following, image, login, profile, register, storeSession, toggleFollow, username)

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


type User a
    = User (UserRecord a)


{-| A User Profile is one we can follow. We can ask whether we're following
this user.
-}
type Profile
    = Following Bool


{-| A `User Me` is the current user. We can get their email and auth token.
-}
type Me
    = Me MyInfo


type alias UserRecord extraInfo =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , extraInfo : extraInfo
    }


type alias MyInfo =
    { email : String
    , token : AuthToken
    }



-- ACCESS


username : User a -> Username
username (User user) =
    user.username


bio : User a -> Maybe String
bio (User user) =
    user.bio


image : User a -> UserPhoto
image (User user) =
    user.image


following : User Profile -> Bool
following (User user) =
    let
        (Following val) =
            user.extraInfo
    in
    val


email : User Me -> String
email (User user) =
    let
        (Me info) =
            user.extraInfo
    in
    info.email


token : User Me -> AuthToken
token (User user) =
    let
        (Me info) =
            user.extraInfo
    in
    info.token



-- SESSION MANAGEMENT


storeSession : User Me -> Cmd msg
storeSession me =
    encodeMe me
        |> Encode.encode 0
        |> Just
        |> Ports.sendSession


port sendSession : Maybe String -> Cmd msg


login : { r | email : String, password : String } -> Http.Request (User Me)
login { email, password } =
    let
        user =
            Encode.object
                [ ( "email", Encode.string email )
                , ( "password", Encode.string password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Decode.field "user" User.selfDecoder
        |> Http.post (apiUrl "/users/login") body



-- REGISTER


register : { r | username : String, email : String, password : String } -> Http.Request (User Me)
register { username, email, password } =
    let
        user =
            Encode.object
                [ ( "username", Encode.string username )
                , ( "email", Encode.string email )
                , ( "password", Encode.string password )
                ]

        body =
            Encode.object [ ( "user", user ) ]
                |> Http.jsonBody
    in
    Decode.field "user" User.selfDecoder
        |> Http.post (apiUrl "/users") body



-- PROFILE


profile : Username -> Maybe AuthToken -> Http.Request (User Profile)
profile username maybeToken =
    apiUrl ("/profiles/" ++ Username.toString username)
        |> HttpBuilder.get
        |> HttpBuilder.withExpect (Http.expectJson (Decode.field "profile" profileDecoder))
        |> withAuthorization maybeToken
        |> HttpBuilder.toRequest



-- EDIT


edit :
    { r
        | username : String
        , email : String
        , bio : String
        , password : Maybe String
        , image : Maybe String
    }
    -> AuthToken
    -> Http.Request (User Me)
edit { username, email, bio, password, image } authToken =
    let
        updates =
            [ Just ( "username", Encode.string username )
            , Just ( "email", Encode.string email )
            , Just ( "bio", Encode.string bio )
            , Just ( "image", Maybe.withDefault Encode.null (Maybe.map Encode.string image) )
            , Maybe.map (\pass -> ( "password", Encode.string pass )) password
            ]
                |> List.filterMap identity

        body =
            ( "user", Encode.object updates )
                |> List.singleton
                |> Encode.object
                |> Http.jsonBody

        expect =
            User.selfDecoder
                |> Decode.field "user"
                |> Http.expectJson
    in
    apiUrl "/user"
        |> HttpBuilder.put
        |> HttpBuilder.withExpect expect
        |> HttpBuilder.withBody body
        |> withAuthorization (Just authToken)
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


encodeMe : User Me -> Value
encodeMe me =
    Encode.object
        [ ( "email", Encode.string (email me) )
        , ( "token", AuthToken.encode (token me) )
        , ( "username", Username.encode (username me) )
        , ( "bio", Maybe.withDefault Encode.null (Maybe.map Encode.string (bio me)) )
        , ( "image", UserPhoto.encode (image me) )
        ]


{-| A user we are potentially following.
-}
profileDecoder : Decoder (User Profile)
profileDecoder =
    partialDecoder
        |> required "following" (Decode.map Following Decode.bool)
        |> Decode.map User


{-| Used when storing the current user's info - e.g. signing in, signing out,
editing them on the Settings page, etc. This User has both email and authToken.
-}
meDecoder : Decoder (User Me)
meDecoder =
    partialDecoder
        |> custom selfInfoDecoder
        |> Decode.map User


myInfoDecoder : Decoder MyInfo
myInfoDecoder =
    Decode.succeed MyInfo
        |> required "email" Decode.string
        |> required "token" AuthToken.decoder


{-| A user where we don't know their email or whether we follow them.
-}
decoder : Decoder (User ())
decoder =
    partialDecoder
        |> hardcoded ()
        |> Decode.map User


partialDecoder : Decoder (a -> UserRecord a)
partialDecoder =
    Decode.succeed UserRecord
        |> required "username" Username.decoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
