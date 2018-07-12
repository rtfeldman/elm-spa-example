module Me exposing (Me, bio, edit, email, following, image, login, profile, register, toggleFollow, username)

{-| The currently signed-in user.

This is used for things like login, logout, and settings.

Contrast with Profile, which is a user whose profile you're viewing.

-}


type Me
    = Me MeRecord


type alias MeRecord =
    { username : Username
    , bio : Maybe String
    , image : UserPhoto
    , email : String
    }



-- ACCESS


username : Me -> Username
username (User user) =
    user.username


bio : Me -> Maybe String
bio (User user) =
    user.bio


image : Me -> UserPhoto
image (User user) =
    user.image


email : Me -> String
email (User user) =
    let
        (Me info) =
            user.extraInfo
    in
    info.email


token : Me -> AuthToken
token (User user) =
    let
        (Me info) =
            user.extraInfo
    in
    info.token



-- SESSION MANAGEMENT


login : { r | email : String, password : String } -> Http.Request ( Me, AuthToken )
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
    Decode.field "user" meAndTokenDecoder
        |> Http.post (apiUrl "/users/login") body



-- REGISTER


register : { r | username : String, email : String, password : String } -> Http.Request ( Me, AuthToken )
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
    Decode.field "user" meAndTokenDecoder
        |> Http.post (apiUrl "/users") body



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
    -> Http.Request Me
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



-- SERIALIZATION


decoder : Decoder Me
decoder =
    Decode.succeed MeRecord
        |> required "username" Username.decoder
        |> required "bio" (Decode.nullable Decode.string)
        |> required "image" UserPhoto.decoder
        |> required "email" Decode.string
        |> Decode.map Me


meAndTokenDecoder : Decoder ( Me, AuthToken )
meAndTokenDecoder =
    Decode.succeed Tuple.pair
        |> custom decoder
        |> required "token" AuthToken.decoder
