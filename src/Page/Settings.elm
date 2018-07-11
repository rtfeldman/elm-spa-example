module Page.Settings exposing (ExternalMsg(..), Model, Msg, init, update, view)

import Browser.Navigation as Nav
import Data.Session exposing (Session)
import Data.User as User exposing (User)
import Data.User.Photo as UserPhoto
import Data.User.Username as Username exposing (Username)
import Html exposing (Html, button, div, fieldset, h1, input, text, textarea)
import Html.Attributes exposing (attribute, class, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode exposing (Decoder, decodeString, field, list, string)
import Json.Decode.Pipeline exposing (optional)
import Request.User exposing (storeSession)
import Route
import Util exposing (pair)
import Validate exposing (Validator, ifBlank, validate)
import Views.Form as Form



-- MODEL --


type alias Model =
    { errors : List Error
    , image : Maybe String
    , email : String
    , bio : String
    , username : String
    , password : Maybe String
    }


init : User -> Model
init user =
    { errors = []
    , image = UserPhoto.toMaybeString user.image
    , email = user.email
    , bio = Maybe.withDefault "" user.bio
    , username = Username.toString user.username
    , password = Nothing
    }



-- VIEW --


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    { title = "Settings"
    , content =
        div [ class "settings-page" ]
            [ div [ class "container page" ]
                [ div [ class "row" ]
                    [ div [ class "col-md-6 offset-md-3 col-xs-12" ]
                        [ h1 [ class "text-xs-center" ] [ text "Your Settings" ]
                        , Form.viewErrors model.errors
                        , viewForm model
                        ]
                    ]
                ]
            ]
    }


viewForm : Model -> Html Msg
viewForm model =
    Html.form [ onSubmit SubmitForm ]
        [ fieldset []
            [ Form.input
                [ placeholder "URL of profile picture"
                , value (Maybe.withDefault "" model.image)
                , onInput SetImage
                ]
                []
            , Form.input
                [ class "form-control-lg"
                , placeholder "Username"
                , value model.username
                , onInput SetUsername
                ]
                []
            , Form.textarea
                [ class "form-control-lg"
                , placeholder "Short bio about you"
                , attribute "rows" "8"
                , value model.bio
                , onInput SetBio
                ]
                []
            , Form.input
                [ class "form-control-lg"
                , placeholder "Email"
                , value model.email
                , onInput SetEmail
                ]
                []
            , Form.password
                [ class "form-control-lg"
                , placeholder "Password"
                , value (Maybe.withDefault "" model.password)
                , onInput SetPassword
                ]
                []
            , button
                [ class "btn btn-lg btn-primary pull-xs-right" ]
                [ text "Update Settings" ]
            ]
        ]



-- UPDATE --


type Msg
    = SubmitForm
    | SetEmail String
    | SetUsername String
    | SetPassword String
    | SetBio String
    | SetImage String
    | SaveCompleted (Result Http.Error User)


type ExternalMsg
    = NoOp
    | SetUser User


update : Nav.Key -> Session -> Msg -> Model -> ( ( Model, Cmd Msg ), ExternalMsg )
update navKey session msg model =
    case msg of
        SubmitForm ->
            case validate modelValidator model of
                [] ->
                    ( session.user
                        |> Maybe.map .token
                        |> Request.User.edit model
                        |> Http.send SaveCompleted
                        |> pair { model | errors = [] }
                    , NoOp
                    )

                errors ->
                    ( ( { model | errors = errors }
                      , Cmd.none
                      )
                    , NoOp
                    )

        SetEmail email ->
            ( ( { model | email = email }
              , Cmd.none
              )
            , NoOp
            )

        SetUsername username ->
            ( ( { model | username = username }
              , Cmd.none
              )
            , NoOp
            )

        SetPassword passwordStr ->
            let
                password =
                    if String.isEmpty passwordStr then
                        Nothing

                    else
                        Just passwordStr
            in
            ( ( { model | password = password }
              , Cmd.none
              )
            , NoOp
            )

        SetBio bio ->
            ( ( { model | bio = bio }
              , Cmd.none
              )
            , NoOp
            )

        SetImage imageStr ->
            let
                image =
                    if String.isEmpty imageStr then
                        Nothing

                    else
                        Just imageStr
            in
            ( ( { model | image = image }
              , Cmd.none
              )
            , NoOp
            )

        SaveCompleted (Err error) ->
            let
                errorMessages =
                    case error of
                        Http.BadStatus response ->
                            response.body
                                |> decodeString (field "errors" errorsDecoder)
                                |> Result.withDefault []

                        _ ->
                            [ "unable to save changes" ]

                errors =
                    errorMessages
                        |> List.map (\errorMessage -> ( Form, errorMessage ))
            in
            ( ( { model | errors = errors }
              , Cmd.none
              )
            , NoOp
            )

        SaveCompleted (Ok user) ->
            ( ( model
              , Cmd.batch [ storeSession user, Route.replaceUrl navKey Route.Home ]
              )
            , SetUser user
            )



-- VALIDATION --


type Field
    = Form
    | Username
    | Email
    | Password
    | ImageUrl
    | Bio


type alias Error =
    ( Field, String )


modelValidator : Validator Error Model
modelValidator =
    Validate.all
        [ ifBlank .username ( Username, "username can't be blank." )
        , ifBlank .email ( Email, "email can't be blank." )
        ]


errorsDecoder : Decoder (List String)
errorsDecoder =
    Decode.succeed (\email username password -> List.concat [ email, username, password ])
        |> optionalError "email"
        |> optionalError "username"
        |> optionalError "password"


optionalError : String -> Decoder (List String -> a) -> Decoder a
optionalError fieldName =
    let
        errorToString errorMessage =
            String.join " " [ fieldName, errorMessage ]
    in
    optional fieldName (list (Decode.map errorToString string)) []
