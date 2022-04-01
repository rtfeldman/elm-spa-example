module Page.Article.Editor exposing (Model, Msg, page)

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Article exposing (Article, Full)
import Article.Body exposing (Body)
import Article.Slug as Slug exposing (Slug)
import Browser.Navigation as Nav
import Effect exposing (Effect)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Loading
import Page
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Spa.Page
import Task exposing (Task)
import Time
import View exposing (View)


page session =
    Spa.Page.element
        { init = init session
        , update = update session
        , subscriptions = subscriptions
        , view = view session
        }



-- MODEL


type alias Model =
    { status : Status
    }


type
    Status
    -- Edit Article
    = Loading Slug
    | LoadingSlowly Slug
    | LoadingFailed Slug
    | Saving Slug Form
    | Editing Slug (List Problem) Form
      -- New Article
    | EditingNew (List Problem) Form
    | Creating Form


type Problem
    = InvalidEntry ValidatedField String
    | ServerError String


type alias Form =
    { title : String
    , body : String
    , description : String
    , tags : String
    }


init : Session -> Maybe Slug -> ( Model, Effect Session.Msg Msg )
init session slug =
    case slug of
        Just s ->
            initEdit session s

        Nothing ->
            initNew session


initNew : Session -> ( Model, Effect Session.Msg Msg )
initNew session =
    ( { status =
            EditingNew []
                { title = ""
                , body = ""
                , description = ""
                , tags = ""
                }
      }
    , Effect.none
    )


initEdit : Session -> Slug -> ( Model, Effect Session.Msg Msg )
initEdit session slug =
    ( { status = Loading slug
      }
    , Effect.batch
        [ Article.fetch (Session.cred session) slug
            |> Http.toTask
            -- If init fails, store the slug that failed in the msg, so we can
            -- at least have it later to display the page's title properly!
            |> Task.mapError (\httpError -> ( slug, httpError ))
            |> Effect.attempt CompletedArticleLoad
        , Effect.perform (\_ -> PassedSlowLoadThreshold) Loading.slowThreshold
        ]
    )



-- VIEW


view : Session -> Model -> View Msg
view session model =
    { title =
        case getSlug model.status of
            Just slug ->
                "Edit Article - " ++ Slug.toString slug

            Nothing ->
                "New Article"
    , page =
        case getSlug model.status of
            Just slug ->
                Page.Other

            Nothing ->
                Page.NewArticle
    , content =
        case Session.cred session of
            Just cred ->
                viewAuthenticated cred model

            Nothing ->
                text "Sign in to edit this article."
    }


viewProblems : List Problem -> Html msg
viewProblems problems =
    ul [ class "error-messages" ]
        (List.map viewProblem problems)


viewProblem : Problem -> Html msg
viewProblem problem =
    let
        errorMessage =
            case problem of
                InvalidEntry _ message ->
                    message

                ServerError message ->
                    message
    in
    li [] [ text errorMessage ]


viewAuthenticated : Cred -> Model -> Html Msg
viewAuthenticated cred model =
    let
        formHtml =
            case model.status of
                Loading _ ->
                    []

                LoadingSlowly _ ->
                    [ Loading.icon ]

                Saving slug form ->
                    [ viewForm cred form (editArticleSaveButton [ disabled True ]) ]

                Creating form ->
                    [ viewForm cred form (newArticleSaveButton [ disabled True ]) ]

                Editing slug problems form ->
                    [ viewProblems problems
                    , viewForm cred form (editArticleSaveButton [])
                    ]

                EditingNew problems form ->
                    [ viewProblems problems
                    , viewForm cred form (newArticleSaveButton [])
                    ]

                LoadingFailed _ ->
                    [ text "Article failed to load." ]
    in
    div [ class "editor-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
                    formHtml
                ]
            ]
        ]


viewForm : Cred -> Form -> Html Msg -> Html Msg
viewForm cred fields saveButton =
    Html.form [ onSubmit (ClickedSave cred) ]
        [ fieldset []
            [ fieldset [ class "form-group" ]
                [ input
                    [ class "form-control form-control-lg"
                    , placeholder "Article Title"
                    , onInput EnteredTitle
                    , value fields.title
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "What's this article about?"
                    , onInput EnteredDescription
                    , value fields.description
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ textarea
                    [ class "form-control"
                    , placeholder "Write your article (in markdown)"
                    , attribute "rows" "8"
                    , onInput EnteredBody
                    , value fields.body
                    ]
                    []
                ]
            , fieldset [ class "form-group" ]
                [ input
                    [ class "form-control"
                    , placeholder "Enter tags"
                    , onInput EnteredTags
                    , value fields.tags
                    ]
                    []
                ]
            , saveButton
            ]
        ]


editArticleSaveButton : List (Attribute msg) -> Html msg
editArticleSaveButton extraAttrs =
    saveArticleButton "Update Article" extraAttrs


newArticleSaveButton : List (Attribute msg) -> Html msg
newArticleSaveButton extraAttrs =
    saveArticleButton "Publish Article" extraAttrs


saveArticleButton : String -> List (Attribute msg) -> Html msg
saveArticleButton caption extraAttrs =
    button (class "btn btn-lg pull-xs-right btn-primary" :: extraAttrs)
        [ text caption ]



-- UPDATE


type Msg
    = ClickedSave Cred
    | EnteredBody String
    | EnteredDescription String
    | EnteredTags String
    | EnteredTitle String
    | CompletedCreate (Result Http.Error (Article Full))
    | CompletedEdit (Result Http.Error (Article Full))
    | CompletedArticleLoad (Result ( Slug, Http.Error ) (Article Full))
    | PassedSlowLoadThreshold


update : Session -> Msg -> Model -> ( Model, Effect Session.Msg Msg )
update session msg model =
    case msg of
        ClickedSave cred ->
            model.status
                |> save cred
                |> Tuple.mapFirst (\status -> { model | status = status })

        EnteredTitle title ->
            updateForm (\form -> { form | title = title }) model

        EnteredDescription description ->
            updateForm (\form -> { form | description = description }) model

        EnteredTags tags ->
            updateForm (\form -> { form | tags = tags }) model

        EnteredBody body ->
            updateForm (\form -> { form | body = body }) model

        CompletedCreate (Ok article) ->
            ( model
            , Route.Article (Article.slug article)
                |> Route.replaceUrl (Session.navKey session)
                |> Effect.fromCmd
            )

        CompletedCreate (Err error) ->
            ( { model | status = savingError error model.status }
            , Effect.none
            )

        CompletedEdit (Ok article) ->
            ( model
            , Route.Article (Article.slug article)
                |> Route.replaceUrl (Session.navKey session)
                |> Effect.fromCmd
            )

        CompletedEdit (Err error) ->
            ( { model | status = savingError error model.status }
            , Effect.none
            )

        CompletedArticleLoad (Err ( slug, error )) ->
            ( { model | status = LoadingFailed slug }
            , Effect.none
            )

        CompletedArticleLoad (Ok article) ->
            let
                { title, description, tags } =
                    Article.metadata article

                status =
                    Editing (Article.slug article)
                        []
                        { title = title
                        , body = Article.Body.toMarkdownString (Article.body article)
                        , description = description
                        , tags = String.join " " tags
                        }
            in
            ( { model | status = status }
            , Effect.none
            )

        PassedSlowLoadThreshold ->
            let
                -- If any data is still Loading, change it to LoadingSlowly
                -- so `view` knows to render a spinner.
                status =
                    case model.status of
                        Loading slug ->
                            LoadingSlowly slug

                        other ->
                            other
            in
            ( { model | status = status }, Effect.none )


save : Cred -> Status -> ( Status, Effect Session.Msg Msg )
save cred status =
    case status of
        Editing slug _ form ->
            case validate form of
                Ok validForm ->
                    ( Saving slug form
                    , edit slug validForm cred
                        |> Http.send CompletedEdit
                        |> Effect.fromCmd
                    )

                Err problems ->
                    ( Editing slug problems form
                    , Effect.none
                    )

        EditingNew _ form ->
            case validate form of
                Ok validForm ->
                    ( Creating form
                    , create validForm cred
                        |> Http.send CompletedCreate
                        |> Effect.fromCmd
                    )

                Err problems ->
                    ( EditingNew problems form
                    , Effect.none
                    )

        _ ->
            -- We're in a state where saving is not allowed.
            -- We tried to prevent getting here by disabling the Save
            -- button, but somehow the user got here anyway!
            --
            -- If we had an error logging service, we would send
            -- something to it here!
            ( status, Effect.none )


savingError : Http.Error -> Status -> Status
savingError error status =
    let
        problems =
            [ ServerError "Error saving article" ]
    in
    case status of
        Saving slug form ->
            Editing slug problems form

        Creating form ->
            EditingNew problems form

        _ ->
            status


{-| Helper function for `update`. Updates the form, if there is one,
and returns Effect.none.

Useful for recording form fields!

This could also log errors to the server if we are trying to record things in
the form and we don't actually have a form.

-}
updateForm : (Form -> Form) -> Model -> ( Model, Effect Session.Msg Msg )
updateForm transform model =
    let
        newModel =
            case model.status of
                Loading _ ->
                    model

                LoadingSlowly _ ->
                    model

                LoadingFailed _ ->
                    model

                Saving slug form ->
                    { model | status = Saving slug (transform form) }

                Editing slug errors form ->
                    { model | status = Editing slug errors (transform form) }

                EditingNew errors form ->
                    { model | status = EditingNew errors (transform form) }

                Creating form ->
                    { model | status = Creating (transform form) }
    in
    ( newModel, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- FORM


{-| Marks that we've trimmed the form's fields, so we don't accidentally send
it to the server without having trimmed it!
-}
type TrimmedForm
    = Trimmed Form


{-| When adding a variant here, add it to `fieldsToValidate` too!
-}
type ValidatedField
    = Title
    | Body


fieldsToValidate : List ValidatedField
fieldsToValidate =
    [ Title
    , Body
    ]


{-| Trim the form and validate its fields. If there are problems, report them!
-}
validate : Form -> Result (List Problem) TrimmedForm
validate form =
    let
        trimmedForm =
            trimFields form
    in
    case List.concatMap (validateField trimmedForm) fieldsToValidate of
        [] ->
            Ok trimmedForm

        problems ->
            Err problems


validateField : TrimmedForm -> ValidatedField -> List Problem
validateField (Trimmed form) field =
    List.map (InvalidEntry field) <|
        case field of
            Title ->
                if String.isEmpty form.title then
                    [ "title can't be blank." ]

                else
                    []

            Body ->
                if String.isEmpty form.body then
                    [ "body can't be blank." ]

                else
                    []


{-| Don't trim while the user is typing! That would be super annoying.
Instead, trim only on submit.
-}
trimFields : Form -> TrimmedForm
trimFields form =
    Trimmed
        { title = String.trim form.title
        , body = String.trim form.body
        , description = String.trim form.description
        , tags = String.trim form.tags
        }



-- HTTP


create : TrimmedForm -> Cred -> Http.Request (Article Full)
create (Trimmed form) cred =
    let
        article =
            Encode.object
                [ ( "title", Encode.string form.title )
                , ( "description", Encode.string form.description )
                , ( "body", Encode.string form.body )
                , ( "tagList", Encode.list Encode.string (tagsFromString form.tags) )
                ]

        body =
            Encode.object [ ( "article", article ) ]
                |> Http.jsonBody
    in
    Decode.field "article" (Article.fullDecoder (Just cred))
        |> Api.post (Endpoint.articles []) (Just cred) body


tagsFromString : String -> List String
tagsFromString str =
    String.split " " str
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)


edit : Slug -> TrimmedForm -> Cred -> Http.Request (Article Full)
edit articleSlug (Trimmed form) cred =
    let
        article =
            Encode.object
                [ ( "title", Encode.string form.title )
                , ( "description", Encode.string form.description )
                , ( "body", Encode.string form.body )
                ]

        body =
            Encode.object [ ( "article", article ) ]
                |> Http.jsonBody
    in
    Decode.field "article" (Article.fullDecoder (Just cred))
        |> Api.put (Endpoint.article articleSlug) cred body



-- INTERNAL


{-| Used for setting the page's title.
-}
getSlug : Status -> Maybe Slug
getSlug status =
    case status of
        Loading slug ->
            Just slug

        LoadingSlowly slug ->
            Just slug

        LoadingFailed slug ->
            Just slug

        Saving slug _ ->
            Just slug

        Editing slug _ _ ->
            Just slug

        EditingNew _ _ ->
            Nothing

        Creating _ ->
            Nothing
