module Page.Article.Editor exposing (Model, Msg, initEdit, initNew, update, view)

import Data.Article as Article exposing (Article, Body)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, defaultValue, disabled, href, id, placeholder, type_)
import Html.Events exposing (onInput, onSubmit)
import Http
import Page.Errored as Errored exposing (PageLoadError, pageLoadError)
import Request.Article
import Route
import Task exposing (Task)
import Util exposing ((=>), pair, viewIf)
import Validate exposing (ifBlank)
import Views.Form as Form
import Views.Page as Page


-- MODEL --


type alias Model =
    { errors : List Error
    , editingArticle : Maybe Article.Slug
    , title : String
    , body : String
    , description : String
    , tags : List String
    }


initNew : Model
initNew =
    { errors = []
    , editingArticle = Nothing
    , title = ""
    , body = ""
    , description = ""
    , tags = []
    }


initEdit : Session -> Article.Slug -> Task PageLoadError Model
initEdit session slug =
    let
        maybeAuthToken =
            session.user
                |> Maybe.map .token
    in
    Request.Article.get maybeAuthToken slug
        |> Http.toTask
        |> Task.mapError (\_ -> pageLoadError Page.Other "Article is currently unavailable.")
        |> Task.map
            (\article ->
                { errors = []
                , editingArticle = Just slug
                , title = article.title
                , body = Article.bodyToMarkdownString article.body
                , description = article.description
                , tags = article.tags
                }
            )



-- VIEW --


view : Model -> Html Msg
view model =
    div [ class "editor-page" ]
        [ div [ class "container page" ]
            [ div [ class "row" ]
                [ div [ class "col-md-10 offset-md-1 col-xs-12" ]
                    [ Form.viewErrors model.errors
                    , viewForm model
                    ]
                ]
            ]
        ]


viewForm : Model -> Html Msg
viewForm model =
    let
        isEditing =
            model.editingArticle /= Nothing

        saveButtonText =
            if isEditing then
                "Update Article"
            else
                "Publish Article"
    in
    Html.form [ onSubmit Save ]
        [ fieldset []
            [ Form.input
                [ class "form-control-lg"
                , placeholder "Article Title"
                , onInput SetTitle
                , defaultValue model.title
                ]
                []
            , Form.input
                [ placeholder "What's this article about?"
                , onInput SetDescription
                , defaultValue model.description
                ]
                []
            , Form.textarea
                [ placeholder "Write your article (in markdown)"
                , attribute "rows" "8"
                , onInput SetBody
                , defaultValue model.body
                ]
                []
            , Form.input
                [ placeholder "Enter tags"
                , onInput SetTags
                , defaultValue (String.join " " model.tags)
                ]
                []
            , button [ class "btn btn-lg pull-xs-right btn-primary" ]
                [ text saveButtonText ]
            ]
        ]



-- UPDATE --


type Msg
    = Save
    | SetTitle String
    | SetDescription String
    | SetTags String
    | SetBody String
    | CreateCompleted (Result Http.Error (Article Body))
    | EditCompleted (Result Http.Error (Article Body))


update : User -> Msg -> Model -> ( Model, Cmd Msg )
update user msg model =
    case msg of
        Save ->
            case validate model of
                [] ->
                    case model.editingArticle of
                        Nothing ->
                            user.token
                                |> Request.Article.create model
                                |> Http.send CreateCompleted
                                |> pair { model | errors = [] }

                        Just slug ->
                            user.token
                                |> Request.Article.update slug model
                                |> Http.send EditCompleted
                                |> pair { model | errors = [] }

                errors ->
                    { model | errors = errors } => Cmd.none

        SetTitle title ->
            { model | title = title } => Cmd.none

        SetDescription description ->
            { model | description = description } => Cmd.none

        SetTags tags ->
            { model | tags = tagsFromString tags } => Cmd.none

        SetBody body ->
            { model | body = body } => Cmd.none

        CreateCompleted (Ok article) ->
            Route.Article article.slug
                |> Route.modifyUrl
                |> pair model

        CreateCompleted (Err error) ->
            { model | errors = model.errors ++ [ Form => "Server error while attempting to publish article" ] }
                => Cmd.none

        EditCompleted (Ok article) ->
            Route.Article article.slug
                |> Route.modifyUrl
                |> pair model

        EditCompleted (Err error) ->
            { model | errors = model.errors ++ [ Form => "Server error while attempting to save article" ] }
                => Cmd.none



-- VALIDATION --


type Field
    = Form
    | Title
    | Body


type alias Error =
    ( Field, String )


validate : Model -> List Error
validate =
    Validate.all
        [ .title >> ifBlank (Title => "title can't be blank.")
        , .body >> ifBlank (Body => "body can't be blank.")
        ]



-- INTERNAL --


tagsFromString : String -> List String
tagsFromString str =
    str
        |> String.split " "
        |> List.map String.trim
        |> List.filter (not << String.isEmpty)


redirectToArticle : Article.Slug -> Cmd msg
redirectToArticle =
    Route.modifyUrl << Route.Article
