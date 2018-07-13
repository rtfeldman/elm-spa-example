module Page.Article exposing (Model, Msg, init, update, view)

{-| Viewing an individual article.
-}

import Article exposing (Article, Full)
import Article.Body
import Article.Comment as Comment exposing (Comment)
import Article.Slug as Slug exposing (Slug)
import AuthToken exposing (AuthToken)
import Browser.Navigation as Nav
import CommentId exposing (CommentId)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Me exposing (Me)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Profile exposing (Profile)
import Route
import Session exposing (Session)
import Task exposing (Task)
import Time
import UserPhoto
import Username exposing (Username)
import Util
import Views.Article
import Views.Article.Favorite as Favorite
import Views.Author
import Views.Errors
import Views.Follow as Follow
import Views.Page as Page



-- MODEL --


type alias Model =
    { errors : List String
    , commentText : String
    , commentInFlight : Bool
    , article : Article Full
    , comments : List Comment
    }


init : Maybe AuthToken -> Slug -> Task PageLoadError Model
init maybeToken slug =
    let
        loadArticle =
            Article.get maybeToken slug
                |> Http.toTask

        loadComments =
            Comment.list maybeToken slug
                |> Http.toTask

        handleLoadError _ =
            pageLoadError Page.Other "Article is currently unavailable."
    in
    Task.map2 (Model [] "" False) loadArticle loadComments
        |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    let
        maybeMe =
            Session.me session

        article =
            model.article

        author =
            Article.author article

        buttons =
            viewButtons article author maybeMe

        timeZone =
            Session.timeZone session

        postingDisabled =
            model.commentInFlight
    in
    { title = Article.title article
    , content =
        div [ class "article-page" ]
            [ viewBanner timeZone model.errors article author maybeMe
            , div [ class "container page" ]
                [ div [ class "row article-content" ]
                    [ div [ class "col-md-12" ]
                        [ Article.Body.toHtml (Article.body article) [] ]
                    ]
                , hr [] []
                , div [ class "article-actions" ]
                    [ div [ class "article-meta" ] <|
                        [ a [ Route.href (Route.Profile (Profile.username author)) ]
                            [ img [ UserPhoto.src (Profile.image author) ] [] ]
                        , div [ class "info" ]
                            [ Views.Author.view (Profile.username author)
                            , Views.Article.viewTimestamp timeZone article
                            ]
                        ]
                            ++ buttons
                    ]
                , div [ class "row" ]
                    [ div [ class "col-xs-12 col-md-8 offset-md-2" ] <|
                        viewAddComment postingDisabled maybeMe
                            :: List.map (viewComment timeZone maybeMe) model.comments
                    ]
                ]
            ]
    }


viewBanner : Time.Zone -> List String -> Article a -> Profile -> Maybe Me -> Html Msg
viewBanner timeZone errors article author maybeMe =
    let
        buttons =
            viewButtons article author maybeMe
    in
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [] [ text (Article.title article) ]
            , div [ class "article-meta" ] <|
                [ a [ Route.href (Route.Profile (Profile.username author)) ]
                    [ img [ UserPhoto.src (Profile.image author) ] [] ]
                , div [ class "info" ]
                    [ Views.Author.view (Profile.username author)
                    , Views.Article.viewTimestamp timeZone article
                    ]
                ]
                    ++ buttons
            , Views.Errors.view DismissErrors errors
            ]
        ]


viewAddComment : Bool -> Maybe Me -> Html Msg
viewAddComment postingDisabled maybeMe =
    case maybeMe of
        Nothing ->
            p []
                [ a [ Route.href Route.Login ] [ text "Sign in" ]
                , text " or "
                , a [ Route.href Route.Register ] [ text "sign up" ]
                , text " to add comments on this article."
                ]

        Just me ->
            Html.form [ class "card comment-form", onSubmit PostComment ]
                [ div [ class "card-block" ]
                    [ textarea
                        [ class "form-control"
                        , placeholder "Write a comment..."
                        , attribute "rows" "3"
                        , onInput SetCommentText
                        ]
                        []
                    ]
                , div [ class "card-footer" ]
                    [ img [ class "comment-author-img", UserPhoto.src (Me.image me) ] []
                    , button
                        [ class "btn btn-sm btn-primary"
                        , disabled postingDisabled
                        ]
                        [ text "Post Comment" ]
                    ]
                ]


viewButtons : Article a -> Profile -> Maybe Me -> List (Html Msg)
viewButtons article author maybeMe =
    let
        isMine =
            Maybe.map Me.username maybeMe == Just (Profile.username author)
    in
    if isMine then
        [ editButton article
        , text " "
        , deleteButton article
        ]

    else
        [ followButton author
        , text " "
        , favoriteButton article
        ]


viewComment : Time.Zone -> Maybe Me -> Comment -> Html Msg
viewComment timeZone maybeMe comment =
    let
        author =
            Comment.author comment

        authorUsername =
            Profile.username author

        isMine =
            Maybe.map Me.username maybeMe == Just authorUsername

        timestamp =
            Util.formatTimestamp timeZone (Comment.createdAt comment)
    in
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ p [ class "card-text" ] [ text (Comment.body comment) ] ]
        , div [ class "card-footer" ]
            [ a [ class "comment-author", href "" ]
                [ img [ class "comment-author-img", UserPhoto.src (Profile.image author) ] []
                , text " "
                ]
            , text " "
            , a [ class "comment-author", Route.href (Route.Profile authorUsername) ]
                [ text (Username.toString authorUsername) ]
            , span [ class "date-posted" ] [ text timestamp ]
            , if isMine then
                span
                    [ class "mod-options"
                    , onClick (DeleteComment (Comment.id comment))
                    ]
                    [ i [ class "ion-trash-a" ] [] ]

              else
                text ""
            ]
        ]



-- UPDATE --


type Msg
    = DismissErrors
    | ToggleFavorite
    | FavoriteCompleted (Result Http.Error (Article Full))
    | ToggleFollow
    | FollowCompleted (Result Http.Error Profile)
    | SetCommentText String
    | DeleteComment CommentId
    | CommentDeleted CommentId (Result Http.Error ())
    | PostComment
    | CommentPosted (Result Http.Error Comment)
    | DeleteArticle
    | ArticleDeleted (Result Http.Error ())


update : Nav.Key -> Session -> Msg -> Model -> ( Model, Cmd Msg )
update navKey session msg model =
    let
        article =
            model.article

        author =
            Article.author article

        oldBody =
            Article.body article
    in
    case msg of
        DismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        ToggleFavorite ->
            let
                cmdFromAuth authToken =
                    Article.toggleFavorite article authToken
                        |> Http.toTask
                        |> Task.map (Article.fromPreview oldBody)
                        |> Task.attempt FavoriteCompleted
            in
            session
                |> Session.attempt "favorite" cmdFromAuth
                |> Util.updateFromResult model Cmd.none

        FavoriteCompleted (Ok newArticle) ->
            ( { model | article = newArticle }, Cmd.none )

        FavoriteCompleted (Err error) ->
            -- In a serious production application, we would log the error to
            -- a logging service so we could investigate later.
            ( { model | errors = model.errors ++ [ "There was a server error trying to record your Favorite. Sorry!" ] }
            , Cmd.none
            )

        ToggleFollow ->
            let
                cmdFromAuth authToken =
                    authToken
                        |> Profile.toggleFollow
                            (Profile.username author)
                            (Profile.following author)
                        |> Http.send FollowCompleted
            in
            session
                |> Session.attempt "follow" cmdFromAuth
                |> Util.updateFromResult model Cmd.none

        FollowCompleted (Ok newAuthor) ->
            ( { model | article = Article.followAuthor (Profile.following newAuthor) article }, Cmd.none )

        FollowCompleted (Err error) ->
            ( { model | errors = "Unable to follow user." :: model.errors }, Cmd.none )

        SetCommentText commentText ->
            ( { model | commentText = commentText }, Cmd.none )

        PostComment ->
            let
                comment =
                    model.commentText
            in
            if model.commentInFlight || String.isEmpty comment then
                ( model, Cmd.none )

            else
                let
                    cmdFromAuth authToken =
                        authToken
                            |> Comment.post (Article.slug model.article) comment
                            |> Http.send CommentPosted
                in
                session
                    |> Session.attempt "post a comment" cmdFromAuth
                    |> Util.updateFromResult { model | commentInFlight = True } Cmd.none

        CommentPosted (Ok comment) ->
            ( { model
                | commentInFlight = False
                , comments = comment :: model.comments
              }
            , Cmd.none
            )

        CommentPosted (Err error) ->
            ( { model | errors = model.errors ++ [ "Server error while trying to post comment." ] }
            , Cmd.none
            )

        DeleteComment id ->
            let
                cmdFromAuth authToken =
                    authToken
                        |> Comment.delete (Article.slug model.article) id
                        |> Http.send (CommentDeleted id)
            in
            session
                |> Session.attempt "delete comments" cmdFromAuth
                |> Util.updateFromResult model Cmd.none

        CommentDeleted id (Ok ()) ->
            ( { model | comments = withoutComment id model.comments }
            , Cmd.none
            )

        CommentDeleted id (Err error) ->
            ( { model | errors = model.errors ++ [ "Server error while trying to delete comment." ] }
            , Cmd.none
            )

        DeleteArticle ->
            let
                cmdFromAuth authToken =
                    authToken
                        |> Article.delete (Article.slug model.article)
                        |> Http.send ArticleDeleted
            in
            session
                |> Session.attempt "delete articles" cmdFromAuth
                |> Util.updateFromResult model Cmd.none

        ArticleDeleted (Ok ()) ->
            ( model, Route.replaceUrl navKey Route.Home )

        ArticleDeleted (Err error) ->
            ( { model | errors = model.errors ++ [ "Server error while trying to delete article." ] }
            , Cmd.none
            )



-- INTERNAL --


withoutComment : CommentId -> List Comment -> List Comment
withoutComment id =
    List.filter (\comment -> Comment.id comment /= id)


favoriteButton : Article a -> Html Msg
favoriteButton article =
    let
        favoriteText =
            " Favorite Article (" ++ String.fromInt (Article.favoritesCount article) ++ ")"
    in
    Favorite.button (\_ -> ToggleFavorite) article [] [ text favoriteText ]


deleteButton : Article a -> Html Msg
deleteButton article =
    button [ class "btn btn-outline-danger btn-sm", onClick DeleteArticle ]
        [ i [ class "ion-trash-a" ] [], text " Delete Article" ]


editButton : Article a -> Html Msg
editButton article =
    a [ class "btn btn-outline-secondary btn-sm", Route.href (Route.EditArticle (Article.slug article)) ]
        [ i [ class "ion-edit" ] [], text " Edit Article" ]


followButton : Profile -> Html Msg
followButton author =
    Follow.button (\_ -> ToggleFollow)
        (Profile.following author)
        (Profile.username author)
