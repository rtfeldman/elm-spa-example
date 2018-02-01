module Page.Article exposing (Model, Msg, init, update, view)

{-| Viewing an individual article.
-}

import Data.Article as Article exposing (Article, Body)
import Data.Article.Author exposing (Author)
import Data.Article.Comment exposing (Comment, CommentId)
import Data.Session as Session exposing (Session)
import Data.User as User exposing (User)
import Data.UserPhoto as UserPhoto
import Date exposing (Date)
import Date.Format
import Html exposing (..)
import Html.Attributes exposing (attribute, class, disabled, href, id, placeholder)
import Html.Events exposing (onClick, onInput, onSubmit)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Request.Article
import Request.Article.Comments
import Request.Profile
import Route
import Task exposing (Task)
import Util exposing ((=>), pair, viewIf)
import Views.Article
import Views.Article.Favorite as Favorite
import Views.Author
import Views.Errors
import Views.Page as Page
import Views.User.Follow as Follow


-- MODEL --


type alias Model =
    { errors : List String
    , commentText : String
    , commentInFlight : Bool
    , article : Article Body
    , comments : List Comment
    }


init : Session -> Article.Slug -> Task PageLoadError Model
init session slug =
    let
        maybeAuthToken =
            Maybe.map .token session.user

        loadArticle =
            Request.Article.get maybeAuthToken slug
                |> Http.toTask

        loadComments =
            Request.Article.Comments.list maybeAuthToken slug
                |> Http.toTask

        handleLoadError _ =
            pageLoadError Page.Other "Article is currently unavailable."
    in
    Task.map2 (Model [] "" False) loadArticle loadComments
        |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    let
        article =
            model.article

        author =
            article.author

        buttons =
            viewButtons article author session.user

        postingDisabled =
            model.commentInFlight
    in
    div [ class "article-page" ]
        [ viewBanner model.errors article author session.user
        , div [ class "container page" ]
            [ div [ class "row article-content" ]
                [ div [ class "col-md-12" ]
                    [ Article.bodyToHtml article.body [] ]
                ]
            , hr [] []
            , div [ class "article-actions" ]
                [ div [ class "article-meta" ] <|
                    [ a [ Route.href (Route.Profile author.username) ]
                        [ img [ UserPhoto.src author.image ] [] ]
                    , div [ class "info" ]
                        [ Views.Author.view author.username
                        , Views.Article.viewTimestamp article
                        ]
                    ]
                        ++ buttons
                ]
            , div [ class "row" ]
                [ div [ class "col-xs-12 col-md-8 offset-md-2" ] <|
                    viewAddComment postingDisabled session.user
                        :: List.map (viewComment session.user) model.comments
                ]
            ]
        ]


viewBanner : List String -> Article a -> Author -> Maybe User -> Html Msg
viewBanner errors article author maybeUser =
    let
        buttons =
            viewButtons article author maybeUser
    in
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [] [ text article.title ]
            , div [ class "article-meta" ] <|
                [ a [ Route.href (Route.Profile author.username) ]
                    [ img [ UserPhoto.src author.image ] [] ]
                , div [ class "info" ]
                    [ Views.Author.view author.username
                    , Views.Article.viewTimestamp article
                    ]
                ]
                    ++ buttons
            , Views.Errors.view DismissErrors errors
            ]
        ]


viewAddComment : Bool -> Maybe User -> Html Msg
viewAddComment postingDisabled maybeUser =
    case maybeUser of
        Nothing ->
            p []
                [ a [ Route.href Route.Login ] [ text "Sign in" ]
                , text " or "
                , a [ Route.href Route.Register ] [ text "sign up" ]
                , text " to add comments on this article."
                ]

        Just user ->
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
                    [ img [ class "comment-author-img", UserPhoto.src user.image ] []
                    , button
                        [ class "btn btn-sm btn-primary"
                        , disabled postingDisabled
                        ]
                        [ text "Post Comment" ]
                    ]
                ]


viewButtons : Article a -> Author -> Maybe User -> List (Html Msg)
viewButtons article author maybeUser =
    let
        isMyArticle =
            Maybe.map .username maybeUser == Just author.username
    in
    if isMyArticle then
        [ editButton article
        , text " "
        , deleteButton article
        ]
    else
        [ followButton author
        , text " "
        , favoriteButton article
        ]


viewComment : Maybe User -> Comment -> Html Msg
viewComment user comment =
    let
        author =
            comment.author

        isAuthor =
            Maybe.map .username user == Just comment.author.username
    in
    div [ class "card" ]
        [ div [ class "card-block" ]
            [ p [ class "card-text" ] [ text comment.body ] ]
        , div [ class "card-footer" ]
            [ a [ class "comment-author", href "" ]
                [ img [ class "comment-author-img", UserPhoto.src author.image ] []
                , text " "
                ]
            , text " "
            , a [ class "comment-author", Route.href (Route.Profile author.username) ]
                [ text (User.usernameToString comment.author.username) ]
            , span [ class "date-posted" ] [ text (formatCommentTimestamp comment.createdAt) ]
            , viewIf isAuthor <|
                span
                    [ class "mod-options"
                    , onClick (DeleteComment comment.id)
                    ]
                    [ i [ class "ion-trash-a" ] [] ]
            ]
        ]


formatCommentTimestamp : Date -> String
formatCommentTimestamp =
    Date.Format.format "%B %e, %Y"



-- UPDATE --


type Msg
    = DismissErrors
    | ToggleFavorite
    | FavoriteCompleted (Result Http.Error (Article Body))
    | ToggleFollow
    | FollowCompleted (Result Http.Error Author)
    | SetCommentText String
    | DeleteComment CommentId
    | CommentDeleted CommentId (Result Http.Error ())
    | PostComment
    | CommentPosted (Result Http.Error Comment)
    | DeleteArticle
    | ArticleDeleted (Result Http.Error ())


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        article =
            model.article

        author =
            article.author
    in
    case msg of
        DismissErrors ->
            { model | errors = [] } => Cmd.none

        ToggleFavorite ->
            let
                cmdFromAuth authToken =
                    Request.Article.toggleFavorite model.article authToken
                        |> Http.toTask
                        |> Task.map (\newArticle -> { newArticle | body = article.body })
                        |> Task.attempt FavoriteCompleted
            in
            session
                |> Session.attempt "favorite" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors model)

        FavoriteCompleted (Ok newArticle) ->
            { model | article = newArticle } => Cmd.none

        FavoriteCompleted (Err error) ->
            -- In a serious production application, we would log the error to
            -- a logging service so we could investigate later.
            [ "There was a server error trying to record your Favorite. Sorry!" ]
                |> Util.appendErrors model
                => Cmd.none

        ToggleFollow ->
            let
                cmdFromAuth authToken =
                    authToken
                        |> Request.Profile.toggleFollow author.username author.following
                        |> Http.send FollowCompleted
            in
            session
                |> Session.attempt "follow" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors model)

        FollowCompleted (Ok { following }) ->
            let
                newArticle =
                    { article | author = { author | following = following } }
            in
            { model | article = newArticle } => Cmd.none

        FollowCompleted (Err error) ->
            { model | errors = "Unable to follow user." :: model.errors }
                => Cmd.none

        SetCommentText commentText ->
            { model | commentText = commentText } => Cmd.none

        PostComment ->
            let
                comment =
                    model.commentText
            in
            if model.commentInFlight || String.isEmpty comment then
                model => Cmd.none
            else
                let
                    cmdFromAuth authToken =
                        authToken
                            |> Request.Article.Comments.post model.article.slug comment
                            |> Http.send CommentPosted
                in
                session
                    |> Session.attempt "post a comment" cmdFromAuth
                    |> Tuple.mapFirst (Util.appendErrors { model | commentInFlight = True })

        CommentPosted (Ok comment) ->
            { model
                | commentInFlight = False
                , comments = comment :: model.comments
            }
                => Cmd.none

        CommentPosted (Err error) ->
            { model | errors = model.errors ++ [ "Server error while trying to post comment." ] }
                => Cmd.none

        DeleteComment id ->
            let
                cmdFromAuth authToken =
                    authToken
                        |> Request.Article.Comments.delete model.article.slug id
                        |> Http.send (CommentDeleted id)
            in
            session
                |> Session.attempt "delete comments" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors model)

        CommentDeleted id (Ok ()) ->
            { model | comments = withoutComment id model.comments }
                => Cmd.none

        CommentDeleted id (Err error) ->
            { model | errors = model.errors ++ [ "Server error while trying to delete comment." ] }
                => Cmd.none

        DeleteArticle ->
            let
                cmdFromAuth authToken =
                    authToken
                        |> Request.Article.delete model.article.slug
                        |> Http.send ArticleDeleted
            in
            session
                |> Session.attempt "delete articles" cmdFromAuth
                |> Tuple.mapFirst (Util.appendErrors model)

        ArticleDeleted (Ok ()) ->
            model => Route.modifyUrl Route.Home

        ArticleDeleted (Err error) ->
            { model | errors = model.errors ++ [ "Server error while trying to delete article." ] }
                => Cmd.none



-- INTERNAL --


withoutComment : CommentId -> List Comment -> List Comment
withoutComment id =
    List.filter (\comment -> comment.id /= id)


favoriteButton : Article a -> Html Msg
favoriteButton article =
    let
        favoriteText =
            " Favorite Article (" ++ toString article.favoritesCount ++ ")"
    in
    Favorite.button (\_ -> ToggleFavorite) article [] [ text favoriteText ]


deleteButton : Article a -> Html Msg
deleteButton article =
    button [ class "btn btn-outline-danger btn-sm", onClick DeleteArticle ]
        [ i [ class "ion-trash-a" ] [], text " Delete Article" ]


editButton : Article a -> Html Msg
editButton article =
    a [ class "btn btn-outline-secondary btn-sm", Route.href (Route.EditArticle article.slug) ]
        [ i [ class "ion-edit" ] [], text " Edit Article" ]


followButton : Follow.State record -> Html Msg
followButton =
    Follow.button (\_ -> ToggleFollow)
