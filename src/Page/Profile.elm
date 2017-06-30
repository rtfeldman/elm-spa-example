module Page.Profile exposing (Model, Msg, init, update, view)

{-| Viewing a user's profile.
-}

import Data.Profile exposing (Profile)
import Data.Session exposing (Session)
import Data.User as User exposing (Username)
import Data.UserPhoto as UserPhoto exposing (UserPhoto)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Page.Errored exposing (PageLoadError, pageLoadError)
import Request.Article exposing (ListConfig, defaultListConfig)
import Request.Profile
import SelectList exposing (SelectList)
import Task exposing (Task)
import Util exposing ((=>), pair, viewIf)
import Views.Article.Feed as Feed exposing (FeedSource, authorFeed, favoritedFeed)
import Views.Errors as Errors
import Views.Page as Page
import Views.User.Follow as Follow


-- MODEL --


type alias Model =
    { errors : List String
    , profile : Profile
    , feed : Feed.Model
    }


init : Session -> Username -> Task PageLoadError Model
init session username =
    let
        config : ListConfig
        config =
            { defaultListConfig | limit = 5, author = Just username }

        maybeAuthToken =
            session.user
                |> Maybe.map .token

        loadProfile =
            Request.Profile.get username maybeAuthToken
                |> Http.toTask

        loadFeedSources =
            Feed.init session (defaultFeedSources username)

        handleLoadError _ =
            "Profile is currently unavailable."
                |> pageLoadError (Page.Profile username)
    in
    Task.map2 (Model []) loadProfile loadFeedSources
        |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    let
        profile =
            model.profile

        isMyProfile =
            session.user
                |> Maybe.map (\{ username } -> username == profile.username)
                |> Maybe.withDefault False
    in
    div [ class "profile-page" ]
        [ Errors.view DismissErrors model.errors
        , div [ class "user-info" ]
            [ div [ class "container" ]
                [ div [ class "row" ]
                    [ viewProfileInfo isMyProfile profile ]
                ]
            ]
        , div [ class "container" ]
            [ div [ class "row" ] [ viewFeed model.feed ] ]
        ]


viewProfileInfo : Bool -> Profile -> Html Msg
viewProfileInfo isMyProfile profile =
    div [ class "col-xs-12 col-md-10 offset-md-1" ]
        [ img [ class "user-img", UserPhoto.src profile.image ] []
        , h4 [] [ User.usernameToHtml profile.username ]
        , p [] [ text (Maybe.withDefault "" profile.bio) ]
        , viewIf (not isMyProfile) (followButton profile)
        ]


viewFeed : Feed.Model -> Html Msg
viewFeed feed =
    div [ class "col-xs-12 col-md-10 offset-md-1" ] <|
        div [ class "articles-toggle" ]
            [ Feed.viewFeedSources feed |> Html.map FeedMsg ]
            :: (Feed.viewArticles feed |> List.map (Html.map FeedMsg))



-- UPDATE --


type Msg
    = DismissErrors
    | ToggleFollow
    | FollowCompleted (Result Http.Error Profile)
    | FeedMsg Feed.Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg )
update session msg model =
    let
        profile =
            model.profile
    in
    case msg of
        DismissErrors ->
            { model | errors = [] } => Cmd.none

        ToggleFollow ->
            case session.user of
                Nothing ->
                    { model | errors = model.errors ++ [ "You are currently signed out. You must be signed in to follow people." ] }
                        => Cmd.none

                Just user ->
                    user.token
                        |> Request.Profile.toggleFollow
                            profile.username
                            profile.following
                        |> Http.send FollowCompleted
                        |> pair model

        FollowCompleted (Ok newProfile) ->
            { model | profile = newProfile } => Cmd.none

        FollowCompleted (Err error) ->
            model => Cmd.none

        FeedMsg subMsg ->
            let
                ( newFeed, subCmd ) =
                    Feed.update session subMsg model.feed
            in
            { model | feed = newFeed } => Cmd.map FeedMsg subCmd


followButton : Profile -> Html Msg
followButton =
    Follow.button (\_ -> ToggleFollow)



-- INTERNAL --


defaultFeedSources : Username -> SelectList FeedSource
defaultFeedSources username =
    SelectList.fromLists [] (authorFeed username) [ favoritedFeed username ]
