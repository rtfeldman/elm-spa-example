module Page.Profile exposing (Model, Msg, init, update, view)

{-| Viewing a user's profile.
-}

import Article.Feed as Feed exposing (ListConfig, defaultListConfig)
import Article.FeedSources as FeedSources exposing (FeedSources, Source(..))
import AuthToken exposing (AuthToken)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Me exposing (Me)
import Page.Errored exposing (PageLoadError, pageLoadError)
import Profile exposing (Profile)
import Session exposing (Session)
import Task exposing (Task)
import Time
import UserPhoto exposing (UserPhoto)
import Username exposing (Username)
import Views.Article.Feed as Feed
import Views.Errors as Errors
import Views.Follow as Follow
import Views.Page as Page



-- MODEL --


type alias Model =
    { errors : List String
    , profile : Profile
    , feed : Feed.Model
    }


init : Maybe AuthToken -> Username -> Task PageLoadError Model
init maybeToken username =
    let
        config : ListConfig
        config =
            { defaultListConfig | limit = 5, author = Just username }

        loadProfile =
            Profile.get username maybeToken
                |> Http.toTask

        loadFeedSources =
            Feed.init maybeToken (defaultFeedSources username)

        handleLoadError _ =
            "Profile is currently unavailable."
                |> pageLoadError (Page.Profile username)
    in
    Task.map2 (Model []) loadProfile loadFeedSources
        |> Task.mapError handleLoadError



-- VIEW --


view : Session -> Model -> { title : String, content : Html Msg }
view session model =
    let
        profile =
            model.profile

        isMyProfile =
            case Session.me session of
                Just me ->
                    Me.username me == Profile.username profile

                Nothing ->
                    False
    in
    { title =
        if isMyProfile then
            "My Profile"

        else
            case Session.me session of
                Just me ->
                    "Profile — " ++ Username.toString (Me.username me)

                Nothing ->
                    "Profile"
    , content =
        div [ class "profile-page" ]
            [ Errors.view DismissErrors model.errors
            , div [ class "user-info" ]
                [ div [ class "container" ]
                    [ div [ class "row" ]
                        [ viewProfileInfo isMyProfile profile ]
                    ]
                ]
            , div [ class "container" ]
                [ div [ class "row" ] [ viewFeed (Session.timeZone session) model.feed ] ]
            ]
    }


viewProfileInfo : Bool -> Profile -> Html Msg
viewProfileInfo isMyProfile profile =
    div [ class "col-xs-12 col-md-10 offset-md-1" ]
        [ img [ class "user-img", UserPhoto.src (Profile.image profile) ] []
        , h4 [] [ Username.toHtml (Profile.username profile) ]
        , p [] [ text (Maybe.withDefault "" (Profile.bio profile)) ]
        , if isMyProfile then
            text ""

          else
            followButton profile
        ]


viewFeed : Time.Zone -> Feed.Model -> Html Msg
viewFeed timeZone feed =
    div [ class "col-xs-12 col-md-10 offset-md-1" ] <|
        div [ class "articles-toggle" ]
            [ Feed.viewFeedSources feed |> Html.map FeedMsg ]
            :: (Feed.viewArticles timeZone feed |> List.map (Html.map FeedMsg))



-- UPDATE --


type Msg
    = DismissErrors
    | ToggleFollow
    | FollowCompleted (Result Http.Error Profile)
    | FeedMsg Feed.Msg


update : Maybe AuthToken -> Msg -> Model -> ( Model, Cmd Msg )
update maybeToken msg model =
    let
        profile =
            model.profile
    in
    case msg of
        DismissErrors ->
            ( { model | errors = [] }, Cmd.none )

        ToggleFollow ->
            case maybeToken of
                Nothing ->
                    ( { model | errors = model.errors ++ [ "You are currently signed out. You must be signed in to follow people." ] }
                    , Cmd.none
                    )

                Just token ->
                    token
                        |> Profile.toggleFollow
                            (Profile.username profile)
                            (Profile.following profile)
                        |> Http.send FollowCompleted
                        |> Tuple.pair model

        FollowCompleted (Ok newProfile) ->
            ( { model | profile = newProfile }, Cmd.none )

        FollowCompleted (Err error) ->
            ( model, Cmd.none )

        FeedMsg subMsg ->
            let
                ( newFeed, subCmd ) =
                    Feed.update maybeToken subMsg model.feed
            in
            ( { model | feed = newFeed }, Cmd.map FeedMsg subCmd )


followButton : Profile -> Html Msg
followButton profile =
    Follow.button (\_ -> ToggleFollow)
        (Profile.following profile)
        (Profile.username profile)



-- INTERNAL --


defaultFeedSources : Username -> FeedSources
defaultFeedSources username =
    FeedSources.fromLists (AuthorFeed username) [ FavoritedFeed username ]
