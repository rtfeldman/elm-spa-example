module Page.Home exposing (Model, Msg, initialize, update, view)

{-| The homepage. You can get here via either the / or /#/ routes.
-}

import Data.Article as Article exposing (Tag)
import Data.Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Request.Article
import SelectList exposing (SelectList)
import Task exposing (Task)
import Util exposing ((=>), onClickStopPropagation)
import Views.Article.Feed as Feed exposing (FeedSource, globalFeed, tagFeed, yourFeed)


-- MODEL --


type alias LoadedData =
    { tags : List Tag
    , feed : Feed.Model
    , session : Session
    }


type Model
    = InitializedModel
    | LoadedModel LoadedData


{-| Initialize returns a Model. It might not be entirely useful.
Maybe we can render something from it. Use
`prepare` to
-}
initialize : Session -> ( Model, Cmd Msg )
initialize session =
    let
        model =
            InitializedModel

        task =
            load model session

        resultToCmd =
            Util.unpackResult ErrorLoadingMsg LoadedModelMsg
    in
        ( model, Task.attempt resultToCmd task )


load : Model -> Session -> Task.Task String LoadedData
load model session =
    case model of
        LoadedModel loadedData ->
            if loadedData.session == session then
                Task.succeed loadedData
            else
                init session

        _ ->
            init session


init : Session -> Task String LoadedData
init session =
    let
        feedSources =
            if session.user == Nothing then
                SelectList.singleton globalFeed
            else
                SelectList.fromLists [] yourFeed [ globalFeed ]

        loadTags =
            Request.Article.tags
                |> Http.toTask

        loadSources =
            Feed.init session feedSources
    in
        Task.map3 LoadedData loadTags loadSources (Task.succeed session)
            |> Task.mapError Util.httpErrorString



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    case model of
        InitializedModel ->
            div [] [ text "LOADING" ]

        LoadedModel model ->
            div [ class "home-page" ]
                [ viewBanner
                , div [ class "container page" ]
                    [ div [ class "row" ]
                        [ div [ class "col-md-9" ] (viewFeed model.feed)
                        , div [ class "col-md-3" ]
                            [ div [ class "sidebar" ]
                                [ p [] [ text "Popular Tags" ]
                                , viewTags model.tags
                                ]
                            ]
                        ]
                    ]
                ]


viewBanner : Html msg
viewBanner =
    div [ class "banner" ]
        [ div [ class "container" ]
            [ h1 [ class "logo-font" ] [ text "conduit" ]
            , p [] [ text "A place to share your knowledge." ]
            ]
        ]


viewFeed : Feed.Model -> List (Html Msg)
viewFeed feed =
    div [ class "feed-toggle" ]
        [ Feed.viewFeedSources feed |> Html.map FeedMsg ]
        :: (Feed.viewArticles feed |> List.map (Html.map FeedMsg))


viewTags : List Tag -> Html Msg
viewTags tags =
    div [ class "tag-list" ] (List.map viewTag tags)


viewTag : Tag -> Html Msg
viewTag tagName =
    a
        [ class "tag-pill tag-default"
        , href "javascript:void(0)"
        , onClick (SelectTag tagName)
        ]
        [ text (Article.tagToString tagName) ]



-- UPDATE --


type Msg
    = FeedMsg Feed.Msg
    | SelectTag Tag
    | LoadedModelMsg LoadedData
    | ErrorLoadingMsg String


update : Session -> Msg -> Model -> Result String ( Model, Cmd Msg )
update session msg model =
    let
        ignore =
            Result.Ok ( model, Cmd.none )
    in
        case ( model, msg ) of
            ( _, ErrorLoadingMsg error ) ->
                Result.Err error

            ( InitializedModel, LoadedModelMsg loadedData ) ->
                Result.Ok ( LoadedModel loadedData, Cmd.none )

            ( InitializedModel, _ ) ->
                ignore

            ( LoadedModel data, msg ) ->
                case msg of
                    FeedMsg msgFeed ->
                        let
                            ( newFeed, subCmd ) =
                                Feed.update session msgFeed data.feed
                        in
                            Result.Ok ( LoadedModel { data | feed = newFeed }, Cmd.map FeedMsg subCmd )

                    SelectTag tagName ->
                        let
                            subCmd =
                                Feed.selectTag (Maybe.map .token session.user) tagName
                        in
                            Result.Ok ( model, Cmd.map FeedMsg subCmd )

                    LoadedModelMsg loadedData ->
                        Result.Ok ( LoadedModel loadedData, Cmd.none )

                    ErrorLoadingMsg error ->
                        Result.Err error
