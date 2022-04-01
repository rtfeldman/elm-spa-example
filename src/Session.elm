module Session exposing (Msg(..), Session, changes, cred, fromViewer, init, navKey, subscriptions, update, viewer)

import Api exposing (Cred)
import Avatar exposing (Avatar)
import Browser
import Browser.Navigation as Nav
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Route
import Time
import Url
import Viewer exposing (Viewer)



-- TYPES


type Session
    = LoggedIn Nav.Key Viewer
    | Guest Nav.Key


type Msg
    = GotSession Session
    | ClickedLink Browser.UrlRequest


init : Value -> Nav.Key -> ( Session, Cmd Msg )
init flags key =
    ( Decode.decodeValue Decode.string flags
        |> Result.andThen (Decode.decodeString (Api.storageDecoder Viewer.decoder))
        |> Result.toMaybe
        |> fromViewer key
    , Cmd.none
    )


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        GotSession newSession ->
            ( newSession
            , Route.replaceUrl (navKey session) Route.Home
            )

        ClickedLink urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    case url.fragment of
                        Nothing ->
                            -- If we got a link that didn't include a fragment,
                            -- it's from one of those (href "") attributes that
                            -- we have to include to make the RealWorld CSS work.
                            --
                            -- In an application doing path routing instead of
                            -- fragment-based routing, this entire
                            -- `case url.fragment of` expression this comment
                            -- is inside would be unnecessary.
                            ( session, Cmd.none )

                        Just _ ->
                            ( session
                            , Nav.pushUrl (navKey session) (Url.toString url)
                            )

                Browser.External href ->
                    ( session
                    , Nav.load href
                    )


subscriptions : Session -> Sub Msg
subscriptions session =
    changes GotSession (navKey session)



-- INFO


viewer : Session -> Maybe Viewer
viewer session =
    case session of
        LoggedIn _ val ->
            Just val

        Guest _ ->
            Nothing


cred : Session -> Maybe Cred
cred session =
    case session of
        LoggedIn _ val ->
            Just (Viewer.cred val)

        Guest _ ->
            Nothing


navKey : Session -> Nav.Key
navKey session =
    case session of
        LoggedIn key _ ->
            key

        Guest key ->
            key



-- CHANGES


changes : (Session -> msg) -> Nav.Key -> Sub msg
changes toMsg key =
    Api.viewerChanges (\maybeViewer -> toMsg (fromViewer key maybeViewer)) Viewer.decoder


fromViewer : Nav.Key -> Maybe Viewer -> Session
fromViewer key maybeViewer =
    -- It's stored in localStorage as a JSON String;
    -- first decode the Value as a String, then
    -- decode that String as JSON.
    case maybeViewer of
        Just viewerVal ->
            LoggedIn key viewerVal

        Nothing ->
            Guest key
