module Author exposing (Author(..), FollowedAuthor, UnfollowedAuthor, decoder, fetch, follow, followButton, profile, requestFollow, requestUnfollow, unfollow, unfollowButton, username, view)

{-| The author of an Article. It includes a Profile.

I designed this to make sure the compiler would help me keep these three
possibilities straight when displaying follow buttons and such:

  - I'm following this author.
  - I'm not following this author.
  - I _can't_ follow this author, because it's me!

To do this, I defined `Author` a custom type with three variants, one for each
of those possibilities.

I also made separate types for FollowedAuthor and UnfollowedAuthor.
They are custom type wrappers around Profile, and thier sole purpose is to
help me keep track of which operations are supported.

For example, consider these functions:

requestFollow : UnfollowedAuthor -> Cred -> Http.Request Author
requestUnfollow : FollowedAuthor -> Cred -> Http.Request Author

These types help the compiler prevent several mistakes:

  - Displaying a Follow button for an author the user already follows.
  - Displaying an Unfollow button for an author the user already doesn't follow.
  - Displaying either button when the author is ourself.

There are still ways we could mess things up (e.g. make a button that calls Author.unfollow when you click it, but which displays "Follow" to the user) - but this rules out a bunch of potential problems.

-}

import Api exposing (Cred)
import Api.Endpoint as Endpoint
import Html exposing (Html, a, i, text)
import Html.Attributes exposing (attribute, class, href, id, placeholder)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (custom, optional, required)
import Json.Encode as Encode exposing (Value)
import Profile exposing (Profile)
import Route exposing (Route)
import Username exposing (Username)
import Viewer exposing (Viewer)


{-| An author - either the current user, another user we're following, or
another user we aren't following.

These distinctions matter because we can only perform "follow" requests for
users we aren't following, we can only perform "unfollow" requests for
users we _are_ following, and we can't perform either for ourselves.

-}
type Author
    = IsFollowing FollowedAuthor
    | IsNotFollowing UnfollowedAuthor
    | IsViewer Cred Profile


{-| An author we're following.
-}
type FollowedAuthor
    = FollowedAuthor Username Profile


{-| An author we're not following.
-}
type UnfollowedAuthor
    = UnfollowedAuthor Username Profile


{-| Return an Author's username.
-}
username : Author -> Username
username author =
    case author of
        IsViewer cred _ ->
            Api.username cred

        IsFollowing (FollowedAuthor val _) ->
            val

        IsNotFollowing (UnfollowedAuthor val _) ->
            val


{-| Return an Author's profile.
-}
profile : Author -> Profile
profile author =
    case author of
        IsViewer _ val ->
            val

        IsFollowing (FollowedAuthor _ val) ->
            val

        IsNotFollowing (UnfollowedAuthor _ val) ->
            val



-- FETCH


fetch : Username -> Maybe Cred -> Http.Request Author
fetch uname maybeCred =
    Decode.field "profile" (decoder maybeCred)
        |> Api.get (Endpoint.profiles uname) maybeCred



-- FOLLOWING


follow : UnfollowedAuthor -> FollowedAuthor
follow (UnfollowedAuthor uname prof) =
    FollowedAuthor uname prof


unfollow : FollowedAuthor -> UnfollowedAuthor
unfollow (FollowedAuthor uname prof) =
    UnfollowedAuthor uname prof


requestFollow : UnfollowedAuthor -> Cred -> Http.Request Author
requestFollow (UnfollowedAuthor uname _) cred =
    Api.post (Endpoint.follow uname) (Just cred) Http.emptyBody (followDecoder cred)


requestUnfollow : FollowedAuthor -> Cred -> Http.Request Author
requestUnfollow (FollowedAuthor uname _) cred =
    Api.delete (Endpoint.follow uname)
        cred
        Http.emptyBody
        (followDecoder cred)


followDecoder : Cred -> Decoder Author
followDecoder cred =
    Decode.field "profile" (decoder (Just cred))


followButton :
    (Cred -> UnfollowedAuthor -> msg)
    -> Cred
    -> UnfollowedAuthor
    -> Html msg
followButton toMsg cred ((UnfollowedAuthor uname _) as author) =
    toggleFollowButton "Follow"
        [ "btn-outline-secondary" ]
        (toMsg cred author)
        uname


unfollowButton :
    (Cred -> FollowedAuthor -> msg)
    -> Cred
    -> FollowedAuthor
    -> Html msg
unfollowButton toMsg cred ((FollowedAuthor uname _) as author) =
    toggleFollowButton "Unfollow"
        [ "btn-secondary" ]
        (toMsg cred author)
        uname


toggleFollowButton : String -> List String -> msg -> Username -> Html msg
toggleFollowButton txt extraClasses msgWhenClicked uname =
    let
        classStr =
            "btn btn-sm " ++ String.join " " extraClasses ++ " action-btn"

        caption =
            "\u{00A0}" ++ txt ++ " " ++ Username.toString uname
    in
    Html.button [ class classStr, onClick msgWhenClicked ]
        [ i [ class "ion-plus-round" ] []
        , text caption
        ]



-- SERIALIZATION


decoder : Maybe Cred -> Decoder Author
decoder maybeCred =
    Decode.succeed Tuple.pair
        |> custom Profile.decoder
        |> required "username" Username.decoder
        |> Decode.andThen (decodeFromPair maybeCred)


decodeFromPair : Maybe Cred -> ( Profile, Username ) -> Decoder Author
decodeFromPair maybeCred ( prof, uname ) =
    case maybeCred of
        Nothing ->
            -- If you're logged out, you can't be following anyone!
            Decode.succeed (IsNotFollowing (UnfollowedAuthor uname prof))

        Just cred ->
            if uname == Api.username cred then
                Decode.succeed (IsViewer cred prof)

            else
                nonViewerDecoder prof uname


nonViewerDecoder : Profile -> Username -> Decoder Author
nonViewerDecoder prof uname =
    Decode.succeed (authorFromFollowing prof uname)
        |> optional "following" Decode.bool False


authorFromFollowing : Profile -> Username -> Bool -> Author
authorFromFollowing prof uname isFollowing =
    if isFollowing then
        IsFollowing (FollowedAuthor uname prof)

    else
        IsNotFollowing (UnfollowedAuthor uname prof)


{-| View an author. We basically render their username and a link to their
profile, and that's it.
-}
view : Username -> Html msg
view uname =
    a [ class "author", Route.href (Route.Profile uname) ]
        [ Username.toHtml uname ]
