module RoutingTests exposing (..)

import Data.Article as Article exposing (Slug)
import Data.User as User exposing (Username)
import Expect exposing (Expectation)
import Json.Decode exposing (decodeString)
import Navigation exposing (Location)
import Route exposing (Route(..))
import Test exposing (..)


-- TODO need to add lots more tests!


fromLocation : Test
fromLocation =
    describe "Route.fromLocation"
        [ testLocation "" Root
        , testLocation "#login" Login
        , testLocation "#logout" Logout
        , testLocation "#settings" Settings
        , testLocation "#profile/foo" (Profile (usernameFromStr "foo"))
        , testLocation "#register" Register
        , testLocation "#article/foo" (Article (slugFromStr "foo"))
        , testLocation "#editor" NewArticle
        , testLocation "#editor/foo" (EditArticle (slugFromStr "foo"))
        ]



-- HELPERS --


testLocation : String -> Route -> Test
testLocation hash route =
    test ("Parsing hash: \"" ++ hash ++ "\"") <|
        \() ->
            makeHashLocation hash
                |> Route.fromLocation
                |> Expect.equal (Just route)


makeHashLocation : String -> Location
makeHashLocation hash =
    { hash = hash
    , href = ""
    , host = ""
    , hostname = ""
    , protocol = ""
    , origin = ""
    , port_ = ""
    , pathname = ""
    , search = ""
    , username = ""
    , password = ""
    }



-- CONSTRUCTING UNEXPOSED VALUES --
-- By decoding values that are not intended to be exposed directly - and crashing
-- if they cannot be decoded, since crashing is harmless in tests - we can let
-- our internal modules continue to expose only the intended ways of
-- constructing those, while still being able to test them.


usernameFromStr : String -> Username
usernameFromStr str =
    case decodeString User.usernameDecoder ("\"" ++ str ++ "\"") of
        Ok username ->
            username

        Err err ->
            Debug.crash ("Error decoding Username from \"" ++ str ++ "\": " ++ err)


slugFromStr : String -> Slug
slugFromStr str =
    let
        json =
            """
            { "description": null
            , "slug": \"""" ++ str ++ """"
            , "title": ""
            , "tagList": []
            , "createdAt": "2012-04-23T18:25:43.511Z"
            , "updatedAt": "2012-04-23T18:25:43.511Z"
            , "favorited": false
            , "favoritesCount": 1
            , "author":
                 { "username": ""
                 , "bio": null
                 , "image": null
                 , "following": false
                 }
            }
        """
    in
    case decodeString Article.decoder json of
        Ok article ->
            article.slug

        Err err ->
            Debug.crash ("Error decoding Slug from \"" ++ str ++ "\": " ++ err)
