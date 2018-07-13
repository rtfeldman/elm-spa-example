module Article.FeedSources exposing (FeedSources, Source(..), after, before, fromLists, select, selected)

import Article
import Article.Feed as Feed
import Article.Tag as Tag exposing (Tag)
import Username exposing (Username)



-- TYPES


type FeedSources
    = FeedSources
        { before : List Source
        , selected : Source
        , after : List Source
        }


type Source
    = YourFeed
    | GlobalFeed
    | TagFeed Tag
    | FavoritedFeed Username
    | AuthorFeed Username



-- BUILDING


fromLists : Source -> List Source -> FeedSources
fromLists selectedSource afterSources =
    FeedSources
        { before = []
        , selected = selectedSource
        , after = afterSources
        }



-- SELECTING


select : Source -> FeedSources -> FeedSources
select selectedSource (FeedSources sources) =
    let
        ( newBefore, newAfter ) =
            (sources.before ++ (sources.selected :: sources.after))
                -- By design, tags can only be included if they're selected.
                |> List.filter isNotTag
                |> splitOn (\source -> source == selectedSource)
    in
    FeedSources
        { before = List.reverse newBefore
        , selected = selectedSource
        , after = List.reverse newAfter
        }


splitOn : (Source -> Bool) -> List Source -> ( List Source, List Source )
splitOn isSelected sources =
    let
        ( _, newBefore, newAfter ) =
            List.foldl (splitOnHelp isSelected) ( False, [], [] ) sources
    in
    ( newBefore, newAfter )


splitOnHelp : (Source -> Bool) -> Source -> ( Bool, List Source, List Source ) -> ( Bool, List Source, List Source )
splitOnHelp isSelected source ( foundSelected, beforeSelected, afterSelected ) =
    if isSelected source then
        ( True, beforeSelected, afterSelected )

    else if foundSelected then
        ( foundSelected, beforeSelected, source :: afterSelected )

    else
        ( foundSelected, source :: beforeSelected, afterSelected )


isNotTag : Source -> Bool
isNotTag currentSource =
    case currentSource of
        TagFeed _ ->
            False

        _ ->
            True



-- ACCESSING


selected : FeedSources -> Source
selected (FeedSources record) =
    record.selected


before : FeedSources -> List Source
before (FeedSources record) =
    record.before


after : FeedSources -> List Source
after (FeedSources record) =
    record.after
