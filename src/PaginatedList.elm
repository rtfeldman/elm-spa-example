module PaginatedList exposing (PaginatedList, fromList, map, params, total, values)

import Html exposing (Html, a, li, text, ul)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Json.Decode as Decode exposing (Decoder)
import Task exposing (Task)
import Url.Builder exposing (QueryParameter)



-- TYPES


type PaginatedList a
    = PaginatedList
        { values : List a
        , total : Int
        }



-- INFO


values (PaginatedList info) = info.values


total (PaginatedList info) = info.total



-- CREATE


fromList totalCount list =
    PaginatedList { values = list, total = totalCount }



-- TRANSFORM


map transform (PaginatedList info) =
    PaginatedList { info | values = List.map transform info.values }



-- PARAMS


{-| I decided to accept a record here so I don't mess up the argument order of the two Ints.
-}
params { page, resultsPerPage } =
    let
        offset =
            (page - 1) * resultsPerPage
    in
    [ Url.Builder.string "limit" (String.fromInt resultsPerPage)
    , Url.Builder.string "offset" (String.fromInt offset)
    ]
