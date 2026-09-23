module Route exposing (Query, View(..), queryPath, repository, showPath, mutationPath, pageSize, pageSlice, pageCount)

import Url


type View
    = Browse
    | Search
    | Relevant
    | History
    | Compare
    | Conflicts
    | Doctor


type alias Query =
    { view : View
    , revision : String
    , text : String
    , mode : String
    , projection : String
    , domain : String
    , file : String
    , actor : String
    , since : String
    , until : String
    , includeObsolete : Bool
    , shallow : Bool
    , worktree : Bool
    , limit : Int
    , order : String
    , adr : String
    , compareFrom : String
    , compareTo : String
    }


repository : String
repository =
    "/api/v1/repository"


queryPath : Query -> String
queryPath query =
    let
        revision =
            if String.trim query.revision == "" then "HEAD" else query.revision

        at =
            [ ( "at", revision ) ]

        filter =
            optional "domain" query.domain
                ++ optional "file" query.file
                ++ optional "actor" query.actor
                ++ optional "since" query.since
                ++ optional "until" query.until

        resultLimit =
            String.fromInt (clamp 1 1000 query.limit)
    in
    case query.view of
        Browse ->
            path "/api/v1/search" ([ ( "q", "" ), ( "mode", query.mode ), ( "view", query.projection ), ( "limit", resultLimit ), ( "include_obsolete", bool query.includeObsolete ), ( "shallow", bool query.shallow ) ] ++ at ++ filter)

        Search ->
            path "/api/v1/search" ([ ( "q", query.text ), ( "mode", query.mode ), ( "view", query.projection ), ( "limit", resultLimit ), ( "include_obsolete", bool query.includeObsolete ), ( "shallow", bool query.shallow ) ] ++ at ++ filter)

        Relevant ->
            path "/api/v1/relevant" ([ ( "file", query.file ), ( "limit", String.fromInt (clamp 1 100 query.limit) ), ( "include_obsolete", bool query.includeObsolete ), ( "worktree", bool query.worktree ) ] ++ (if query.worktree then [] else at))

        History ->
            path "/api/v1/history" ([ ( "limit", resultLimit ), ( "order", query.order ) ] ++ at ++ optional "adr" query.adr ++ optional "actor" query.actor ++ optional "since" query.since ++ optional "until" query.until)

        Compare ->
            path "/api/v1/compare" [ ( "from", query.compareFrom ), ( "to", query.compareTo ), ( "include_unchanged", "false" ) ]

        Conflicts ->
            path "/api/v1/conflicts" at

        Doctor ->
            path "/api/v1/doctor" at


showPath : String -> String -> String -> String
showPath adr revision projection =
    path ("/api/v1/adrs/" ++ Url.percentEncode adr) [ ( "at", revision ), ( "view", projection ) ]


mutationPath : String -> String -> String
mutationPath action adr =
    if action == "create" then "/api/v1/adrs" else "/api/v1/adrs/" ++ Url.percentEncode adr ++ "/" ++ action


path : String -> List ( String, String ) -> String
path base fields =
    base ++ "?" ++ String.join "&" (List.map (\( key, value ) -> Url.percentEncode key ++ "=" ++ Url.percentEncode value) fields)


optional : String -> String -> List ( String, String )
optional key value =
    if String.trim value == "" then [] else [ ( key, value ) ]


bool : Bool -> String
bool value =
    if value then "true" else "false"


pageSize : Int
pageSize =
    100


pageCount : List a -> Int
pageCount items =
    max 1 ((List.length items + pageSize - 1) // pageSize)


pageSlice : Int -> List a -> List a
pageSlice number items =
    List.take pageSize (List.drop (max 0 number * pageSize) items)
