module Api exposing (AsOf(..), Envelope, Metadata, Repository, SearchWindow, SearchHit, RelevantWindow, RelevantHit, Evidence, Inspection, CandidateRecord, CandidateSet, Operation, OperationItem, Provenance, HistoryWindow, HistoryItem, CompareWindow, CompareEntry, CompareSnapshot, CompareChange, ConflictWindow, ConflictEntry, ConflictCandidate, Doctor, Issue, MutationOutcome, Failure, Event, generation, compareGeneration, response, failure, repository, search, relevant, inspection, history, comparison, conflicts, doctor, mutation, event, request)

import Dict
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


type AsOf
    = AtCommit String
    | AtComparison String String
    | Unavailable String


type alias Metadata =
    { generation : String, asOf : AsOf }


type alias Envelope a =
    { metadata : Metadata, data : a }


type alias Repository =
    { worktree : String, head : String, headRef : Maybe String, stateToken : String }


type alias SearchWindow =
    { asOf : String, limit : Int, results : List SearchHit }


type alias SearchHit =
    { adr : String, title : String, summary : String, status : String, domains : List String, scopes : List String, stateToken : String, score : Maybe Float, matchedTitle : Maybe String, matchFields : List String, matchTerms : List String, sourcePaths : List String, conflicts : List String, resolutionRequired : Bool }


type alias RelevantWindow =
    { asOf : String, file : String, source : String, results : List RelevantHit }


type alias RelevantHit =
    { adr : String, title : String, summary : String, status : String, scopeMatch : String, confidence : String, score : Float, evidence : List Evidence }


type alias Evidence =
    { fileExcerpt : String, adrExcerpt : String, section : String, score : Float }


type alias Inspection =
    { adr : String, asOf : String, view : String, title : String, summary : String, body : String, status : String, domains : List String, scopes : List String, stateToken : String, recordHeads : List String, scopeHeads : List String, domainHeads : List String, statusHeads : List String, candidates : CandidateSet, conflicts : List String, sourcePaths : List String, operations : List Operation, provenance : List Provenance, resolutionRequired : Bool }


type alias CandidateRecord =
    { id : String, title : String, summary : String, body : String, path : String }


type alias CandidateSet =
    { records : List CandidateRecord, scopes : List CandidateRecord, domains : List CandidateRecord }


type alias Operation =
    { id : String, items : List OperationItem, provenance : Maybe Provenance }


type alias OperationItem =
    { id : String, kind : String, event : String, title : Maybe String, summary : Maybe String, body : Maybe String, rationale : Maybe String, relation : Maybe String, rawSemantic : Maybe String, path : String, domains : List String, scopes : List String, state : Maybe String, replacement : Maybe String, added : List String, removed : List String, refinements : List String, parents : List String, diffs : List String }


type alias Provenance =
    { actor : String, model : Maybe String, claimedAt : String, basis : String, operation : String, inputDigest : Maybe String, promptDigest : Maybe String, contextDigest : Maybe String, introductions : List String, originalCommits : List String, placements : List String, lineLandings : List String }


type alias HistoryWindow =
    { asOf : String, limit : Int, truncated : Bool, operations : List HistoryItem }


type alias HistoryItem =
    { adr : String, title : String, label : String, actor : String, claimedAt : String, operation : String, reason : Maybe String, commit : Maybe String, changes : List String }


type alias CompareWindow =
    { from : String, to : String, entries : List CompareEntry }


type alias CompareEntry =
    { adr : String, title : String, kind : String, before : Maybe CompareSnapshot, after : Maybe CompareSnapshot, changes : List CompareChange }


type alias CompareChange =
    { field : String, before : String, after : String, diff : Maybe String }


type alias CompareSnapshot =
    { title : String, summary : String, body : String, status : String, domains : List String, scopes : List String }


type alias ConflictWindow =
    { conflicts : List ConflictEntry }


type alias ConflictEntry =
    { adr : String, code : String, summaries : List String, candidates : List ConflictCandidate }


type alias ConflictCandidate =
    { axis : String, heads : List String, summary : String }


type alias Doctor =
    { issues : List Issue }


type alias Issue =
    { severity : String, code : String, message : String, path : Maybe String }


type alias MutationOutcome =
    { committed : Bool, operation : String, commit : String, adr : String, indexed : Bool, indexError : Maybe String, publicationWarning : Maybe String }


type alias Failure =
    { metadata : Metadata, category : String, status : Int, code : String, message : String }


type alias Event =
    { generation : String, asOf : AsOf, kind : String, facts : List String }


generation : Decoder String
generation =
    D.string
        |> D.andThen
            (\raw ->
                if validGeneration raw then D.succeed raw else D.fail "generation must be a canonical unsigned decimal Word64 string"
            )


validGeneration : String -> Bool
validGeneration raw =
    let
        digits =
            not (String.isEmpty raw)
                && String.all (\c -> c >= '0' && c <= '9') raw

        canonical =
            raw == "0" || not (String.startsWith "0" raw)

        maxWord =
            "18446744073709551615"
    in
    digits && canonical && (String.length raw < String.length maxWord || (String.length raw == String.length maxWord && raw <= maxWord))


compareGeneration : String -> String -> Order
compareGeneration left right =
    case compare (String.length left) (String.length right) of
        EQ -> compare left right
        answer -> answer


asOf : Decoder AsOf
asOf =
    D.field "kind" D.string
        |> D.andThen
            (\kind ->
                case kind of
                    "commit" -> D.map AtCommit (D.field "oid" D.string)
                    "comparison" -> D.map2 AtComparison (D.field "from" D.string) (D.field "to" D.string)
                    "unavailable" -> D.map Unavailable (D.field "reason" D.string)
                    _ -> D.fail "unknown as_of kind"
            )


metadata : Decoder Metadata
metadata =
    D.map2 Metadata (D.field "generation" generation) (D.field "as_of" asOf)


response : Decoder a -> Decoder (Envelope a)
response dataDecoder =
    D.field "schema" D.string
        |> D.andThen
            (\schema ->
                if schema == "adrai/api/v1" then
                    D.map2 Envelope (D.field "metadata" metadata) (D.field "data" dataDecoder)
                else
                    D.fail "unsupported API schema"
            )


failure : Decoder Failure
failure =
    D.field "schema" D.string
        |> D.andThen
            (\schema ->
                if schema == "adrai/api/v1" then
                    D.map5 Failure
                        (D.field "metadata" metadata)
                        (D.at [ "error", "category" ] D.string)
                        (D.at [ "error", "status" ] D.int)
                        (D.at [ "error", "code" ] D.string)
                        (D.at [ "error", "message" ] D.string)
                else
                    D.fail "unsupported API schema"
            )


repository : Decoder Repository
repository =
    D.map4 Repository
        (D.field "worktree" D.string)
        (D.field "head" D.string)
        (D.field "head_ref" (D.nullable D.string))
        (D.at [ "repository_state", "token" ] D.string)


search : Decoder SearchWindow
search =
    D.map3 SearchWindow
        (D.field "as_of" D.string)
        (D.field "limit" D.int)
        (D.field "results" (D.list searchHit))


searchHit : Decoder SearchHit
searchHit =
    D.map8 (\adr title summary status domains scopes stateToken score ->
        { adr = adr, title = title, summary = summary, status = status, domains = domains, scopes = scopes, stateToken = stateToken, score = score, matchedTitle = Nothing, matchFields = [], matchTerms = [], sourcePaths = [], conflicts = [], resolutionRequired = False })
        (D.field "adr" D.string)
        (D.field "title" D.string)
        (D.field "summary" D.string)
        (D.field "status" D.string)
        (D.field "domains" (D.list D.string))
        (D.field "applies_to" (D.list D.string))
        (D.field "state_token" D.string)
        (D.field "score" (D.nullable D.float))
        |> D.andThen
            (\base ->
                D.map6
                    (\matched fields terms paths conflictSummaries required ->
                        { base | matchedTitle = matched, matchFields = fields, matchTerms = terms, sourcePaths = paths, conflicts = conflictSummaries, resolutionRequired = required })
                    (optional "matched_title" (D.nullable D.string) Nothing)
                    (optionalAt [ "matches", "fields" ] (D.list D.string) [])
                    (optionalAt [ "matches", "terms" ] (D.list D.string) [])
                    (optional "source_paths" (D.list D.string) [])
                    (optional "conflicts" (D.list (D.field "summary" D.string)) [])
                    (optional "resolution_required" D.bool False)
            )


relevant : Decoder RelevantWindow
relevant =
    D.map4 RelevantWindow
        (D.field "as_of" D.string)
        (D.at [ "file", "path" ] D.string)
        (D.at [ "file", "source" ] D.string)
        (D.field "results" (D.list relevantHit))


relevantHit : Decoder RelevantHit
relevantHit =
    D.map8 RelevantHit
        (D.field "adr" D.string)
        (D.field "title" D.string)
        (D.field "summary" D.string)
        (D.field "status" D.string)
        (D.field "scope_match" D.string)
        (D.field "confidence" D.string)
        (D.field "score" D.float)
        (D.field "evidence" (D.list evidence))


evidence : Decoder Evidence
evidence =
    D.map4 Evidence
        (D.field "file_excerpt" D.string)
        (D.field "adr_excerpt" D.string)
        (D.field "adr_section" D.string)
        (D.field "score" D.float)


inspection : Decoder Inspection
inspection =
    D.map2 Tuple.pair (D.field "schema" D.string) (D.field "view" D.string)
        |> D.andThen
            (\( schema, viewMode ) ->
                case ( schema, viewMode ) of
                    ( "adrai/show-collapsed/v1", "collapsed" ) ->
                        D.map2 (\_ value -> value) collapsedRequired inspectionFields

                    ( "adrai/show-exploded/v1", "exploded" ) ->
                        D.map2 (\_ value -> value) explodedRequired inspectionFields

                    _ ->
                        D.fail "unsupported inspection schema or view"
            )


collapsedRequired : Decoder ()
collapsedRequired =
    D.map8 (\_ _ _ _ _ _ _ _ -> ())
        (D.field "title" D.string)
        (D.field "summary" D.string)
        (D.field "body" D.string)
        (D.field "status" D.string)
        (D.field "domains" (D.list D.string))
        (D.field "applies_to" (D.list D.string))
        (D.field "record_heads" (D.list D.string))
        (D.field "scope_heads" (D.list D.string))
        |> D.andThen
            (\_ ->
                D.map8 (\_ _ _ _ _ _ _ _ -> ())
                    (D.field "domain_heads" (D.list D.string))
                    (D.field "status_heads" (D.list D.string))
                    (D.field "candidate_records" (D.list candidateRecord))
                    (D.field "candidate_scopes" (D.list candidateScope))
                    (D.field "candidate_domains" (D.list candidateDomain))
                    (D.field "conflicts" (D.list D.string))
                    (D.field "source_paths" (D.list D.string))
                    (D.field "provenance" (D.dict (D.nullable provenance)))
                    |> D.andThen
                        (\_ ->
                            D.map3 (\_ _ _ -> ())
                                (D.field "resolved" D.bool)
                                (D.field "resolution_required" D.bool)
                                (D.field "resolution" resolution)
                        )
            )


explodedRequired : Decoder ()
explodedRequired =
    D.map4 (\_ _ _ _ -> ())
        (D.field "operations" (D.list operation))
        (D.field "resolution" resolution)
        (D.field "resolution_required" D.bool)
        (D.field "resolved" D.bool)


resolution : Decoder ()
resolution =
    D.map3 (\_ _ _ -> ())
        (D.field "resolved" D.bool)
        (D.field "resolution_required" D.bool)
        (D.field "conflicts" (D.list resolutionConflict))


resolutionConflict : Decoder ()
resolutionConflict =
    D.map4 (\_ _ _ _ -> ())
        (D.field "kind" D.string)
        (D.field "head_count" D.int)
        (D.field "heads" (D.list D.string))
        (D.field "summary" D.string)


inspectionFields : Decoder Inspection
inspectionFields =
    D.map8
        (\adr revision view title summary body status token ->
            { adr = adr, asOf = revision, view = view, title = title, summary = summary, body = body, status = status, domains = [], scopes = [], stateToken = token, recordHeads = [], scopeHeads = [], domainHeads = [], statusHeads = [], candidates = { records = [], scopes = [], domains = [] }, conflicts = [], sourcePaths = [], operations = [], provenance = [], resolutionRequired = False })
        (D.field "adr" D.string)
        (D.field "as_of" D.string)
        (D.field "view" D.string)
        (optional "title" D.string "")
        (optional "summary" D.string "")
        (optional "body" D.string "")
        (optional "status" D.string "")
        (D.field "state_token" D.string)
        |> D.andThen
            (\base ->
                D.map8
                    (\domains scopes recordHeads scopeHeads domainHeads statusHeads candidates conflictSummaries ->
                        { base | domains = domains, scopes = scopes, recordHeads = recordHeads, scopeHeads = scopeHeads, domainHeads = domainHeads, statusHeads = statusHeads, candidates = candidates, conflicts = conflictSummaries })
                    (optional "domains" (D.list D.string) [])
                    (optional "applies_to" (D.list D.string) [])
                    (optional "record_heads" (D.list D.string) [])
                    (optional "scope_heads" (D.list D.string) [])
                    (optional "domain_heads" (D.list D.string) [])
                    (optional "status_heads" (D.list D.string) [])
                    candidateSet
                    (optional "conflicts" (D.list D.string) [])
                |> D.andThen
                    (\extended ->
                        D.map4
                            (\paths operations originDetails required ->
                                { extended | sourcePaths = paths, operations = operations, provenance = originDetails, resolutionRequired = required })
                            (optional "source_paths" (D.list D.string) [])
                            (optional "operations" (D.list operation) [])
                            provenanceSet
                            (D.field "resolution_required" D.bool)
                    )
            )


candidateSet : Decoder CandidateSet
candidateSet =
    D.map3 CandidateSet
        (optional "candidate_records" (D.list candidateRecord) [])
        (optional "candidate_scopes" (D.list candidateScope) [])
        (optional "candidate_domains" (D.list candidateDomain) [])


candidateRecord : Decoder CandidateRecord
candidateRecord =
    D.map4 (\id title summary path -> CandidateRecord id title summary "" path)
        (D.field "record" D.string)
        (D.field "title" D.string)
        (D.field "summary" D.string)
        (D.field "path" D.string)


candidateScope : Decoder CandidateRecord
candidateScope =
    D.map3 (\id scopes path -> CandidateRecord id "Scope candidate" (String.join ", " scopes) "" path)
        (D.field "connection" D.string)
        (D.field "applies_to" (D.list D.string))
        (D.field "path" D.string)


candidateDomain : Decoder CandidateRecord
candidateDomain =
    D.map3 (\id domains path -> CandidateRecord id "Domain candidate" (String.join ", " domains) "" path)
        (D.field "connection" D.string)
        (D.field "domains" (D.list D.string))
        (D.field "path" D.string)


operation : Decoder Operation
operation =
    D.map3 Operation
        (D.field "operation" D.string)
        (D.field "items" (D.list operationItem))
        (D.field "provenance" (D.nullable provenance))


operationItem : Decoder OperationItem
operationItem =
    D.map4 (\id kind eventName path ->
        { id = id, kind = kind, event = eventName, path = path, title = Nothing, summary = Nothing, body = Nothing, rationale = Nothing, relation = Nothing, rawSemantic = Nothing, domains = [], scopes = [], state = Nothing, replacement = Nothing, added = [], removed = [], refinements = [], parents = [], diffs = [] })
        (D.field "item" D.string)
        (D.field "type" D.string)
        (D.field "event" D.string)
        (D.field "path" D.string)
        |> D.andThen
            (\base ->
                D.map5
                    (\title summary body rationale domains ->
                        { base | title = title, summary = summary, body = body, rationale = rationale, domains = domains })
                    (optional "title" (D.nullable D.string) Nothing)
                    (optional "summary" (D.nullable D.string) Nothing)
                    (optional "body" (D.nullable D.string) Nothing)
                    (optional "rationale" (D.nullable D.string) Nothing)
                    (optional "domains" (D.list D.string) [])
                |> D.andThen
                    (\extended ->
                        D.map6
                            (\scopes state replacement added removed refinements ->
                                { extended | scopes = scopes, state = state, replacement = replacement, added = added, removed = removed, refinements = refinements })
                            (optionalAt [ "metadata", "applies_to" ] (D.list D.string) [])
                            (optionalAt [ "metadata", "state" ] (D.nullable D.string) Nothing)
                            (optionalAt [ "metadata", "replacement" ] (D.nullable D.string) Nothing)
                            (optionalAt [ "metadata", "added" ] (D.list D.string) [])
                            (optionalAt [ "metadata", "removed" ] (D.list D.string) [])
                            (optionalAt [ "metadata", "refinements" ] (D.list D.string) [])
                    )
                |> D.andThen
                    (\extended ->
                        D.map4
                            (\relation rawSemantic parents diffs ->
                                { extended | relation = relation, rawSemantic = rawSemantic, parents = parents, diffs = diffs })
                            (optional "relation" (D.nullable D.string) Nothing)
                            (optional "raw_semantic" (D.nullable D.string) Nothing)
                            (optional "parents" (D.list D.string) [])
                            (optional "diffs" (D.list parentDiff) [])
                    )
            )


parentDiff : Decoder String
parentDiff =
    D.map2 (\parent diff -> parent ++ "\n" ++ diff)
        (D.field "parent" D.string)
        (D.field "diff" D.string)


provenanceSet : Decoder (List Provenance)
provenanceSet =
    optional "provenance" (D.dict (D.nullable provenance)) Dict.empty
        |> D.map (Dict.values >> List.filterMap identity)


provenance : Decoder Provenance
provenance =
    D.map5 (\actor model claimedAt basis operationId ->
        { actor = actor, model = model, claimedAt = claimedAt, basis = basis, operation = operationId, inputDigest = Nothing, promptDigest = Nothing, contextDigest = Nothing, introductions = [], originalCommits = [], placements = [], lineLandings = [] })
        (D.field "actor" D.string)
        (optional "model" (D.nullable D.string) Nothing)
        (D.field "claimed_at" D.string)
        (D.field "basis" D.string)
        (D.field "operation" D.string)
        |> D.andThen
            (\base ->
                D.map5
                    (\input prompt context placements landings ->
                        { base | inputDigest = input, promptDigest = prompt, contextDigest = context, placements = placements, lineLandings = landings })
                    (optional "input_digest" (D.nullable D.string) Nothing)
                    (optional "prompt_digest" (D.nullable D.string) Nothing)
                    (optional "context_digest" (D.nullable D.string) Nothing)
                    (optional "placements" (D.list placement) [])
                    (optional "line_landings" (D.list landing) [])
                |> D.andThen
                    (\extended ->
                        D.map4
                            (\topIntroductions whenIntroductions topOriginals whenOriginals ->
                                { extended | introductions = topIntroductions ++ whenIntroductions, originalCommits = topOriginals ++ whenOriginals })
                            (optional "introductions" (D.list D.string) [])
                            (optionalAt [ "when", "introductions" ] (D.list D.string) [])
                            (optional "original_commits" (D.list D.string) [])
                            (optionalAt [ "when", "original_operation_commits" ] (D.list D.string) [])
                    )
            )


placement : Decoder String
placement =
    D.map3 (\classification commit subject -> classification ++ " · " ++ commit ++ " · " ++ subject)
        (D.field "classification" D.string)
        (D.field "commit" D.string)
        (D.field "subject" D.string)
        |> D.andThen
            (\base ->
                D.map4
                    (\authored committed parents reachable ->
                        base ++ " · authored " ++ authored ++ " · committed " ++ committed ++ " · parents " ++ String.join ", " parents ++ " · " ++ (if reachable then "reachable" else "unreachable"))
                    (D.field "authored_at" D.string)
                    (D.field "committed_at" D.string)
                    (D.field "parents" (D.list D.string))
                    (D.field "reachable" D.bool)
            )


landing : Decoder String
landing =
    D.map3 (\ref commit line -> ref ++ " · " ++ commit ++ " · " ++ line)
        (D.field "ref" D.string)
        (D.field "commit" D.string)
        (D.field "line" D.string)
        |> D.andThen
            (\base ->
                D.map (\complete -> base ++ " · " ++ (if complete then "complete landing" else "partial landing"))
                    (D.field "complete" D.bool)
            )


history : Decoder HistoryWindow
history =
    D.map4 HistoryWindow
        (D.field "as_of" D.string)
        (D.field "limit" D.int)
        (D.field "truncated" D.bool)
        (D.field "operations" (D.list historyItem))


historyItem : Decoder HistoryItem
historyItem =
    D.map8 (\adr title label actor claimedAt operationId reason commit ->
        { adr = adr, title = title, label = label, actor = actor, claimedAt = claimedAt, operation = operationId, reason = reason, commit = commit, changes = [] })
        (D.field "adr" D.string)
        (D.field "title" D.string)
        (D.field "label" D.string)
        (D.field "actor" D.string)
        (D.field "claimed_at" D.string)
        (D.field "operation" D.string)
        (optional "reason" (D.nullable D.string) Nothing)
        (optional "commit" (D.nullable D.string) Nothing)
        |> D.andThen (\base -> D.map (\changes -> { base | changes = changes }) (D.field "changes" (D.list D.string)))


comparison : Decoder CompareWindow
comparison =
    D.map3 CompareWindow
        (D.field "from" D.string)
        (D.field "to" D.string)
        (D.field "entries" (D.list compareEntry))


compareEntry : Decoder CompareEntry
compareEntry =
    D.map6 CompareEntry
        (D.field "adr" D.string)
        (D.field "title" D.string)
        (D.field "kind" D.string)
        (D.field "before" (D.nullable compareSnapshot))
        (D.field "after" (D.nullable compareSnapshot))
        (D.field "changes" (D.list compareChange))


compareChange : Decoder CompareChange
compareChange =
    D.map4 CompareChange
        (D.field "field" D.string)
        (optional "before" readableValue "")
        (optional "after" readableValue "")
        (optional "diff" (D.nullable D.string) Nothing)


readableValue : Decoder String
readableValue =
    D.oneOf
        [ D.string
        , D.map (String.join ", ") (D.list D.string)
        , D.map (\value -> if value then "true" else "false") D.bool
        , D.map String.fromInt D.int
        , D.null ""
        ]


compareSnapshot : Decoder CompareSnapshot
compareSnapshot =
    D.map6 CompareSnapshot
        (D.field "title" D.string)
        (D.field "summary" D.string)
        (D.field "body" D.string)
        (D.field "status" D.string)
        (D.field "domains" (D.list D.string))
        (D.field "applies_to" (D.list D.string))


conflicts : Decoder ConflictWindow
conflicts =
    D.map ConflictWindow (D.field "conflicts" (D.list conflictEntry))


conflictEntry : Decoder ConflictEntry
conflictEntry =
    D.map4 ConflictEntry
        (D.field "adr" D.string)
        (D.field "code" D.string)
        (D.field "summaries" (D.list D.string))
        (D.field "candidates" (D.list conflictCandidate))


conflictCandidate : Decoder ConflictCandidate
conflictCandidate =
    D.map3 ConflictCandidate
        (D.field "axis" D.string)
        (D.field "heads" (D.list D.string))
        (D.field "summary" D.string)


doctor : Decoder Doctor
doctor =
    D.map2 (\_ issues -> Doctor issues)
        (D.field "ok" D.bool)
        (D.field "issues" (D.list issue))


issue : Decoder Issue
issue =
    D.map4 Issue
        (D.field "severity" D.string)
        (D.field "code" D.string)
        (D.field "message" D.string)
        (optional "path" (D.nullable D.string) Nothing)


mutation : Decoder MutationOutcome
mutation =
    D.map5 (\committed operationId commit adr indexed ->
        { committed = committed, operation = operationId, commit = commit, adr = adr, indexed = indexed, indexError = Nothing, publicationWarning = Nothing })
        (D.field "committed" D.bool)
        (D.field "operation" D.string)
        (D.field "commit" D.string)
        (D.field "adr" D.string)
        (D.field "indexed" D.bool)
        |> D.andThen
            (\base ->
                D.map2
                    (\indexError warning -> { base | indexError = indexError, publicationWarning = warning })
                    (optional "index_error" (D.nullable D.string) Nothing)
                    (optional "publication_warning" (D.nullable D.string) Nothing)
            )


event : Decoder Event
event =
    D.field "schema" D.string
        |> D.andThen
            (\schema ->
                if schema /= "adrai/events/v1" then D.fail "unsupported event schema"
                else
                    D.map4 Event
                        (D.field "generation" generation)
                        (D.field "as_of" asOf)
                        (D.at [ "event", "type" ] D.string)
                        (optionalAt [ "event", "facts" ] (D.list D.string) [])
            )


request : String -> String -> String -> Maybe E.Value -> E.Value
request requestId method path body =
    E.object
        [ ( "type", E.string "request" )
        , ( "request_id", E.string requestId )
        , ( "method", E.string method )
        , ( "path", E.string path )
        , ( "body", Maybe.withDefault E.null body )
        ]


optional : String -> Decoder a -> a -> Decoder a
optional key decoder fallback =
    optionalAt [ key ] decoder fallback


optionalAt : List String -> Decoder a -> a -> Decoder a
optionalAt path decoder fallback =
    D.value
        |> D.andThen
            (\whole ->
                case lookupAt path whole of
                    Err problem ->
                        D.fail problem

                    Ok Nothing ->
                        D.succeed fallback

                    Ok (Just raw) ->
                        case D.decodeValue decoder raw of
                            Ok decoded ->
                                D.succeed decoded

                            Err _ ->
                                D.fail ("invalid " ++ String.join "." path)
            )


lookupAt : List String -> D.Value -> Result String (Maybe D.Value)
lookupAt path whole =
    case path of
        [] ->
            Ok (Just whole)

        key :: rest ->
            case D.decodeValue (D.null ()) whole of
                Ok _ ->
                    Ok Nothing

                Err _ ->
                    case D.decodeValue (D.dict D.value) whole of
                        Err _ ->
                            Err ("invalid object before " ++ key)

                        Ok object ->
                            case Dict.get key object of
                                Nothing ->
                                    Ok Nothing

                                Just value ->
                                    lookupAt rest value
