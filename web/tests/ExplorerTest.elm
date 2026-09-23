module ExplorerTest exposing (tests)

import Api
import Dict
import Expect
import FixtureData
import Html.Attributes as Attr
import Json.Decode as D
import Json.Encode as E
import Main
import Route
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import View.Forms as Forms


tests : Test
tests =
    describe "production Elm explorer"
        [ describe "shared serializer fixtures"
            [ fixtureTest "repository" Api.repository
            , fixtureTest "rich_resolved" Api.inspection
            , fixtureTest "rich_conflicted" Api.inspection
            , fixtureTest "exploded" Api.inspection
            , fixtureTest "exploded_conflicted" Api.inspection
            , fixtureTest "exploded_raw" Api.inspection
            , fixtureTest "search_blank" Api.search
            , fixtureTest "search_ranked" Api.search
            , fixtureTest "search_conflicted" Api.search
            , fixtureTest "relevant_committed" Api.relevant
            , fixtureTest "relevant_worktree" Api.relevant
            , fixtureTest "history" Api.history
            , fixtureTest "compare" Api.comparison
            , fixtureTest "conflicts" Api.conflicts
            , fixtureTest "doctor" Api.doctor
            , fixtureTest "mutation_create" Api.mutation
            , fixtureTest "mutation_amend" Api.mutation
            , fixtureTest "mutation_scope" Api.mutation
            , fixtureTest "mutation_domain" Api.mutation
            , fixtureTest "mutation_obsolete" Api.mutation
            , fixtureTest "mutation_reactivate" Api.mutation
            , fixtureTest "mutation_warning" Api.mutation
            , test "errors retain typed metadata" <|
                \_ ->
                    case fixture "error" Api.failure of
                        Ok failure -> Expect.equal "0" failure.metadata.generation
                        Err problem -> Expect.fail problem
            , test "large and max event generations decode" <|
                \_ ->
                    case ( fixture "event_large" Api.event, fixture "event_max" Api.event ) of
                        ( Ok large, Ok maximum ) ->
                            Expect.equal GT (Api.compareGeneration maximum.generation large.generation)
                        _ ->
                            Expect.fail "event fixtures must decode"
            , test "genuine conflicted rich heads join exploded candidate bodies at one revision" <|
                \_ ->
                    case ( fixture "rich_conflicted" (Api.response Api.inspection), fixture "exploded_conflicted" (Api.response Api.inspection) ) of
                        ( Ok rich, Ok exploded ) ->
                            let
                                heads = rich.data.recordHeads
                                candidates = List.map .id rich.data.candidates.records
                                items = List.concatMap .items exploded.data.operations
                                itemIds = List.map .id items
                                inspected = rich.data
                                joined = { inspected | operations = exploded.data.operations }
                                base = Tuple.first (Main.init { hasCredential = False })
                                rendered = Query.fromHtml (Main.view { base | inspection = Just joined }) |> Query.find [ Selector.id "inspector-pane" ]
                            in
                            if rich.data.adr /= exploded.data.adr || rich.data.asOf /= exploded.data.asOf || rich.data.stateToken /= exploded.data.stateToken || not rich.data.resolutionRequired || List.length heads /= 2 || not (List.all (\head -> List.member head candidates && List.member head itemIds) heads) then
                                Expect.fail "real conflict heads, candidate IDs, and operation items disagree"
                            else
                                rendered |> Query.has [ Selector.text "Retain this candidate" ]

                        _ -> Expect.fail "both real conflicted views must decode"
            ]
        , describe "strict Word64 generations"
            [ test "rejects every invalid wire form" <|
                \_ ->
                    let
                        invalid =
                            [ "0", "-1", "+1", "01", "1.0", "18446744073709551616", "", " 1", "1 " ]

                        encoded =
                            List.map
                                (\raw ->
                                    if raw == "0" then
                                        "0"
                                    else
                                        E.encode 0 (E.string raw)
                                )
                                invalid
                    in
                    Expect.equal True (List.all (D.decodeString Api.generation >> isError) encoded)
            , test "compares beyond JavaScript safe integer exactly" <|
                \_ ->
                    Expect.equal GT (Api.compareGeneration "9007199254740993" "9007199254740992")
            , test "numeric metadata generation fails before a state transition" <|
                \_ ->
                    Expect.equal True
                        (isError
                            (D.decodeString
                                (Api.response D.value)
                                "{\"schema\":\"adrai/api/v1\",\"metadata\":{\"generation\":9007199254740993,\"as_of\":{\"kind\":\"unavailable\",\"reason\":\"test\"}},\"data\":{}}"
                            )
                        )
            , test "present malformed candidate, head, and provenance fields fail" <|
                \_ ->
                    let
                        badCases =
                            [ ( "candidate_records", E.string "wrong" )
                            , ( "record_heads", E.string "wrong" )
                            , ( "provenance", E.string "wrong" )
                            ]
                    in
                    Expect.equal True
                        (List.all
                            (\field -> isError (D.decodeValue Api.inspection (inspectionWith field)))
                            badCases
                        )
            , test "collapsed and exploded views reject missing required fields and unknown discriminants" <|
                \_ ->
                    let
                        missingTitle =
                            case fixtureValue "rich_resolved" of
                                Just value ->
                                    case D.decodeValue (D.at [ "data" ] (D.dict D.value)) value of
                                        Ok fields -> E.object (Dict.toList (Dict.remove "title" fields))
                                        Err _ -> E.null
                                Nothing -> E.null
                        unknown = inspectionWith ( "view", E.string "other" )
                    in
                    Expect.equal True
                        (isError (D.decodeValue Api.inspection missingTitle)
                            && isError (D.decodeValue Api.inspection unknown))
            , test "doctor empty object fails instead of reporting no issues" <|
                \_ -> Expect.equal True (isError (D.decodeValue Api.doctor (E.object [])))
            , test "inspection requires typed resolution and explicit conflict state" <|
                \_ ->
                    let
                        missingCollapsedFlag = inspectionWithout "rich_resolved" "resolution_required"
                        malformedCollapsedResolution = inspectionWith ( "resolution", E.object [] )
                        missingExplodedResolution = inspectionWithout "exploded" "resolution"
                        malformedExplodedResolution = inspectionSet "exploded" ( "resolution", E.list E.string [] )
                    in
                    Expect.equal True
                        (List.all
                            (D.decodeValue Api.inspection >> isError)
                            [ missingCollapsedFlag, malformedCollapsedResolution, missingExplodedResolution, malformedExplodedResolution ]
                        )
            ]
        , describe "typed checked bodies"
            [ test "create omits ADR state token" <|
                \_ ->
                    case Forms.build (ready Forms.Create) of
                        Ok body ->
                            Expect.equal True (isError (D.decodeValue (D.field "state_token" D.string) body))
                        Err problem -> Expect.fail problem
            , test "amend has change summary and reviewed content" <|
                \_ -> expectField (ready Forms.Amend) "change_summary" "reviewed decision"
            , test "scope delta has add/remove and no patterns" <|
                \_ -> expectMode (ready Forms.Scope) "delta" "add"
            , test "scope reviewed has patterns" <|
                \_ -> expectMode (withMode "reviewed" (ready Forms.Scope)) "reviewed" "patterns"
            , test "domain delta has add/remove" <|
                \_ -> expectMode (ready Forms.Domain) "delta" "add"
            , test "domain reviewed has domains" <|
                \_ -> expectMode (withMode "reviewed" (ready Forms.Domain)) "reviewed" "domains"
            , test "domain refine has refinements" <|
                \_ -> expectMode (withMode "refine" (ready Forms.Domain)) "refine" "refinements"
            , test "obsolete has resolve and replacement" <|
                \_ -> expectField (ready Forms.Obsolete) "replacement" "ADR-2"
            , test "reactivate has resolve" <|
                \_ ->
                    case Forms.build (ready Forms.Reactivate) of
                        Ok body -> Expect.equal (Ok True) (D.decodeValue (D.field "resolve" D.bool) body)
                        Err problem -> Expect.fail problem
            , test "ordinary create and amend body text is canonical for the server" <|
                \_ ->
                    let
                        from action =
                            case Forms.build (ready action) of
                                Ok body -> D.decodeValue (D.field "body" D.string) body |> Result.mapError D.errorToString
                                Err problem -> Err problem
                    in
                    Expect.equal ( Ok "Decision body\n", Ok "Decision body\n" )
                        ( from Forms.Create, from Forms.Amend )
            , test "stale draft refuses submission until exact inspection adoption" <|
                \_ ->
                    let
                        stale =
                            Forms.markStale (ready Forms.Amend)
                    in
                    case ( fixture "repository" (Api.response Api.repository), fixture "rich_resolved" (Api.response Api.inspection) ) of
                        ( Ok repository, Ok inspection ) ->
                            case Forms.adopt repository.data inspection.data { stale | target = inspection.data.adr } of
                                Ok adopted ->
                                    if adopted.basisHead == repository.data.head && adopted.stateToken == inspection.data.stateToken then
                                        Expect.equal False adopted.stale
                                    else
                                        Expect.fail "tokens were not adopted from exact current sources"
                                Err problem -> Expect.fail problem
                        _ -> Expect.fail "repository and inspection fixtures must decode"
            , test "editing and refreshing preserves original reviewed values and candidate heads" <|
                \_ ->
                    case ( fixture "repository" (Api.response Api.repository), fixture "rich_resolved" (Api.response Api.inspection) ) of
                        ( Ok repository, Ok inspection ) ->
                            let
                                started = Forms.begin Forms.Amend repository.data (Just inspection.data) Forms.initial
                                edited = Forms.change Forms.Title "my unsent title" started
                                stale = Forms.markStale edited
                            in
                            case stale.original of
                                Just original ->
                                    Expect.equal True
                                        (stale.title == "my unsent title"
                                            && original.title == inspection.data.title
                                            && original.heads == inspection.data.recordHeads ++ inspection.data.scopeHeads ++ inspection.data.domainHeads ++ inspection.data.statusHeads)
                                Nothing -> Expect.fail "original review was not stored"
                        _ -> Expect.fail "review fixtures must decode"
            , test "actions render original reviewed content beside a changed fresh inspection" <|
                \_ ->
                    case ( fixture "repository" (Api.response Api.repository), fixture "rich_resolved" (Api.response Api.inspection) ) of
                        ( Ok repository, Ok inspection ) ->
                            let
                                started = Forms.begin Forms.Amend repository.data (Just inspection.data) Forms.initial
                                edited = Forms.change Forms.Title "unsent local title" started
                                inspected = inspection.data
                                fresh = { inspected | title = "new server title" }
                                base = Tuple.first (Main.init { hasCredential = True })
                                model =
                                    { base
                                        | repository = Just repository.data
                                        , repositoryReady = True
                                        , inspection = Just fresh
                                        , collapsedReady = True
                                        , explodedReady = True
                                        , selectedAdr = Just inspection.data.adr
                                        , selectedRevision = Just repository.data.head
                                        , draft = edited
                                    }
                                rendered = Query.fromHtml (Main.view model) |> Query.find [ Selector.id "actions-pane" ]
                            in
                            Expect.all
                                [ \html -> html |> Query.has [ Selector.text "Original reviewed state" ]
                                , \html -> html |> Query.has [ Selector.text ("Title: " ++ inspection.data.title) ]
                                , \html -> html |> Query.has [ Selector.text "Current freshly inspected state" ]
                                , \html -> html |> Query.has [ Selector.text "Title: new server title" ]
                                ]
                                rendered

                        _ -> Expect.fail "review fixtures must decode"
            ]
        , describe "request state and windows"
            [ test "fixed 250-item window pages without changing source" <|
                \_ ->
                    let
                        items = List.range 1 250
                        allPages = List.concat (List.map (\page -> Route.pageSlice page items) [ 0, 1, 2 ])
                    in
                    Expect.equal items allPages
            , test "production 250-hit rendering pages locally without issuing HTTP" <|
                \_ ->
                    case fixture "search_blank" (Api.response Api.search) of
                        Ok envelope ->
                            case List.head envelope.data.results of
                                Just example ->
                                    let
                                        hits = List.indexedMap (\index _ -> { example | title = "Window item " ++ String.fromInt (index + 1) }) (List.repeat 250 ())
                                        search = envelope.data
                                        base = Tuple.first (Main.init { hasCredential = False })
                                        loaded = { base | search = Just { search | results = hits, limit = 250 } }
                                        paged = Tuple.first (Main.update (Main.SetPage 1) loaded)
                                        rendered = Query.fromHtml (Main.view paged) |> Query.find [ Selector.id "context-pane" ]
                                    in
                                    if paged.page /= 1 || paged.nextRequest /= loaded.nextRequest || paged.pending /= loaded.pending then
                                        Expect.fail "local pagination changed the HTTP request state"
                                    else
                                        rendered |> Query.has [ Selector.text "Window item 101" ]

                                Nothing -> Expect.fail "real search fixture needs a serializer hit"

                        Err problem -> Expect.fail problem
            , test "window never requests or pages beyond 1000" <|
                \_ ->
                    let
                        initial = Tuple.first (Main.init { hasCredential = False })
                        query = initial.query
                        oversized = { query | limit = 2000 }
                        path = Route.queryPath oversized
                    in
                    Expect.equal True (String.contains "limit=1000" path && Route.pageCount (List.range 1 1000) == 10)
            , test "superseded query response cannot replace the new window" <|
                \_ ->
                    let
                        base = Tuple.first (Main.init { hasCredential = False })
                        next = Tuple.first (Main.update Main.Load base)
                        old = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-2" "search_ranked")) next)
                        current = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-3" "search_blank")) old)
                    in
                    Expect.equal True (old.search == Nothing && current.search /= Nothing)
            , test "superseded request failure cannot replace current view status" <|
                \_ ->
                    let
                        base = Tuple.first (Main.init { hasCredential = False })
                        next = Tuple.first (Main.update Main.Load base)
                        failed = Tuple.first (Main.update (Main.FromJs (transportFailure "ui-2")) next)
                    in
                    Expect.equal Nothing failed.error
            , test "superseded terminal read blocks later current success without a socket" <|
                \_ ->
                    let
                        base = Tuple.first (Main.init { hasCredential = False })
                        edited = Tuple.first (Main.update (Main.EditDraft Forms.Title "unsent title") base)
                        waiting = Tuple.first (Main.update Main.Load edited)
                        exhausted = Tuple.first (Main.update (Main.FromJs (transportTypedError "ui-2" 503 "generation-exhausted")) waiting)
                        lateSuccess = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-3" "search_blank")) exhausted)
                        afterLoad = Tuple.first (Main.update Main.Load lateSuccess)
                        afterSubmit = Tuple.first (Main.update Main.Submit afterLoad)
                    in
                    Expect.equal True
                        (exhausted.terminalExhausted
                            && not lateSuccess.repositoryReady
                            && lateSuccess.search == Nothing
                            && lateSuccess.viewStale
                            && lateSuccess.draft.title == "unsent title"
                            && lateSuccess.draft.stale
                            && afterLoad.nextRequest == lateSuccess.nextRequest
                            && String.contains "Restart the web server" afterSubmit.operationStatus)
            , test "one busy inspection retries its exact read without losing the successful sibling" <|
                \_ ->
                    let
                        selected = inspectedAfter []
                        edited = Tuple.first (Main.update (Main.EditDraft Forms.Title "unsent inspection draft") selected)
                    in
                    case Dict.get "ui-3" edited.pending of
                        Nothing ->
                            Expect.fail "collapsed inspection request must be pending"

                        Just collapsed ->
                            let
                                busy = Tuple.first (Main.update (Main.FromJs (transportTypedError "ui-3" 503 "repository-busy")) edited)
                                sibling = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-4" "exploded")) busy)
                                retried = Tuple.first (Main.update (Main.RetryRead "ui-3" collapsed) sibling)
                                settled = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-5" "rich_resolved")) retried)
                            in
                            Expect.equal True
                                (sibling.explodedReady
                                    && not sibling.collapsedReady
                                    && sibling.inspection /= Nothing
                                    && retried.epoch == selected.epoch
                                    && retried.repositoryReady
                                    && retried.explodedReady
                                    && retried.nextRequest == selected.nextRequest + 1
                                    && settled.collapsedReady
                                    && settled.explodedReady
                                    && Maybe.map (.operations >> List.isEmpty) settled.inspection == Just False
                                    && settled.draft.title == "unsent inspection draft"
                                    && Dict.get "collapsed" settled.inspectionIssues == Nothing)
            , test "capped inspection busy error remains visible after sibling success and can be retried" <|
                \_ ->
                    let
                        selected = inspectedAfter []
                    in
                    case Dict.get "ui-3" selected.pending of
                        Nothing ->
                            Expect.fail "collapsed inspection request must be pending"

                        Just first ->
                            let
                                busy1 = Tuple.first (Main.update (Main.FromJs (transportTypedError "ui-3" 503 "repository-busy")) selected)
                                retry1 = Tuple.first (Main.update (Main.RetryRead "ui-3" first) busy1)
                                busy2 = Tuple.first (Main.update (Main.FromJs (transportTypedError "ui-5" 503 "repository-busy")) retry1)
                            in
                            case Dict.get "ui-5" retry1.pending of
                                Nothing ->
                                    Expect.fail "first inspection retry must be pending"

                                Just second ->
                                    let
                                        retry2 = Tuple.first (Main.update (Main.RetryRead "ui-5" second) busy2)
                                        capped = Tuple.first (Main.update (Main.FromJs (transportTypedError "ui-6" 503 "repository-busy")) retry2)
                                        sibling = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-4" "exploded")) capped)
                                        rendered = Query.fromHtml (Main.view sibling) |> Query.find [ Selector.id "inspector-pane" ]
                                        manual = Tuple.first (Main.update (Main.RetryInspection first.kind) sibling)
                                    in
                                    Expect.all
                                        [ \_ -> Expect.equal True
                                            (sibling.explodedReady
                                                && not sibling.collapsedReady
                                                && sibling.nextRequest == retry2.nextRequest
                                                && sibling.error == Nothing
                                                && Dict.get "collapsed" sibling.inspectionIssues /= Nothing
                                                && manual.nextRequest == sibling.nextRequest + 1
                                                && manual.explodedReady
                                                && Dict.get "collapsed" manual.inspectionIssues == Nothing)
                                        , \_ -> rendered |> Query.has [ Selector.text "Repository remains busy" ]
                                        , \_ -> rendered |> Query.has [ Selector.text "Retry decision inspection" ]
                                        ]
                                        ()
            , test "delayed inspection retry drops superseded selection and invalidation" <|
                \_ ->
                    let
                        selected = inspectedAfter []
                    in
                    case Dict.get "ui-3" selected.pending of
                        Nothing ->
                            Expect.fail "collapsed inspection request must be pending"

                        Just collapsed ->
                            let
                                busy = Tuple.first (Main.update (Main.FromJs (transportTypedError "ui-3" 503 "repository-busy")) selected)
                                changed = Tuple.first (Main.update (Main.SelectAdr "A22222222222222222222222222") busy)
                                ignoredSelection = Tuple.first (Main.update (Main.RetryRead "ui-3" collapsed) changed)
                                invalidated = Tuple.first (Main.update (Main.FromJs (transportEvent "event_large")) busy)
                                ignoredInvalidation = Tuple.first (Main.update (Main.RetryRead "ui-3" collapsed) invalidated)
                            in
                            Expect.equal True
                                (ignoredSelection.nextRequest == changed.nextRequest
                                    && ignoredSelection.selectedAdr == changed.selectedAdr
                                    && Dict.get "collapsed" ignoredSelection.inspectionIssues == Nothing
                                    && ignoredInvalidation.nextRequest == invalidated.nextRequest
                                    && ignoredInvalidation.epoch == invalidated.epoch)
            , test "unrelated large HTTP generation cannot suppress later socket invalidation" <|
                \_ ->
                    let
                        base = Tuple.first (Main.init { hasCredential = False })
                        afterRepository = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-1" "repository")) base)
                        afterEvent = Tuple.first (Main.update (Main.FromJs (transportEvent "event_large")) afterRepository)
                    in
                    Expect.equal True (afterEvent.epoch > afterRepository.epoch && afterEvent.viewStale)
            , test "socket invalidation discards an older in-flight query response" <|
                \_ ->
                    let
                        base = Tuple.first (Main.init { hasCredential = False })
                        afterEvent = Tuple.first (Main.update (Main.FromJs (transportEvent "event_large")) base)
                        afterOldQuery = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-2" "search_blank")) afterEvent)
                    in
                    Expect.equal Nothing afterOldQuery.search
            , test "read-only reload never enables checked submission" <|
                \_ ->
                    let
                        base = Tuple.first (Main.init { hasCredential = False })
                        afterSubmit = Tuple.first (Main.update Main.Submit base)
                    in
                    Expect.equal True (String.contains "bootstrap" afterSubmit.operationStatus && not afterSubmit.hasCredential)
            , test "late known commit survives newer invalidation and a newer draft" <|
                \_ ->
                    let
                        pending = createPending
                        newer = Tuple.first (Main.update (Main.EditDraft Forms.Title "new unsent title") pending)
                        invalidated = Tuple.first (Main.update (Main.FromJs (transportEvent "event_large")) newer)
                        settled = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-3" "mutation_create")) invalidated)
                    in
                    Expect.equal True
                        (String.contains "Committed" settled.operationStatus
                            && settled.draft.title == "new unsent title"
                            && settled.mutationPending == Nothing)
            , test "network ambiguity preserves draft and never confirms success" <|
                \_ ->
                    let
                        pending = createPending
                        failed = Tuple.first (Main.update (Main.FromJs (transportFailure "ui-3")) pending)
                    in
                    Expect.equal True
                        (String.contains "uncertain" failed.operationStatus
                            && failed.draft.title == pending.draft.title
                            && failed.mutationPending == Nothing)
            , test "typed authentication rejection gives re-entry, never ambiguity" <|
                \_ ->
                    let
                        pending = createPending
                        rejected = Tuple.first (Main.update (Main.FromJs (transportStatus "ui-3" 401 "error")) pending)
                    in
                    Expect.equal True
                        (String.contains "bootstrap" rejected.operationStatus
                            && not rejected.hasCredential
                            && not (String.contains "uncertain" rejected.operationStatus))
            , test "typed validation and authorization rejections remain definite" <|
                \_ ->
                    let
                        badRequest = Tuple.first (Main.update (Main.FromJs (transportTypedError "ui-3" 400 "invalid-body")) createPending)
                        forbidden = Tuple.first (Main.update (Main.FromJs (transportTypedError "ui-3" 403 "forbidden")) createPending)
                    in
                    Expect.equal True
                        (String.contains "invalid-body" badRequest.operationStatus
                            && String.contains "forbidden" forbidden.operationStatus
                            && not (String.contains "uncertain" badRequest.operationStatus)
                            && not (String.contains "uncertain" forbidden.operationStatus))
            , test "typed 409 keeps draft content and requires a fresh review" <|
                \_ ->
                    let
                        pending = createPending
                        rejected = Tuple.first (Main.update (Main.FromJs (transportTypedError "ui-3" 409 "state-conflict")) pending)
                    in
                    Expect.equal True
                        (rejected.draft.stale
                            && rejected.draft.title == pending.draft.title
                            && String.contains "state-conflict" rejected.operationStatus
                            && not (String.contains "uncertain" rejected.operationStatus))
            , test "unexpected 503 is not treated as a retryable busy response" <|
                \_ ->
                    let
                        base = Tuple.first (Main.init { hasCredential = False })
                        rejected = Tuple.first (Main.update (Main.FromJs (transportStatus "ui-2" 503 "error_exhausted")) base)
                    in
                    Expect.equal True
                        (rejected.busyRetries == Dict.empty
                            && rejected.viewStale
                            && rejected.error /= Nothing)
            , test "HTTP generation exhaustion disables freshness and queued retry" <|
                \_ ->
                    let
                        base = inspectedAfter [ "rich_resolved", "exploded" ]
                        requestId = "ui-" ++ String.fromInt base.nextRequest
                        waiting = Tuple.first (Main.update Main.Load base)
                        rejected = Tuple.first (Main.update (Main.FromJs (transportTypedError requestId 503 "generation-exhausted")) waiting)
                        afterReconnect = Tuple.first (Main.update Main.Reconnect rejected)
                    in
                    case Dict.get requestId waiting.pending of
                        Nothing ->
                            Expect.fail "the read must be pending before terminal rejection"

                        Just pending ->
                            let
                                afterRetry = Tuple.first (Main.update (Main.RetryRead requestId pending) afterReconnect)
                            in
                            Expect.equal True
                                (rejected.terminalExhausted
                                    && not rejected.repositoryReady
                                    && not rejected.collapsedReady
                                    && not rejected.explodedReady
                                    && rejected.draft.stale
                                    && String.contains "Restart the web server" rejected.operationStatus
                                    && afterRetry.nextRequest == rejected.nextRequest)
            , test "terminal WebSocket close preserves editable and original review values" <|
                \_ ->
                    let
                        inspected = inspectedAfter [ "exploded", "rich_resolved" ]
                        started = Tuple.first (Main.update (Main.StartAction Forms.Amend) inspected)
                        edited = Tuple.first (Main.update (Main.EditDraft Forms.Title "unsent amendment") started)
                        closed = Tuple.first (Main.update (Main.FromJs (socketExhausted)) edited)
                        afterSubmit = Tuple.first (Main.update Main.Submit closed)
                    in
                    Expect.equal True
                        (closed.terminalExhausted
                            && closed.draft.title == "unsent amendment"
                            && closed.draft.original == edited.draft.original
                            && closed.draft.stale
                            && closed.socketState == "unavailable"
                            && String.contains "Restart the web server" afterSubmit.operationStatus)
            , test "known committed warning retains both publication and index failures" <|
                \_ ->
                    let
                        settled = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-3" "mutation_warning")) createPending)
                    in
                    Expect.equal True
                        (settled.terminalExhausted
                            && String.contains "Committed" settled.operationStatus
                            && String.contains "Publication warning: generation-exhausted" settled.operationStatus
                            && String.contains "Index warning: post-commit index failed" settled.operationStatus)
            , test "reconnect attempts terminate visibly" <|
                \_ ->
                    let
                        base = Tuple.first (Main.init { hasCredential = True })
                        closed = List.foldl
                            (\_ current -> Tuple.first (Main.update (Main.FromJs (socketState "closed")) current))
                            base
                            (List.range 1 5)
                    in
                    Expect.equal "unavailable" closed.socketState
            , test "rendered inspector exposes provenance, placement, landing, and raw decision diff" <|
                \_ ->
                    case fixture "exploded_raw" (Api.response Api.inspection) of
                        Ok envelope ->
                            let
                                base = Tuple.first (Main.init { hasCredential = False })
                                model = { base | inspection = Just envelope.data }
                                rendered = Query.fromHtml (Main.view model) |> Query.find [ Selector.id "inspector-pane" ]
                            in
                            rendered |> Query.has [ Selector.text "complete landing" ]
                        Err problem ->
                            Expect.fail problem
            , test "freshness requires both inspection responses in either order" <|
                \_ ->
                    let
                        firstCollapsed = inspectedAfter [ "rich_resolved" ]
                        firstExploded = inspectedAfter [ "exploded" ]
                        both = inspectedAfter [ "exploded", "rich_resolved" ]
                        bothReverse = inspectedAfter [ "rich_resolved", "exploded" ]
                        earlyCollapsed = Tuple.first (Main.update (Main.StartAction Forms.Amend) firstCollapsed)
                        earlyExploded = Tuple.first (Main.update (Main.StartAction Forms.Amend) firstExploded)
                        started = Tuple.first (Main.update (Main.StartAction Forms.Amend) both)
                        startedReverse = Tuple.first (Main.update (Main.StartAction Forms.Amend) bothReverse)
                        stale = Tuple.first (Main.update Main.Refresh both)
                        blocked = Tuple.first (Main.update Main.AdoptTokens stale)
                        eventStale = Tuple.first (Main.update (Main.FromJs (transportEvent "event_large")) both)
                    in
                    Expect.equal True
                        (not firstCollapsed.explodedReady
                            && not firstExploded.collapsedReady
                            && both.collapsedReady
                            && both.explodedReady
                            && bothReverse.collapsedReady
                            && bothReverse.explodedReady
                            && earlyCollapsed.draft.target == ""
                            && earlyExploded.draft.target == ""
                            && started.draft.target == "A11111111111111111111111111"
                            && startedReverse.draft.target == "A11111111111111111111111111"
                            && not stale.repositoryReady
                            && not eventStale.repositoryReady
                            && not eventStale.collapsedReady
                            && not eventStale.explodedReady
                            && String.contains "fresh" blocked.operationStatus)
            , test "existing actions stay unavailable until exact primary inspection is ready" <|
                \_ ->
                    let
                        selected = inspectedAfter []
                        partial = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-4" "exploded")) selected)
                        earlyOption = Query.fromHtml (Main.view partial) |> Query.find [ Selector.id "action" ] |> Query.find [ Selector.attribute (Attr.value "amend") ]
                        earlyAttempt = Tuple.first (Main.update (Main.StartAction Forms.Amend) partial)
                        inspectedReady = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-3" "rich_resolved")) partial)
                        started = Tuple.first (Main.update (Main.StartAction Forms.Amend) inspectedReady)
                        actions = Query.fromHtml (Main.view started) |> Query.find [ Selector.id "actions-pane" ]
                    in
                    Expect.all
                        [ \_ -> earlyOption |> Query.has [ Selector.attribute (Attr.disabled True) ]
                        , \_ -> Expect.equal True
                            (partial.explodedReady
                                && not partial.collapsedReady
                                && earlyAttempt.draft.action == Forms.Create
                                && earlyAttempt.draft.target == ""
                                && inspectedReady.collapsedReady
                                && inspectedReady.explodedReady
                                && started.draft.action == Forms.Amend
                                && started.draft.target == "A11111111111111111111111111"
                                && Maybe.map .title started.draft.original == Just "Café architecture v2"
                                && String.contains "Adopt the service architecture." (Maybe.withDefault "" (Maybe.map .body started.draft.original)))
                        , \_ -> actions |> Query.has [ Selector.text "Change summary" ]
                        ]
                        ()
            , test "a typed but wrong inspection view never marks the requested view fresh" <|
                \_ ->
                    let
                        base = Tuple.first (Main.init { hasCredential = True })
                        repository = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-1" "repository")) base)
                        searched = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-2" "search_blank")) repository)
                        selected = Tuple.first (Main.update (Main.SelectAdr "A11111111111111111111111111") searched)
                        wrongCollapsed = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-3" "exploded")) selected)
                        wrongExploded = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-4" "rich_resolved")) wrongCollapsed)
                    in
                    Expect.equal True (not wrongCollapsed.collapsedReady && not wrongExploded.explodedReady)
            , test "removed comparison renders before state and inspects from revision" <|
                \_ ->
                    let
                        base = Tuple.first (Main.init { hasCredential = False })
                        query = base.query
                        before = { title = "Removed decision", summary = "old summary", body = "old body", status = "active", domains = [ "data" ], scopes = [ "src/**" ] }
                        entry = { adr = "A11111111111111111111111111", title = "Removed decision", kind = "removed", before = Just before, after = Nothing, changes = [] }
                        comparison = { from = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", to = "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb", entries = [ entry ] }
                        model = { base | query = { query | view = Route.Compare }, comparison = Just comparison, selectedAdr = Just entry.adr }
                        selected = Tuple.first (Main.update (Main.SelectCompareAdr entry.adr comparison.from) model)
                    in
                    if selected.selectedRevision /= Just comparison.from then
                        Expect.fail "removed ADR did not select from revision"
                    else
                        Query.fromHtml (Main.view model)
                            |> Query.find [ Selector.id "inspector-pane" ]
                            |> Query.has [ Selector.text "old body" ]
            ]
        ]


fixtureTest : String -> D.Decoder a -> Test
fixtureTest key decoder =
    test (key ++ " decodes through production API") <|
        \_ ->
            case fixture key (Api.response decoder) of
                Ok _ -> Expect.pass
                Err problem -> Expect.fail problem


fixture : String -> D.Decoder a -> Result String a
fixture key decoder =
    D.decodeString (D.at [ "cases", key ] decoder) FixtureData.document
        |> Result.mapError D.errorToString


isError : Result x a -> Bool
isError result =
    case result of
        Ok _ -> False
        Err _ -> True


ready : Forms.Action -> Forms.Draft
ready action =
    let
        base = Forms.initial
    in
    { base
        | action = action
        , target = "ADR-1"
        , title = "Decision title"
        , summary = "Decision summary"
        , body = "Decision body"
        , changeSummary = "reviewed decision"
        , reason = "reviewed reason"
        , domains = "data\napi"
        , scopes = "src/**"
        , add = "src/new/**"
        , remove = "src/old/**"
        , refinements = "data>data/storage"
        , replacement = "ADR-2"
        , actorKind = "human"
        , actorId = "reviewer"
        , mode = "delta"
        , resolve = True
        , basisHead = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
        , basisRef = Just "refs/heads/main"
        , basisToken = "Rtoken"
        , stateToken = "Stoken"
        , reviewed = True
    }


withMode : String -> Forms.Draft -> Forms.Draft
withMode mode draft =
    { draft | mode = mode }


createPending : Main.Model
createPending =
    let
        base = Tuple.first (Main.init { hasCredential = True })
        repository = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-1" "repository")) base)
        actor = Tuple.first (Main.update (Main.EditDraft Forms.ActorId "reviewer") repository)
        titled = Tuple.first (Main.update (Main.EditDraft Forms.Title "first title") actor)
    in
    Tuple.first (Main.update Main.Submit titled)


inspectedAfter : List String -> Main.Model
inspectedAfter order =
    let
        base = Tuple.first (Main.init { hasCredential = True })
        repository = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-1" "repository")) base)
        searched = Tuple.first (Main.update (Main.FromJs (transportResponse "ui-2" "search_blank")) repository)
        selected = Tuple.first (Main.update (Main.SelectAdr "A11111111111111111111111111") searched)
    in
    List.foldl
        (\key current ->
            Tuple.first
                (Main.update
                    (Main.FromJs (transportResponse (if key == "exploded" then "ui-4" else "ui-3") key))
                    current
                )
        )
        selected
        order


expectField : Forms.Draft -> String -> String -> Expect.Expectation
expectField draft key expected =
    case Forms.build draft of
        Ok body -> Expect.equal (Ok expected) (D.decodeValue (D.field key D.string) body)
        Err problem -> Expect.fail problem


expectMode : Forms.Draft -> String -> String -> Expect.Expectation
expectMode draft mode required =
    case Forms.build draft of
        Ok body ->
            case ( D.decodeValue (D.field "mode" D.string) body, D.decodeValue (D.field required (D.list D.string)) body ) of
                ( Ok actual, Ok _ ) -> Expect.equal mode actual
                _ -> Expect.fail "mode or required variant field missing"
        Err problem -> Expect.fail problem


transportResponse : String -> String -> E.Value
transportResponse requestId key =
    transportStatus requestId 200 key


transportStatus : String -> Int -> String -> E.Value
transportStatus requestId status key =
    case fixtureValue key of
        Just body ->
            E.object
                [ ( "type", E.string "response" )
                , ( "request_id", E.string requestId )
                , ( "status", E.int status )
                , ( "body", body )
                ]
        Nothing -> E.null


transportTypedError : String -> Int -> String -> E.Value
transportTypedError requestId status code =
    case fixtureValue "error" of
        Just example ->
            case D.decodeValue (D.field "metadata" D.value) example of
                Ok metadata ->
                    E.object
                        [ ( "type", E.string "response" )
                        , ( "request_id", E.string requestId )
                        , ( "status", E.int status )
                        , ( "body"
                          , E.object
                                [ ( "schema", E.string "adrai/api/v1" )
                                , ( "metadata", metadata )
                                , ( "error", E.object [ ( "category", E.string "client" ), ( "status", E.int status ), ( "code", E.string code ), ( "message", E.string "request rejected" ) ] )
                                ]
                          )
                        ]

                Err _ -> E.null

        Nothing -> E.null


transportEvent : String -> E.Value
transportEvent key =
    case fixtureValue key of
        Just body -> E.object [ ( "type", E.string "event" ), ( "body", body ) ]
        Nothing -> E.null


transportFailure : String -> E.Value
transportFailure requestId =
    E.object
        [ ( "type", E.string "request-failed" )
        , ( "request_id", E.string requestId )
        , ( "message", E.string "Network failure" )
        ]


socketState : String -> E.Value
socketState state =
    E.object [ ( "type", E.string "socket-state" ), ( "state", E.string state ) ]


socketExhausted : E.Value
socketExhausted =
    E.object
        [ ( "type", E.string "socket-state" )
        , ( "state", E.string "unavailable" )
        , ( "reason", E.string "generation-exhausted" )
        ]


fixtureValue : String -> Maybe E.Value
fixtureValue key =
    D.decodeString (D.at [ "cases", key ] D.value) FixtureData.document
        |> Result.toMaybe


inspectionWith : ( String, E.Value ) -> E.Value
inspectionWith extra =
    inspectionSet "rich_resolved" extra


inspectionSet : String -> ( String, E.Value ) -> E.Value
inspectionSet fixtureKey extra =
    case fixtureValue fixtureKey of
        Just envelope ->
            case D.decodeValue (D.field "data" (D.dict D.value)) envelope of
                Ok fields ->
                    E.object (Dict.toList (Dict.insert (Tuple.first extra) (Tuple.second extra) fields))

                Err _ ->
                    E.null

        Nothing ->
            E.null


inspectionWithout : String -> String -> E.Value
inspectionWithout fixtureKey field =
    case fixtureValue fixtureKey of
        Just envelope ->
            case D.decodeValue (D.field "data" (D.dict D.value)) envelope of
                Ok fields -> E.object (Dict.toList (Dict.remove field fields))
                Err _ -> E.null

        Nothing -> E.null
