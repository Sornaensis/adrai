port module Main exposing (Model, Msg(..), init, update, maxPage, view, main)

import Api
import Browser
import Dict exposing (Dict)
import Html exposing (Html, article, button, details, div, fieldset, h1, h2, h3, h4, header, input, label, li, main_, option, p, pre, section, select, small, span, strong, summary, text, textarea, ul)
import Html.Attributes exposing (checked, class, disabled, for, id, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Decode as D
import Json.Encode as E
import Process
import Route
import Task
import View.Forms as Forms


port toJs : E.Value -> Cmd msg


port fromJs : (D.Value -> msg) -> Sub msg


type alias Flags =
    { hasCredential : Bool }


type RequestKind
    = RepositoryRead
    | QueryRead
    | CollapsedRead
    | ExplodedRead
    | MutationWrite


type alias Pending =
    { kind : RequestKind, context : String, epoch : Int, draftSerial : Int }


type alias Model =
    { hasCredential : Bool
    , repository : Maybe Api.Repository
    , repositoryReady : Bool
    , collapsedReady : Bool
    , explodedReady : Bool
    , query : Route.Query
    , pending : Dict String Pending
    , latest : Dict String String
    , nextRequest : Int
    , epoch : Int
    , watermark : String
    , socketState : String
    , terminalExhausted : Bool
    , reconnects : Int
    , busyRetries : Dict String Int
    , inspectionIssues : Dict String String
    , search : Maybe Api.SearchWindow
    , relevant : Maybe Api.RelevantWindow
    , history : Maybe Api.HistoryWindow
    , comparison : Maybe Api.CompareWindow
    , conflicts : Maybe Api.ConflictWindow
    , doctor : Maybe Api.Doctor
    , inspection : Maybe Api.Inspection
    , selectedAdr : Maybe String
    , selectedRevision : Maybe String
    , queryAsOf : Maybe String
    , page : Int
    , viewStale : Bool
    , error : Maybe String
    , draft : Forms.Draft
    , draftSerial : Int
    , mutationPending : Maybe String
    , operationStatus : String
    }


type Msg
    = FromJs D.Value
    | ChooseView Route.View
    | EditQuery String String
    | ToggleQuery String Bool
    | Load
    | SelectAdr String
    | SelectCompareAdr String String
    | SetPage Int
    | Refresh
    | StartAction Forms.Action
    | EditDraft Forms.Field String
    | ResolveDraft Bool
    | AdoptTokens
    | Submit
    | Reconnect
    | RetryRead String Pending
    | RetryInspection RequestKind


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = \_ -> fromJs FromJs
        , view = view
        }


initialQuery : Route.Query
initialQuery =
    { view = Route.Browse
    , revision = "HEAD"
    , text = ""
    , mode = "hybrid"
    , projection = "collapsed"
    , domain = ""
    , file = ""
    , actor = ""
    , since = ""
    , until = ""
    , includeObsolete = False
    , shallow = False
    , worktree = False
    , limit = 100
    , order = "newest"
    , adr = ""
    , compareFrom = "HEAD~1"
    , compareTo = "HEAD"
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        base =
            { hasCredential = flags.hasCredential
            , repository = Nothing
            , repositoryReady = False
            , collapsedReady = False
            , explodedReady = False
            , query = initialQuery
            , pending = Dict.empty
            , latest = Dict.empty
            , nextRequest = 1
            , epoch = 0
            , watermark = "0"
            , socketState = if flags.hasCredential then "connecting" else "unavailable"
            , terminalExhausted = False
            , reconnects = 0
            , busyRetries = Dict.empty
            , inspectionIssues = Dict.empty
            , search = Nothing
            , relevant = Nothing
            , history = Nothing
            , comparison = Nothing
            , conflicts = Nothing
            , doctor = Nothing
            , inspection = Nothing
            , selectedAdr = Nothing
            , selectedRevision = Nothing
            , queryAsOf = Nothing
            , page = 0
            , viewStale = True
            , error = Nothing
            , draft = Forms.initial
            , draftSerial = 0
            , mutationPending = Nothing
            , operationStatus = ""
            }

        ( withRepository, repositoryCommand ) =
            issue RepositoryRead "GET" Route.repository Nothing base

        ( withQuery, queryCommand ) =
            issue QueryRead "GET" (Route.queryPath initialQuery) Nothing withRepository
    in
    ( withQuery
    , Cmd.batch
        [ repositoryCommand
        , queryCommand
        , if flags.hasCredential then toJs (E.object [ ( "type", E.string "connect" ) ]) else Cmd.none
        ]
    )


requestKey : RequestKind -> String
requestKey kind =
    case kind of
        RepositoryRead -> "repository"
        QueryRead -> "query"
        CollapsedRead -> "collapsed"
        ExplodedRead -> "exploded"
        MutationWrite -> "mutation"


inspectionReady : Model -> Bool
inspectionReady model =
    not model.terminalExhausted
        && model.repositoryReady
        && model.collapsedReady
        && model.explodedReady
        && Maybe.map .view model.inspection == Just "collapsed"
        && model.selectedRevision == Maybe.map .head model.repository
        && model.selectedAdr == Maybe.map .adr model.inspection


isHistorical : Model -> Bool
isHistorical model =
    model.query.revision /= "HEAD"
        || (model.selectedAdr /= Nothing && model.selectedRevision /= Maybe.map .head model.repository)


issue : RequestKind -> String -> String -> Maybe E.Value -> Model -> ( Model, Cmd Msg )
issue kind method path body model =
    if model.terminalExhausted then
        ( model, Cmd.none )

    else
        let
            identifier =
                "ui-" ++ String.fromInt model.nextRequest

            pending =
                { kind = kind, context = path, epoch = model.epoch, draftSerial = model.draftSerial }

            next =
                { model
                    | nextRequest = model.nextRequest + 1
                    , pending = Dict.insert identifier pending model.pending
                    , latest = Dict.insert (requestKey kind) identifier model.latest
                }
        in
        ( next, toJs (Api.request identifier method path body) )


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        FromJs value ->
            receive value model

        ChooseView selected ->
            let
                current =
                    model.query

                query =
                    { current | view = selected }
            in
            load { model | query = query, page = 0, viewStale = True, error = Nothing }

        EditQuery key content ->
            let
                current = model.query

                query =
                    case key of
                        "revision" -> { current | revision = content }
                        "text" -> { current | text = content }
                        "mode" -> { current | mode = content }
                        "projection" -> { current | projection = content }
                        "domain" -> { current | domain = content }
                        "file" -> { current | file = content }
                        "actor" -> { current | actor = content }
                        "since" -> { current | since = content }
                        "until" -> { current | until = content }
                        "limit" -> { current | limit = Maybe.withDefault 0 (String.toInt content) }
                        "order" -> { current | order = content }
                        "adr" -> { current | adr = content }
                        "compareFrom" -> { current | compareFrom = content }
                        "compareTo" -> { current | compareTo = content }
                        _ -> current
            in
            ( { model | query = query, page = 0, viewStale = True }, Cmd.none )

        ToggleQuery key checked ->
            let
                current = model.query

                query =
                    case key of
                        "includeObsolete" -> { current | includeObsolete = checked }
                        "shallow" -> { current | shallow = checked }
                        "worktree" -> { current | worktree = checked }
                        _ -> current
            in
            ( { model | query = query, page = 0, viewStale = True }, Cmd.none )

        Load ->
            load model

        SelectAdr adr ->
            selectAdr adr model

        SelectCompareAdr adr revision ->
            selectAdrAt adr revision { model | selectedAdr = Just adr, inspection = Nothing }

        SetPage page ->
            ( { model | page = clamp 0 (maxPage model) page }, Cmd.none )

        Refresh ->
            refresh model

        StartAction action ->
            if model.terminalExhausted then
                ( { model | operationStatus = terminalMessage }, Cmd.none )

            else if isHistorical model then
                ( { model | operationStatus = "Historical inspection is read-only. Return to current HEAD first." }, Cmd.none )

            else
                case model.repository of
                    Nothing ->
                        ( { model | operationStatus = "Refresh the repository before editing." }, Cmd.none )

                    Just repository ->
                        if not model.repositoryReady || (action /= Forms.Create && not (inspectionReady model)) then
                            ( { model | operationStatus = "Wait for fresh repository and both inspection views before editing." }, Cmd.none )

                        else
                            ( { model
                                | draft = Forms.begin action repository model.inspection model.draft
                                , draftSerial = model.draftSerial + 1
                                , operationStatus = "Review original values against current heads before submitting."
                              }
                            , Cmd.none
                            )

        EditDraft field content ->
            ( { model | draft = Forms.change field content model.draft, draftSerial = model.draftSerial + 1 }, Cmd.none )

        ResolveDraft checked ->
            let
                current = model.draft
            in
            ( { model | draft = { current | resolve = checked, dirty = True }, draftSerial = model.draftSerial + 1 }, Cmd.none )

        AdoptTokens ->
            if model.terminalExhausted then
                ( { model | operationStatus = terminalMessage }, Cmd.none )

            else if isHistorical model then
                ( { model | operationStatus = "Historical inspection is read-only. Return to current HEAD first." }, Cmd.none )

            else
                case model.repository of
                    Nothing ->
                        ( { model | operationStatus = "Refresh the repository first." }, Cmd.none )

                    Just repository ->
                        if not model.repositoryReady then
                            ( { model | operationStatus = "Wait for a fresh repository read before adopting tokens." }, Cmd.none )

                        else if model.draft.action == Forms.Create then
                            ( { model | draft = Forms.adoptCreate repository model.draft, operationStatus = "Current repository basis adopted." }, Cmd.none )

                        else if inspectionReady model then
                            case model.inspection of
                                Just inspection ->
                                    case Forms.adopt repository inspection model.draft of
                                        Ok draft ->
                                            ( { model | draft = draft, operationStatus = "Current heads reviewed; fresh tokens adopted." }, Cmd.none )

                                        Err problem ->
                                            ( { model | operationStatus = problem }, Cmd.none )

                                Nothing ->
                                    ( { model | operationStatus = "Inspect the target ADR before adopting tokens." }, Cmd.none )

                        else
                            ( { model | operationStatus = "Wait for fresh collapsed and exploded inspection at current HEAD." }, Cmd.none )

        Submit ->
            submit model

        Reconnect ->
            if model.terminalExhausted || not model.hasCredential || model.reconnects > 4 then
                ( { model | socketState = "unavailable" }, Cmd.none )

            else
                ( { model | socketState = "connecting" }, toJs (E.object [ ( "type", E.string "connect" ) ]) )

        RetryRead requestId pending ->
            if readPendingCurrent requestId pending model then
                issue pending.kind "GET" pending.context Nothing model

            else
                ( model, Cmd.none )

        RetryInspection kind ->
            case ( kind, model.selectedAdr, model.selectedRevision ) of
                ( CollapsedRead, Just adr, Just revision ) ->
                    retryInspection kind (Route.showPath adr revision "collapsed") model

                ( ExplodedRead, Just adr, Just revision ) ->
                    retryInspection kind (Route.showPath adr revision "exploded") model

                _ ->
                    ( model, Cmd.none )


retryInspection : RequestKind -> String -> Model -> ( Model, Cmd Msg )
retryInspection kind path model =
    if model.terminalExhausted then
        ( model, Cmd.none )

    else
        issue kind "GET" path Nothing
            { model
                | busyRetries = Dict.remove (requestKey kind) model.busyRetries
                , inspectionIssues = Dict.remove (requestKey kind) model.inspectionIssues
            }


load : Model -> ( Model, Cmd Msg )
load model =
    if model.terminalExhausted then
        ( model, Cmd.none )

    else
        loadActive model


loadActive : Model -> ( Model, Cmd Msg )
loadActive model =
    case validateQuery model.query of
        Just problem ->
            ( { model | error = Just problem, viewStale = True }, Cmd.none )

        Nothing ->
            let
                path =
                    Route.queryPath model.query

                ( next, command ) =
                    issue QueryRead "GET" path Nothing { model | viewStale = True, error = Nothing, page = 0 }

                interest =
                    if model.query.view == Route.Relevant && model.query.worktree then
                        [ model.query.file ]

                    else
                        []
            in
            ( next
            , Cmd.batch
                [ command
                , if model.hasCredential then
                    toJs (E.object [ ( "type", E.string "active-files" ), ( "paths", E.list E.string interest ) ])
                  else
                    Cmd.none
                ]
            )


validateQuery : Route.Query -> Maybe String
validateQuery query =
    if query.limit < 1 || query.limit > (if query.view == Route.Relevant then 100 else 1000) then
        Just "Window limit must be between 1 and 1000 (100 for relevance)."

    else if query.view == Route.Relevant && String.trim query.file == "" then
        Just "Choose a repository-relative file for relevance."

    else if query.view == Route.Relevant && query.worktree && query.revision /= "HEAD" then
        Just "Worktree relevance cannot use an explicit revision."

    else if query.view == Route.Compare && (String.trim query.compareFrom == "" || String.trim query.compareTo == "") then
        Just "Choose both comparison revisions."

    else if not (String.isEmpty query.since || signedDecimal query.since) || not (String.isEmpty query.until || signedDecimal query.until) then
        Just "Time filters must be signed Unix milliseconds."

    else
        Nothing


signedDecimal : String -> Bool
signedDecimal raw =
    let
        digits =
            if String.startsWith "-" raw then String.dropLeft 1 raw else raw
    in
    not (String.isEmpty digits) && String.all (\c -> c >= '0' && c <= '9') digits


selectAdr : String -> Model -> ( Model, Cmd Msg )
selectAdr adr model =
    let
        revision =
            currentViewRevision model

        selected =
            { model | selectedAdr = Just adr, selectedRevision = Just revision, inspection = Nothing, collapsedReady = False, explodedReady = False, error = Nothing, inspectionIssues = Dict.empty, busyRetries = clearInspectionRetries model.busyRetries }

        ( withCollapsed, collapsedCommand ) =
            issue CollapsedRead "GET" (Route.showPath adr revision "collapsed") Nothing selected

        ( withExploded, explodedCommand ) =
            issue ExplodedRead "GET" (Route.showPath adr revision "exploded") Nothing withCollapsed
    in
    ( withExploded, Cmd.batch [ collapsedCommand, explodedCommand ] )


clearInspectionRetries : Dict String Int -> Dict String Int
clearInspectionRetries retries =
    Dict.remove "collapsed" (Dict.remove "exploded" retries)


currentViewRevision : Model -> String
currentViewRevision model =
    case model.query.view of
        Route.Browse -> Maybe.withDefault "HEAD" (Maybe.map .asOf model.search)
        Route.Search -> Maybe.withDefault "HEAD" (Maybe.map .asOf model.search)
        Route.Relevant -> Maybe.withDefault "HEAD" (Maybe.map .asOf model.relevant)
        Route.History -> Maybe.withDefault "HEAD" (Maybe.map .asOf model.history)
        Route.Compare -> Maybe.withDefault "HEAD" (Maybe.map .to model.comparison)
        _ -> Maybe.withDefault "HEAD" model.queryAsOf


refresh : Model -> ( Model, Cmd Msg )
refresh model =
    if model.terminalExhausted then
        ( model, Cmd.none )

    else
        refreshActive model


refreshActive : Model -> ( Model, Cmd Msg )
refreshActive model =
    let
        stale =
            { model | viewStale = True, draft = Forms.markStale model.draft, epoch = model.epoch + 1, repositoryReady = False, collapsedReady = False, explodedReady = False, busyRetries = Dict.empty, inspectionIssues = Dict.empty }

        ( withRepository, repositoryCommand ) =
            issue RepositoryRead "GET" Route.repository Nothing stale

        ( withView, viewCommand ) =
            load withRepository
    in
    ( withView, Cmd.batch [ repositoryCommand, viewCommand ] )


submit : Model -> ( Model, Cmd Msg )
submit model =
    let
        draft =
            model.draft

        historical =
            isHistorical model
    in
    if model.terminalExhausted then
        ( { model | operationStatus = terminalMessage }, Cmd.none )

    else if not model.hasCredential then
        ( { model | operationStatus = "Reopen the process bootstrap URL to make changes." }, Cmd.none )

    else if not model.repositoryReady || (draft.action /= Forms.Create && not (inspectionReady model)) then
        ( { model | operationStatus = "Wait for fresh repository and both inspection views before submitting." }, Cmd.none )

    else if model.mutationPending /= Nothing then
        ( { model | operationStatus = "Wait for the current operation result." }, Cmd.none )

    else if historical then
        ( { model | operationStatus = "Historical inspection is read-only. Return to current HEAD and review it." }, Cmd.none )

    else if Maybe.map .head model.repository /= Just draft.basisHead || Maybe.map .stateToken model.repository /= Just draft.basisToken then
        ( { model | operationStatus = "Repository changed. Refresh, inspect, then adopt tokens." }, Cmd.none )

    else
        case Forms.build draft of
            Err problem ->
                ( { model | operationStatus = problem }, Cmd.none )

            Ok body ->
                let
                    path =
                        Route.mutationPath (Forms.actionName draft.action) draft.target

                    ( next, command ) =
                        issue MutationWrite "POST" path (Just body) model

                    identifier =
                        "ui-" ++ String.fromInt model.nextRequest
                in
                ( { next | mutationPending = Just identifier, operationStatus = "Submitting checked operation…" }, command )


receive : D.Value -> Model -> ( Model, Cmd Msg )
receive raw model =
    case D.decodeValue (D.field "type" D.string) raw of
        Ok "response" ->
            case D.decodeValue (D.map3 (\requestId status body -> ( requestId, status, body ))
                    (D.field "request_id" D.string)
                    (D.field "status" D.int)
                    (D.field "body" D.value)) raw of
                Ok ( requestId, status, body ) -> receiveResponse requestId status body model
                Err _ -> ( { model | error = Just "Malformed transport response." }, Cmd.none )

        Ok "request-failed" ->
            case D.decodeValue (D.map2 Tuple.pair (D.field "request_id" D.string) (D.field "message" D.string)) raw of
                Ok ( requestId, message ) -> requestFailed requestId message model
                Err _ -> ( { model | error = Just "Malformed transport failure." }, Cmd.none )

        Ok "socket-state" ->
            case D.decodeValue (D.field "state" D.string) raw of
                Ok state ->
                    if state == "unavailable" && D.decodeValue (D.field "reason" D.string) raw == Ok "generation-exhausted" then
                        terminalExhaustion model

                    else
                        socketChanged state model
                Err _ -> ( model, Cmd.none )

        Ok "event" ->
            case D.decodeValue (D.field "body" Api.event) raw of
                Ok event -> eventReceived event model
                Err _ -> refresh { model | error = Just "Event decoding failed; refreshing snapshots." }

        _ ->
            ( model, Cmd.none )


receiveResponse : String -> Int -> D.Value -> Model -> ( Model, Cmd Msg )
receiveResponse requestId status body model =
    case Dict.get requestId model.pending of
        Nothing ->
            ( model, Cmd.none )

        Just pending ->
            let
                without =
                    { model | pending = Dict.remove requestId model.pending }

                current =
                    readPendingCurrent requestId pending model
            in
            if pending.kind == MutationWrite then
                mutationResponse requestId pending status body without

            else if status == 503 && terminalFailure body then
                terminalExhaustion without

            else if not current then
                ( without, Cmd.none )

            else if status == 503 && retryableBusy body then
                let
                    key = requestKey pending.kind
                    attempts = Maybe.withDefault 0 (Dict.get key model.busyRetries)
                in
                if attempts < 2 then
                    ( readIssue pending.kind "Repository is busy; retrying this read." { without | busyRetries = Dict.insert key (attempts + 1) model.busyRetries }
                    , Task.perform (\_ -> RetryRead requestId pending) (Process.sleep 400)
                    )

                else
                    ( readIssue pending.kind
                        (if pending.kind == CollapsedRead || pending.kind == ExplodedRead then
                            "Repository remains busy. Retry this inspection when it settles."
                         else
                            "Repository remains busy. Refresh when it settles.")
                        without
                    , Cmd.none
                    )

            else if status >= 400 then
                case D.decodeValue Api.failure body of
                    Ok failure ->
                        if failure.status /= status then
                            ( readIssue pending.kind "The server returned an inconsistent error status." without, Cmd.none )

                        else if failure.status == 401 then
                            ( { without | error = Just "Session unavailable. Reopen the process bootstrap URL.", viewStale = True, hasCredential = False, socketState = "unavailable" }
                            , toJs (E.object [ ( "type", E.string "disconnect" ) ])
                            )

                        else
                            ( readIssue pending.kind (failure.code ++ ": " ++ failure.message) without, Cmd.none )

                    Err _ ->
                        ( readIssue pending.kind "The server returned an unreadable error." without, Cmd.none )

            else
                readResponse pending.kind body
                    { without
                        | busyRetries = Dict.remove (requestKey pending.kind) without.busyRetries
                        , inspectionIssues = Dict.remove (requestKey pending.kind) without.inspectionIssues
                    }


readPendingCurrent : String -> Pending -> Model -> Bool
readPendingCurrent requestId pending model =
    not model.terminalExhausted
        && Dict.get (requestKey pending.kind) model.latest == Just requestId
        && pending.epoch == model.epoch
        && (pending.kind /= QueryRead || pending.context == Route.queryPath model.query)
        && (pending.kind /= CollapsedRead || pending.context == selectedPath "collapsed" model)
        && (pending.kind /= ExplodedRead || pending.context == selectedPath "exploded" model)


readIssue : RequestKind -> String -> Model -> Model
readIssue kind message model =
    case kind of
        CollapsedRead ->
            { model | inspectionIssues = Dict.insert "collapsed" message model.inspectionIssues, viewStale = True }

        ExplodedRead ->
            { model | inspectionIssues = Dict.insert "exploded" message model.inspectionIssues, viewStale = True }

        _ ->
            { model | error = Just message, viewStale = True }


selectedPath : String -> Model -> String
selectedPath viewMode model =
    case ( model.selectedAdr, model.selectedRevision ) of
        ( Just adr, Just revision ) -> Route.showPath adr revision viewMode
        _ -> ""


retryableBusy : D.Value -> Bool
retryableBusy body =
    case D.decodeValue Api.failure body of
        Ok problem ->
            problem.status == 503 && List.member problem.code [ "repository-busy", "repository-lock-unavailable" ]

        Err _ ->
            False


terminalFailure : D.Value -> Bool
terminalFailure body =
    case D.decodeValue Api.failure body of
        Ok failure ->
            failure.status == 503 && failure.code == "generation-exhausted"

        Err _ ->
            False


terminalMessage : String
terminalMessage =
    "Event generation exhausted. Restart the web server, then reopen its new bootstrap URL."


terminalExhaustion : Model -> ( Model, Cmd Msg )
terminalExhaustion model =
    let
        draft = model.draft
        statusText =
            if String.contains terminalMessage model.operationStatus then model.operationStatus
            else if String.isEmpty model.operationStatus then terminalMessage
            else model.operationStatus ++ " " ++ terminalMessage
    in
    ( { model
        | terminalExhausted = True
        , socketState = "unavailable"
        , repositoryReady = False
        , collapsedReady = False
        , explodedReady = False
        , epoch = model.epoch + 1
        , busyRetries = Dict.empty
        , inspectionIssues = Dict.empty
        , viewStale = True
        , error = Just terminalMessage
        , operationStatus = statusText
        , draft = { draft | stale = True, reviewed = False }
      }
    , toJs (E.object [ ( "type", E.string "disconnect" ) ])
    )


readResponse : RequestKind -> D.Value -> Model -> ( Model, Cmd Msg )
readResponse kind body model =
    case kind of
        RepositoryRead ->
            case D.decodeValue (Api.response Api.repository) body of
                Ok envelope ->
                    case envelope.metadata.asOf of
                        Api.Unavailable reason ->
                            ( { model | error = Just ("Repository observation unavailable: " ++ reason), viewStale = True }, Cmd.none )

                        Api.AtCommit oid ->
                            let
                                repository =
                                    envelope.data

                                updated =
                                    { model | repository = Just repository, repositoryReady = True, error = Nothing, draft = Forms.seedBasis repository model.draft }
                            in
                            if oid /= repository.head then
                                ( { model | error = Just "Repository response basis disagrees with its metadata.", viewStale = True }, Cmd.none )

                            else
                                case ( model.selectedAdr, model.query.revision ) of
                                    ( Just adr, "HEAD" ) -> selectAdrAt adr repository.head updated
                                    _ -> ( updated, Cmd.none )

                        Api.AtComparison _ _ ->
                            ( { model | error = Just "Repository response has an invalid comparison basis.", viewStale = True }, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Repository response has an unsupported shape.", viewStale = True }, Cmd.none )

        QueryRead ->
            case model.query.view of
                Route.Browse -> readSearch body model
                Route.Search -> readSearch body model
                Route.Relevant ->
                    accept Api.relevant body
                        (\window state -> { state | relevant = Just window, page = 0, viewStale = False })
                        model
                Route.History ->
                    accept Api.history body
                        (\window state -> { state | history = Just window, page = 0, viewStale = False })
                        model
                Route.Compare ->
                    accept Api.comparison body
                        (\window state -> { state | comparison = Just window, page = 0, viewStale = False })
                        model
                Route.Conflicts ->
                    accept Api.conflicts body
                        (\window state -> { state | conflicts = Just window, page = 0, viewStale = False })
                        model
                Route.Doctor ->
                    accept Api.doctor body
                        (\window state -> { state | doctor = Just window, page = 0, viewStale = False })
                        model

        CollapsedRead ->
            case D.decodeValue (Api.response Api.inspection) body of
                Ok envelope ->
                    case envelope.metadata.asOf of
                        Api.Unavailable reason ->
                            ( { model | viewStale = True, error = Just ("Inspection unavailable: " ++ reason) }, Cmd.none )

                        Api.AtCommit oid ->
                            let
                                inspection =
                                    envelope.data

                                previous =
                                    model.inspection

                                merged =
                                    case previous of
                                        Just prior ->
                                            if prior.adr == inspection.adr && prior.asOf == inspection.asOf then
                                                { inspection | operations = prior.operations }
                                            else
                                                inspection

                                        Nothing ->
                                            inspection
                            in
                            if inspection.view /= "collapsed" || oid /= inspection.asOf || model.selectedRevision /= Just oid || model.selectedAdr /= Just inspection.adr then
                                ( { model | viewStale = True, error = Just "Inspection basis disagrees with the selected ADR and revision." }, Cmd.none )

                            else
                                ( { model | inspection = Just merged, collapsedReady = True, error = Nothing }, Cmd.none )

                        Api.AtComparison _ _ ->
                            ( { model | viewStale = True, error = Just "Inspection has an invalid comparison basis." }, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Inspection response has an unsupported shape.", viewStale = True }, Cmd.none )

        ExplodedRead ->
            case D.decodeValue (Api.response Api.inspection) body of
                Ok envelope ->
                    case envelope.metadata.asOf of
                        Api.Unavailable reason ->
                            ( { model | viewStale = True, error = Just ("Operation history unavailable: " ++ reason) }, Cmd.none )

                        Api.AtCommit oid ->
                            let
                                exploded =
                                    envelope.data
                            in
                            if exploded.view /= "exploded" || oid /= exploded.asOf || model.selectedRevision /= Just oid || model.selectedAdr /= Just exploded.adr then
                                ( { model | viewStale = True, error = Just "Operation history basis disagrees with the selected ADR and revision." }, Cmd.none )

                            else
                                case model.inspection of
                                    Just collapsed ->
                                        if collapsed.adr == exploded.adr && collapsed.asOf == exploded.asOf then
                                            ( { model | inspection = Just { collapsed | operations = exploded.operations }, explodedReady = True }, Cmd.none )
                                        else
                                            ( { model | inspection = Just exploded, explodedReady = True }, Cmd.none )

                                    Nothing ->
                                        ( { model | inspection = Just exploded, explodedReady = True }, Cmd.none )

                        Api.AtComparison _ _ ->
                            ( { model | viewStale = True, error = Just "Operation history has an invalid comparison basis." }, Cmd.none )

                Err _ ->
                    ( { model | error = Just "Operation history has an unsupported shape.", viewStale = True }, Cmd.none )

        MutationWrite ->
            ( model, Cmd.none )


readSearch : D.Value -> Model -> ( Model, Cmd Msg )
readSearch body model =
    accept Api.search body
        (\window state -> { state | search = Just window, page = 0, viewStale = False })
        model


accept : D.Decoder a -> D.Value -> (a -> Model -> Model) -> Model -> ( Model, Cmd Msg )
accept decoder body set model =
    case D.decodeValue (Api.response decoder) body of
        Ok envelope ->
            case envelope.metadata.asOf of
                Api.Unavailable reason ->
                    ( { model | viewStale = True, error = Just ("Snapshot unavailable: " ++ reason) }, Cmd.none )

                Api.AtCommit oid ->
                    ( set envelope.data { model | error = Nothing, queryAsOf = Just oid }, Cmd.none )

                Api.AtComparison _ toOid ->
                    ( set envelope.data { model | error = Nothing, queryAsOf = Just toOid }, Cmd.none )

        Err _ ->
            ( { model | viewStale = True, error = Just "Response has an unsupported shape." }, Cmd.none )


selectAdrAt : String -> String -> Model -> ( Model, Cmd Msg )
selectAdrAt adr revision model =
    let
        selected =
            { model | selectedAdr = Just adr, selectedRevision = Just revision, collapsedReady = False, explodedReady = False, inspectionIssues = Dict.empty, busyRetries = clearInspectionRetries model.busyRetries }

        ( withCollapsed, collapsedCommand ) =
            issue CollapsedRead "GET" (Route.showPath adr revision "collapsed") Nothing selected

        ( withExploded, explodedCommand ) =
            issue ExplodedRead "GET" (Route.showPath adr revision "exploded") Nothing withCollapsed
    in
    ( withExploded, Cmd.batch [ collapsedCommand, explodedCommand ] )


mutationResponse : String -> Pending -> Int -> D.Value -> Model -> ( Model, Cmd Msg )
mutationResponse requestId pending status body model =
    let
        cleared =
            { model | mutationPending = if model.mutationPending == Just requestId then Nothing else model.mutationPending }
    in
    case D.decodeValue (Api.response Api.mutation) body of
        Ok envelope ->
            if status < 400 && envelope.data.committed then
                let
                    outcome =
                        envelope.data

                    currentDraft =
                        model.draft

                    warning =
                        String.join ""
                            (List.filterMap identity
                                [ Maybe.map (\value -> " Publication warning: " ++ value) outcome.publicationWarning
                                , if outcome.indexed then Nothing else Just (" Index warning: " ++ Maybe.withDefault "index unavailable" outcome.indexError)
                                ]
                            )

                    statusText =
                        "Committed " ++ outcome.operation ++ " at " ++ outcome.commit ++ "." ++ warning

                    safe =
                        { cleared
                            | operationStatus = statusText
                            , draft = if pending.draftSerial == model.draftSerial then { currentDraft | stale = True, reviewed = False } else model.draft
                            , viewStale = True
                        }
                in
                if model.terminalExhausted || outcome.publicationWarning == Just "generation-exhausted" then
                    terminalExhaustion safe

                else
                    refresh safe

            else if status >= 400 then
                ( { cleared | operationStatus = "The response is inconsistent. Inspect operation history before another submission.", draft = Forms.markStale model.draft }, Cmd.none )

            else
                ( { cleared | operationStatus = "Server did not confirm a commit. Inspect current repository state.", draft = Forms.markStale model.draft }, Cmd.none )

        Err _ ->
            case D.decodeValue Api.failure body of
                Ok failure ->
                    if failure.status /= status then
                        ( { cleared | operationStatus = "The response is inconsistent. Inspect operation history before another submission.", draft = Forms.markStale model.draft }, Cmd.none )

                    else if status == 503 && failure.code == "generation-exhausted" then
                        terminalExhaustion { cleared | operationStatus = "Operation rejected (generation-exhausted): " ++ failure.message }

                    else if status == 401 then
                        ( { cleared
                            | operationStatus = "Session unavailable. Reopen the process bootstrap URL."
                            , hasCredential = False
                            , socketState = "unavailable"
                            , draft = Forms.markStale model.draft
                          }
                        , toJs (E.object [ ( "type", E.string "disconnect" ) ])
                        )

                    else
                        ( { cleared
                            | operationStatus = "Operation rejected (" ++ failure.code ++ "): " ++ failure.message
                                ++ (if status == 409 then " Refresh, inspect, and adopt tokens." else "")
                            , draft = if status == 409 then Forms.markStale model.draft else model.draft
                          }
                        , Cmd.none
                        )

                Err _ ->
                    ( { cleared | operationStatus = "The result is uncertain. Inspect repository and operation history before another submission.", draft = Forms.markStale model.draft }, Cmd.none )


requestFailed : String -> String -> Model -> ( Model, Cmd Msg )
requestFailed requestId message model =
    case Dict.get requestId model.pending of
        Nothing -> ( model, Cmd.none )
        Just pending ->
            let
                without = { model | pending = Dict.remove requestId model.pending }
            in
            if pending.kind == MutationWrite then
                ( { without | mutationPending = Nothing, draft = Forms.markStale model.draft, operationStatus = "Mutation outcome is uncertain. Inspect current history before retrying." }, Cmd.none )
            else if not (readPendingCurrent requestId pending model) then
                ( without, Cmd.none )
            else
                ( readIssue pending.kind message without, Cmd.none )


socketChanged : String -> Model -> ( Model, Cmd Msg )
socketChanged state model =
    if model.terminalExhausted then
        ( model, Cmd.none )

    else if state == "closed" && model.hasCredential && model.reconnects < 4 then
        let
            attempts = model.reconnects + 1
            pause = toFloat (min 8000 (500 * (2 ^ attempts)))
        in
        ( { model | socketState = "closed", reconnects = attempts, viewStale = True, draft = Forms.markStale model.draft, repositoryReady = False, collapsedReady = False, explodedReady = False }
        , Task.perform (\_ -> Reconnect) (Process.sleep pause)
        )
    else if state == "closed" && model.hasCredential then
        ( { model | socketState = "unavailable", viewStale = True, repositoryReady = False, collapsedReady = False, explodedReady = False }, Cmd.none )
    else
        ( { model | socketState = state }, Cmd.none )


eventReceived : Api.Event -> Model -> ( Model, Cmd Msg )
eventReceived event model =
    if model.terminalExhausted || Api.compareGeneration event.generation model.watermark /= GT then
        ( model, Cmd.none )

    else
        let
            changed =
                { model | watermark = event.generation, epoch = model.epoch + 1, viewStale = True, draft = Forms.markStale model.draft, reconnects = if event.kind == "repository-invalidated" then 0 else model.reconnects }
        in
        if event.kind == "observation-failed" then
            refresh { changed | error = Just "Repository observation failed; refreshing." }

        else
            case event.asOf of
                Api.Unavailable reason ->
                    refresh { changed | error = Just ("Live observation unavailable: " ++ reason) }

                _ ->
                    refresh changed


maxPage : Model -> Int
maxPage model =
    case model.query.view of
        Route.Browse -> Maybe.withDefault 0 (Maybe.map (.results >> Route.pageCount >> (\n -> n - 1)) model.search)
        Route.Search -> Maybe.withDefault 0 (Maybe.map (.results >> Route.pageCount >> (\n -> n - 1)) model.search)
        Route.History -> Maybe.withDefault 0 (Maybe.map (.operations >> Route.pageCount >> (\n -> n - 1)) model.history)
        _ -> 0


view : Model -> Html Msg
view model =
    main_ []
        [ header [ class "masthead" ]
            [ h1 [] [ text "ADRAI repository explorer" ]
            , p [] [ text (repositoryLabel model) ]
            ]
        , div [ class "panes" ]
            [ section [ class "pane", id "context-pane" ] [ h2 [] [ text "Context and results" ], contextPane model ]
            , section [ class "pane", id "inspector-pane" ] [ h2 [] [ text "Inspector" ], inspectionIssuesView model, inspectorPane model ]
            , section [ class "pane", id "actions-pane" ] [ h2 [] [ text "Actions and status" ], actionsPane model ]
            ]
        ]


repositoryLabel : Model -> String
repositoryLabel model =
    case model.repository of
        Just repository ->
            "HEAD " ++ repository.head ++ " · " ++ Maybe.withDefault "detached" repository.headRef
        Nothing ->
            "Repository loading"


contextPane : Model -> Html Msg
contextPane model =
    let
        field key title content =
            div [ class "field" ] [ label [ for key ] [ text title ], input [ id key, value content, onInput (EditQuery key) ] [] ]

        check key title selected =
            div [] [ label [ for key ] [ input [ id key, type_ "checkbox", checked selected, onCheck (ToggleQuery key) ] [], text title ] ]
    in
    div []
        [ if not model.hasCredential then
            p [ class "notice" ] [ text "Read-only session. Reopen the process bootstrap URL to restore live updates and checked mutations." ]
          else
            p [ class "meta" ] [ text ("Live connection: " ++ model.socketState) ]
        , case model.error of
            Just problem -> p [ class "error" ] [ text problem ]
            Nothing -> text ""
        , if model.viewStale then p [ class "notice" ] [ text "Snapshot is loading or stale." ] else text ""
        , div [ class "nav" ]
            (List.map (\( name, kind ) -> button [ type_ "button", class (if model.query.view == kind then "selected" else ""), onClick (ChooseView kind) ] [ text name ])
                [ ( "Browse", Route.Browse ), ( "Search", Route.Search ), ( "Relevant", Route.Relevant ), ( "History", Route.History ), ( "Compare", Route.Compare ), ( "Conflicts", Route.Conflicts ), ( "Doctor", Route.Doctor ) ]
            )
        , field "revision" "Revision (HEAD or exact commit)" model.query.revision
        , case model.query.view of
            Route.Browse -> searchControls model field check False
            Route.Search -> searchControls model field check True
            Route.Relevant ->
                div []
                    [ field "file" "Repository-relative source file" model.query.file
                    , check "worktree" "Use worktree source" model.query.worktree
                    , check "includeObsolete" "Include obsolete" model.query.includeObsolete
                    , field "limit" "Result limit (1–100)" (String.fromInt model.query.limit)
                    ]
            Route.History ->
                div []
                    [ field "adr" "ADR filter" model.query.adr
                    , field "actor" "Actor (kind:identifier)" model.query.actor
                    , field "since" "Since (Unix milliseconds)" model.query.since
                    , field "until" "Until (Unix milliseconds)" model.query.until
                    , field "limit" "Window size (1–1000)" (String.fromInt model.query.limit)
                    , label [ for "order" ] [ text "Order" ]
                    , select [ id "order", onInput (EditQuery "order") ] [ option [ value "newest", selected (model.query.order == "newest") ] [ text "Newest" ], option [ value "oldest", selected (model.query.order == "oldest") ] [ text "Oldest" ] ]
                    ]
            Route.Compare ->
                div [] [ field "compareFrom" "From revision" model.query.compareFrom, field "compareTo" "To revision" model.query.compareTo ]
            _ ->
                text ""
        , div [ class "actions" ]
            [ button [ type_ "button", class "primary", onClick Load ] [ text "Load view" ]
            , button [ type_ "button", onClick Refresh ] [ text "Refresh repository" ]
            ]
        , results model
        ]


searchControls : Model -> (String -> String -> String -> Html Msg) -> (String -> String -> Bool -> Html Msg) -> Bool -> Html Msg
searchControls model field check withText =
    div []
        ([ if withText then field "text" "Search terms" model.query.text else p [] [ text "Browse the ordered result window." ]
         , label [ for "mode" ] [ text "Retrieval mode" ]
         , select [ id "mode", onInput (EditQuery "mode") ] (List.map (\mode -> option [ value mode, selected (model.query.mode == mode) ] [ text mode ]) [ "hybrid", "fts", "vector" ])
         , label [ for "projection" ] [ text "Result view" ]
         , select [ id "projection", onInput (EditQuery "projection") ] [ option [ value "collapsed", selected (model.query.projection == "collapsed") ] [ text "Collapsed" ], option [ value "exploded", selected (model.query.projection == "exploded") ] [ text "Exploded" ] ]
         , field "domain" "Domain filter" model.query.domain
         , field "file" "File scope filter" model.query.file
         , field "actor" "Actor (kind:identifier)" model.query.actor
         , field "since" "Since (Unix milliseconds)" model.query.since
         , field "until" "Until (Unix milliseconds)" model.query.until
         , check "includeObsolete" "Include obsolete" model.query.includeObsolete
         , check "shallow" "Shallow history" model.query.shallow
         , field "limit" "Window size (1–1000)" (String.fromInt model.query.limit)
         ]
        )


results : Model -> Html Msg
results model =
    case model.query.view of
        Route.Browse -> searchResults model
        Route.Search -> searchResults model
        Route.Relevant ->
            case model.relevant of
                Nothing -> p [] [ text "No relevance results loaded." ]
                Just window ->
                    div []
                        [ p [ class "meta" ] [ text ("Source: " ++ window.file ++ " · " ++ window.source) ]
                        , ul [ class "result-list" ]
                            (List.map (\hit ->
                                li []
                                    [ button [ type_ "button", onClick (SelectAdr hit.adr) ] [ strong [] [ text hit.title ], text (" · " ++ hit.adr) ]
                                    , p [] [ text hit.summary ]
                                    , p [ class "meta" ] [ text ("Declared applicability: " ++ hit.scopeMatch ++ " · Semantic evidence: " ++ hit.confidence ++ " · Score " ++ String.fromFloat hit.score) ]
                                    , ul [] (List.map (\e -> li [] [ text (e.section ++ ": " ++ e.fileExcerpt ++ " ↔ " ++ e.adrExcerpt) ]) hit.evidence)
                                    ]
                              ) window.results)
                        ]
        Route.History ->
            case model.history of
                Nothing -> p [] [ text "No history loaded." ]
                Just window ->
                    div []
                        [ if window.truncated then p [ class "notice" ] [ text "History window truncated by the server." ] else text ""
                        , pager model
                        , ul [ class "result-list" ]
                            (List.map (\item ->
                                li []
                                    [ button [ type_ "button", onClick (SelectAdr item.adr) ] [ text (item.title ++ " · " ++ item.label) ]
                                    , p [ class "meta" ] [ text (item.actor ++ " · " ++ item.claimedAt ++ " · " ++ item.operation) ]
                                    , p [] [ text (String.join ", " item.changes) ]
                                    ]
                              ) (Route.pageSlice model.page window.operations))
                        ]
        Route.Compare ->
            case model.comparison of
                Nothing -> p [] [ text "No comparison loaded." ]
                Just window ->
                    div []
                        [ p [ class "meta" ] [ text (window.from ++ " → " ++ window.to) ]
                        , ul [ class "result-list" ] (List.map (\entry ->
                            li []
                                [ button [ type_ "button", onClick (SelectCompareAdr entry.adr (if entry.after == Nothing then window.from else window.to)) ] [ text (entry.title ++ " · " ++ entry.kind) ]
                                , snapshotView "Before" entry.before
                                , snapshotView "After" entry.after
                                , div [] (List.map (\change ->
                                    article [ class "candidate" ]
                                        [ strong [] [ text change.field ]
                                        , p [] [ text ("Before: " ++ change.before) ]
                                        , p [] [ text ("After: " ++ change.after) ]
                                        , case change.diff of
                                            Just difference -> pre [ class "body-text" ] [ text difference ]
                                            Nothing -> text ""
                                        ]
                                  ) entry.changes)
                                ]
                          ) window.entries)
                        ]
        Route.Conflicts ->
            case model.conflicts of
                Nothing -> p [] [ text "No conflict index loaded." ]
                Just window ->
                    if List.isEmpty window.conflicts then p [] [ text "No conflicts at this revision." ]
                    else
                        ul [ class "result-list" ] (List.map (\entry ->
                            li []
                                [ button [ type_ "button", onClick (SelectAdr entry.adr) ] [ text entry.adr ]
                                , p [] [ text (String.join "; " entry.summaries) ]
                                , ul [] (List.map (\candidate -> li [] [ text (candidate.axis ++ ": " ++ candidate.summary ++ " [" ++ String.join ", " candidate.heads ++ "]") ]) entry.candidates)
                                ]
                          ) window.conflicts)
        Route.Doctor ->
            case model.doctor of
                Nothing -> p [] [ text "No diagnostics loaded." ]
                Just doctor ->
                    if List.isEmpty doctor.issues then p [] [ text "No issues reported." ]
                    else ul [] (List.map (\diagnostic -> li [] [ text (diagnostic.severity ++ " · " ++ diagnostic.code ++ ": " ++ diagnostic.message ++ " " ++ Maybe.withDefault "" diagnostic.path) ]) doctor.issues)


searchResults : Model -> Html Msg
searchResults model =
    case model.search of
        Nothing ->
            p [] [ text "No result window loaded." ]

        Just window ->
            div []
                [ p [ class "meta" ] [ text ("At " ++ window.asOf ++ " · " ++ String.fromInt (List.length window.results) ++ " results in a bounded window") ]
                , if List.length window.results >= window.limit then p [ class "notice" ] [ text "Result window full; more may exist." ] else text ""
                , pager model
                , ul [ class "result-list" ]
                    (List.map (\hit ->
                        li []
                            [ button [ type_ "button", onClick (SelectAdr hit.adr) ] [ strong [] [ text hit.title ], text (" · " ++ hit.adr) ]
                            , p [] [ text hit.summary ]
                            , p [ class "meta" ] [ text (hit.status ++ " · " ++ String.join ", " hit.domains ++ " · score " ++ Maybe.withDefault "n/a" (Maybe.map String.fromFloat hit.score)) ]
                            , if List.isEmpty hit.matchFields then text "" else p [] [ text ("Matched " ++ String.join ", " hit.matchFields ++ " for " ++ String.join ", " hit.matchTerms) ]
                            , if hit.resolutionRequired then p [ class "notice" ] [ text ("Conflict: " ++ String.join "; " hit.conflicts) ] else text ""
                            ]
                      ) (Route.pageSlice model.page window.results))
                ]


pager : Model -> Html Msg
pager model =
    div [ class "pager" ]
        [ button [ type_ "button", disabled (model.page <= 0), onClick (SetPage (model.page - 1)) ] [ text "Previous page" ]
        , span [] [ text ("Page " ++ String.fromInt (model.page + 1) ++ " of " ++ String.fromInt (maxPage model + 1)) ]
        , button [ type_ "button", disabled (model.page >= maxPage model), onClick (SetPage (model.page + 1)) ] [ text "Next page" ]
                ]


snapshotView : String -> Maybe Api.CompareSnapshot -> Html msg
snapshotView label snapshot =
    case snapshot of
        Nothing ->
            p [ class "meta" ] [ text (label ++ ": absent at this revision") ]

        Just value ->
            article [ class "candidate" ]
                [ h4 [] [ text label ]
                , p [] [ strong [] [ text value.title ], text (" · " ++ value.status) ]
                , p [] [ text value.summary ]
                , pre [ class "body-text" ] [ text value.body ]
                , p [] [ text ("Domains: " ++ String.join ", " value.domains) ]
                , p [] [ text ("Scopes: " ++ String.join ", " value.scopes) ]
                ]


compareDetail : Model -> Html Msg
compareDetail model =
    case ( model.query.view, model.comparison, model.selectedAdr ) of
        ( Route.Compare, Just window, Just adr ) ->
            case List.head (List.filter (\entry -> entry.adr == adr) window.entries) of
                Just entry ->
                    div []
                        [ h3 [] [ text ("Comparison: " ++ entry.kind ++ " · " ++ entry.adr) ]
                        , snapshotView "Before" entry.before
                        , snapshotView "After" entry.after
                        ]

                Nothing ->
                    text ""

        _ ->
            text ""


inspectionIssuesView : Model -> Html Msg
inspectionIssuesView model =
    div []
        (List.filterMap
            (\( kind, label ) ->
                case Dict.get (requestKey kind) model.inspectionIssues of
                    Just problem ->
                        Just
                            (div []
                                [ p [ class "error" ] [ text (label ++ ": " ++ problem) ]
                                , button [ type_ "button", onClick (RetryInspection kind), disabled model.terminalExhausted ] [ text ("Retry " ++ label) ]
                                ]
                            )

                    Nothing ->
                        Nothing
            )
            [ ( CollapsedRead, "decision inspection" ), ( ExplodedRead, "operation history" ) ]
        )


inspectorPane : Model -> Html Msg
inspectorPane model =
    case model.inspection of
        Nothing ->
            div []
                [ compareDetail model
                , p [] [ text "Select an ADR to inspect its decision, candidates, and operation provenance." ]
                ]

        Just inspection ->
            div []
                [ compareDetail model
                , h3 [] [ text (if inspection.title == "" then inspection.adr else inspection.title) ]
                , p [ class "meta" ] [ text (inspection.adr ++ " · " ++ inspection.status ++ " · " ++ inspection.asOf) ]
                , p [] [ text inspection.summary ]
                , h4 [] [ text "Decision body" ]
                , pre [ class "body-text" ] [ text inspection.body ]
                , p [] [ text ("Domains: " ++ String.join ", " inspection.domains) ]
                , p [] [ text ("Scopes: " ++ String.join ", " inspection.scopes) ]
                , if inspection.resolutionRequired then p [ class "notice" ] [ text ("Review required: " ++ String.join "; " inspection.conflicts) ] else text ""
                , candidateView "Decision" inspection.recordHeads (candidateBodies inspection inspection.candidates.records)
                , candidateView "Scope" inspection.scopeHeads inspection.candidates.scopes
                , candidateView "Domain" inspection.domainHeads inspection.candidates.domains
                , candidateView "Status" inspection.statusHeads (statusCandidates inspection)
                , h4 [] [ text "Source paths" ]
                , ul [] (List.map (\path -> li [] [ text path ]) inspection.sourcePaths)
                , h4 [] [ text "Current provenance" ]
                , div [] (List.map provenanceView inspection.provenance)
                , h4 [] [ text "Operations and provenance" ]
                , if List.isEmpty inspection.operations then p [] [ text "No exploded operations loaded." ] else text ""
                , List.map operationView inspection.operations |> div []
                ]


candidateView : String -> List String -> List Api.CandidateRecord -> Html Msg
candidateView axis heads candidates =
    if List.isEmpty heads then text ""
    else
        div []
            [ h4 [] [ text (axis ++ " heads") ]
            , p [ class "meta" ] [ text (String.join ", " heads) ]
            , div [] (List.map (\candidate ->
                article [ class "candidate" ]
                    [ strong [] [ text candidate.title ]
                    , p [] [ text candidate.summary ]
                    , if candidate.body == "" then text "" else pre [ class "body-text" ] [ text candidate.body ]
                    , p [ class "meta" ] [ text (candidate.id ++ " · " ++ candidate.path) ]
                    ]
              ) candidates)
            ]


candidateBodies : Api.Inspection -> List Api.CandidateRecord -> List Api.CandidateRecord
candidateBodies inspection candidates =
    List.map
        (\candidate ->
            case List.head (List.filter (\item -> item.id == candidate.id) (List.concatMap .items inspection.operations)) of
                Just item -> { candidate | body = Maybe.withDefault "" item.body }
                Nothing -> candidate
        )
        candidates


statusCandidates : Api.Inspection -> List Api.CandidateRecord
statusCandidates inspection =
    List.filterMap
        (\item ->
            if List.member item.id inspection.statusHeads then
                Just { id = item.id, title = "Status candidate", summary = Maybe.withDefault item.event item.state, body = Maybe.withDefault "" item.rationale, path = item.path }
            else
                Nothing
        )
        (List.concatMap .items inspection.operations)


operationView : Api.Operation -> Html Msg
operationView operation =
    article [ class "candidate" ]
        [ h4 [] [ text ("Operation " ++ operation.id) ]
        , case operation.provenance of
            Nothing -> text ""
            Just provenance -> provenanceView provenance
        , ul [] (List.map (\item ->
            li []
                [ strong [] [ text (item.kind ++ " · " ++ item.event) ]
                , p [] [ text (Maybe.withDefault "" item.title ++ " " ++ Maybe.withDefault "" item.summary) ]
                , pre [ class "body-text" ] [ text (Maybe.withDefault "" item.body) ]
                , p [] [ text ("Rationale: " ++ Maybe.withDefault "none" item.rationale) ]
                , p [] [ text ("Relation: " ++ Maybe.withDefault "none" item.relation) ]
                , p [] [ text ("Parents: " ++ String.join ", " item.parents) ]
                , div [] (List.map (\difference -> pre [ class "body-text" ] [ text difference ]) item.diffs)
                , case item.rawSemantic of
                    Just raw -> details [ class "candidate" ] [ summary [] [ text "Raw semantic source" ], pre [ class "body-text" ] [ text raw ] ]
                    Nothing -> text ""
                , p [] [ text ("Scopes: " ++ String.join ", " item.scopes ++ " · Domains: " ++ String.join ", " item.domains) ]
                , p [] [ text ("Status: " ++ Maybe.withDefault "" item.state ++ " · Replacement: " ++ Maybe.withDefault "" item.replacement) ]
                , p [] [ text ("Added: " ++ String.join ", " item.added ++ " · Removed: " ++ String.join ", " item.removed ++ " · Refinements: " ++ String.join ", " item.refinements) ]
                , p [ class "meta" ] [ text (item.id ++ " · " ++ item.path) ]
                ]
          ) operation.items)
        ]


provenanceView : Api.Provenance -> Html msg
provenanceView provenance =
    div [ class "candidate" ]
        [ p [] [ text ("Actor: " ++ provenance.actor ++ " · Claimed: " ++ provenance.claimedAt) ]
        , p [ class "meta" ] [ text ("Model: " ++ Maybe.withDefault "none" provenance.model ++ " · Basis: " ++ provenance.basis ++ " · Operation: " ++ provenance.operation) ]
        , p [ class "meta" ] [ text ("Input digest: " ++ Maybe.withDefault "none" provenance.inputDigest) ]
        , p [ class "meta" ] [ text ("Prompt digest: " ++ Maybe.withDefault "none" provenance.promptDigest) ]
        , p [ class "meta" ] [ text ("Context digest: " ++ Maybe.withDefault "none" provenance.contextDigest) ]
        , p [] [ text ("Introductions: " ++ String.join ", " provenance.introductions) ]
        , p [] [ text ("Original operation commits: " ++ String.join ", " provenance.originalCommits) ]
        , ul [] (List.map (\placement -> li [] [ text placement ]) provenance.placements)
        , ul [] (List.map (\landing -> li [] [ text landing ]) provenance.lineLandings)
        ]


actionsPane : Model -> Html Msg
actionsPane model =
    let
        historical =
            isHistorical model

        canAdopt =
            model.hasCredential && not model.terminalExhausted && not historical && model.repositoryReady && (model.draft.action == Forms.Create || inspectionReady model)

        canSubmit =
            model.hasCredential && not model.terminalExhausted && model.repositoryReady && model.mutationPending == Nothing && not historical && not model.draft.stale && (model.draft.action == Forms.Create || inspectionReady model)

        canChooseAction =
            not model.terminalExhausted && not historical && model.repositoryReady
    in
    div []
        [ if historical then p [ class "notice" ] [ text "Historical inspection is read-only. Return to current HEAD before submitting changes." ] else text ""
        , if not model.hasCredential then p [ class "notice" ] [ text "This cleaned-URL session can read snapshots. Reopen the process bootstrap URL for a new in-memory credential before submitting." ] else text ""
        , Forms.view
            { onField = EditDraft
            , onResolve = ResolveDraft
            , onMode = StartAction
            , onAdopt = AdoptTokens
            , onSubmit = Submit
            , canSubmit = canSubmit
            , canAdopt = canAdopt
            , canChooseAction = canChooseAction
            , canChooseExisting = canChooseAction && inspectionReady model
            , draft = model.draft
            , status = model.operationStatus
            , freshRepository = if model.repositoryReady && (model.draft.action == Forms.Create || inspectionReady model) then model.repository else Nothing
            , freshInspection = if inspectionReady model then model.inspection else Nothing
            }
        ]
