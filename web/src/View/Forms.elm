module View.Forms exposing (Action(..), Draft, Field(..), initial, seedBasis, change, begin, markStale, adopt, adoptCreate, build, actionName, view)

import Api
import Html exposing (Html, button, div, fieldset, h3, h4, input, label, legend, li, option, p, pre, select, text, textarea, ul)
import Html.Attributes exposing (checked, disabled, for, id, selected, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Json.Encode as E


type Action
    = Create
    | Amend
    | Scope
    | Domain
    | Obsolete
    | Reactivate


type Field
    = Title
    | Summary
    | Body
    | ChangeSummary
    | Reason
    | Domains
    | Scopes
    | Add
    | Remove
    | Refinements
    | Replacement
    | ActorKind
    | ActorId
    | ActorModel
    | InputDigest
    | PromptDigest
    | ContextDigest
    | Mode


type alias Draft =
    { action : Action
    , target : String
    , title : String
    , summary : String
    , body : String
    , changeSummary : String
    , reason : String
    , domains : String
    , scopes : String
    , add : String
    , remove : String
    , refinements : String
    , replacement : String
    , actorKind : String
    , actorId : String
    , actorModel : String
    , inputDigest : String
    , promptDigest : String
    , contextDigest : String
    , mode : String
    , resolve : Bool
    , dirty : Bool
    , stale : Bool
    , reviewed : Bool
    , basisHead : String
    , basisRef : Maybe String
    , basisToken : String
    , stateToken : String
    , reviewedHeads : List String
    , original : Maybe ReviewBaseline
    , reviewedCurrent : Maybe ReviewBaseline
    }


type alias ReviewBaseline =
    { head : String
    , headRef : Maybe String
    , basisToken : String
    , stateToken : String
    , title : String
    , summary : String
    , body : String
    , domains : List String
    , scopes : List String
    , heads : List String
    , candidates : List String
    }


initial : Draft
initial =
    { action = Create
    , target = ""
    , title = ""
    , summary = ""
    , body = ""
    , changeSummary = ""
    , reason = ""
    , domains = ""
    , scopes = ""
    , add = ""
    , remove = ""
    , refinements = ""
    , replacement = ""
    , actorKind = "human"
    , actorId = ""
    , actorModel = ""
    , inputDigest = ""
    , promptDigest = ""
    , contextDigest = ""
    , mode = "delta"
    , resolve = False
    , dirty = False
    , stale = False
    , reviewed = False
    , basisHead = ""
    , basisRef = Nothing
    , basisToken = ""
    , stateToken = ""
    , reviewedHeads = []
    , original = Nothing
    , reviewedCurrent = Nothing
    }


seedBasis : Api.Repository -> Draft -> Draft
seedBasis repository draft =
    if draft.basisToken == "" then
        { draft
            | basisHead = repository.head
            , basisRef = repository.headRef
            , basisToken = repository.stateToken
            , original = Just (baseline repository Nothing)
        }

    else
        draft


begin : Action -> Api.Repository -> Maybe Api.Inspection -> Draft -> Draft
begin action repository inspection previous =
    let
        selected =
            case if action == Create then Nothing else inspection of
                Nothing -> initial
                Just item ->
                    { initial
                        | target = item.adr
                        , title = item.title
                        , summary = item.summary
                        , body = item.body
                        , domains = String.join "\n" item.domains
                        , scopes = String.join "\n" item.scopes
                        , stateToken = item.stateToken
                        , reviewedHeads = heads item
                    }
    in
    { selected
        | action = action
        , actorKind = previous.actorKind
        , actorId = previous.actorId
        , actorModel = previous.actorModel
        , basisHead = repository.head
        , basisRef = repository.headRef
        , basisToken = repository.stateToken
        , reviewed = Maybe.map .asOf inspection == Just repository.head && Maybe.map .view inspection == Just "collapsed" && action /= Create
        , stale = inspection /= Nothing && Maybe.map .asOf inspection /= Just repository.head && action /= Create
        , original = Just (baseline repository (if action == Create then Nothing else inspection))
        , reviewedCurrent = Nothing
    }


heads : Api.Inspection -> List String
heads inspection =
    inspection.recordHeads ++ inspection.scopeHeads ++ inspection.domainHeads ++ inspection.statusHeads


change : Field -> String -> Draft -> Draft
change field content draft =
    let
        modified =
            case field of
                Title -> { draft | title = content }
                Summary -> { draft | summary = content }
                Body -> { draft | body = content }
                ChangeSummary -> { draft | changeSummary = content }
                Reason -> { draft | reason = content }
                Domains -> { draft | domains = content }
                Scopes -> { draft | scopes = content }
                Add -> { draft | add = content }
                Remove -> { draft | remove = content }
                Refinements -> { draft | refinements = content }
                Replacement -> { draft | replacement = content }
                ActorKind -> { draft | actorKind = content }
                ActorId -> { draft | actorId = content }
                ActorModel -> { draft | actorModel = content }
                InputDigest -> { draft | inputDigest = content }
                PromptDigest -> { draft | promptDigest = content }
                ContextDigest -> { draft | contextDigest = content }
                Mode -> { draft | mode = content }
    in
    { modified | dirty = True }


markStale : Draft -> Draft
markStale draft =
    { draft | stale = draft.dirty || draft.stale, reviewed = False }


adoptCreate : Api.Repository -> Draft -> Draft
adoptCreate repository draft =
    { draft
        | basisHead = repository.head
        , basisRef = repository.headRef
        , basisToken = repository.stateToken
        , stale = False
        , reviewed = True
        , reviewedCurrent = Just (baseline repository Nothing)
    }


adopt : Api.Repository -> Api.Inspection -> Draft -> Result String Draft
adopt repository inspection draft =
    if draft.target /= inspection.adr then
        Err "Inspect the draft's target ADR before adopting tokens."

    else if inspection.asOf /= repository.head then
        Err "Inspect the exact current repository HEAD before adopting tokens."

    else
        Ok
            { draft
                | basisHead = repository.head
                , basisRef = repository.headRef
                , basisToken = repository.stateToken
                , stateToken = inspection.stateToken
                , reviewedHeads = heads inspection
                , stale = False
                , reviewed = True
                , reviewedCurrent = Just (baseline repository (Just inspection))
            }


baseline : Api.Repository -> Maybe Api.Inspection -> ReviewBaseline
baseline repository inspection =
    { head = repository.head
    , headRef = repository.headRef
    , basisToken = repository.stateToken
    , stateToken = Maybe.withDefault "" (Maybe.map .stateToken inspection)
    , title = Maybe.withDefault "" (Maybe.map .title inspection)
    , summary = Maybe.withDefault "" (Maybe.map .summary inspection)
    , body = Maybe.withDefault "" (Maybe.map .body inspection)
    , domains = Maybe.withDefault [] (Maybe.map .domains inspection)
    , scopes = Maybe.withDefault [] (Maybe.map .scopes inspection)
    , heads = Maybe.withDefault [] (Maybe.map heads inspection)
    , candidates = Maybe.withDefault [] (Maybe.map candidateLabels inspection)
    }


candidateLabels : Api.Inspection -> List String
candidateLabels inspection =
    List.map (\candidate -> candidate.id ++ " · " ++ candidate.title ++ " · " ++ candidate.summary) inspection.candidates.records
        ++ List.map (\candidate -> candidate.id ++ " · " ++ candidate.summary) inspection.candidates.scopes
        ++ List.map (\candidate -> candidate.id ++ " · " ++ candidate.summary) inspection.candidates.domains


actionName : Action -> String
actionName action =
    case action of
        Create -> "create"
        Amend -> "amend"
        Scope -> "scope"
        Domain -> "domain"
        Obsolete -> "obsolete"
        Reactivate -> "reactivate"


build : Draft -> Result String E.Value
build draft =
    if draft.basisToken == "" || draft.basisHead == "" then
        Err "Refresh the repository before submitting."

    else if draft.stale || not draft.reviewed && draft.action /= Create then
        Err "Review current heads and adopt fresh tokens before submitting."

    else if draft.actorId == "" then
        Err "Actor ID is required."

    else if not (List.member draft.actorKind [ "human", "llm", "service" ]) then
        Err "Actor kind must be human, llm, or service."

    else if draft.action /= Create && (draft.target == "" || draft.stateToken == "") then
        Err "Select and inspect an ADR before submitting."

    else
        case fields draft of
            Err problem -> Err problem
            Ok actionFields ->
                Ok (E.object (common draft ++ actionFields))


fields : Draft -> Result String (List ( String, E.Value ))
fields draft =
    case draft.action of
        Create ->
            if String.trim draft.title == "" then
                Err "Title is required."
            else
                Ok
                    [ ( "title", E.string draft.title )
                    , ( "summary", E.string draft.summary )
                    , ( "body", E.string (canonicalBody draft.body) )
                    , ( "domains", strings draft.domains )
                    , ( "scopes", strings draft.scopes )
                    ]

        Amend ->
            if String.trim draft.changeSummary == "" then
                Err "Change summary is required."
            else
                Ok
                    [ ( "change_summary", E.string draft.changeSummary )
                    , ( "title", E.string draft.title )
                    , ( "summary", E.string draft.summary )
                    , ( "body", E.string (canonicalBody draft.body) )
                    ]

        Scope ->
            if String.trim draft.reason == "" then
                Err "Scope reason is required."
            else
                variant draft "patterns" draft.scopes

        Domain ->
            if String.trim draft.reason == "" then
                Err "Domain reason is required."
            else
                variant draft "domains" draft.domains

        Obsolete ->
            if String.trim draft.reason == "" then
                Err "Obsolete reason is required."
            else
                Ok
                    ([ ( "reason", E.string draft.reason ), ( "resolve", E.bool draft.resolve ) ]
                        ++ (if String.trim draft.replacement == "" then [] else [ ( "replacement", E.string (String.trim draft.replacement) ) ])
                    )

        Reactivate ->
            if String.trim draft.reason == "" then
                Err "Reactivate reason is required."
            else
                Ok [ ( "reason", E.string draft.reason ), ( "resolve", E.bool draft.resolve ) ]


variant : Draft -> String -> String -> Result String (List ( String, E.Value ))
variant draft reviewedField reviewedValues =
    case draft.mode of
        "delta" ->
            Ok
                [ ( "reason", E.string draft.reason )
                , ( "mode", E.string "delta" )
                , ( "add", strings draft.add )
                , ( "remove", strings draft.remove )
                ]

        "reviewed" ->
            Ok
                [ ( "reason", E.string draft.reason )
                , ( "mode", E.string "reviewed" )
                , ( reviewedField, strings reviewedValues )
                ]

        "refine" ->
            if draft.action /= Domain || List.isEmpty (lines draft.refinements) then
                Err "Domain refinements must contain at least one entry."
            else
                Ok
                    [ ( "reason", E.string draft.reason )
                    , ( "mode", E.string "refine" )
                    , ( "refinements", strings draft.refinements )
                    ]

        _ ->
            Err "Choose a supported change mode."


common : Draft -> List ( String, E.Value )
common draft =
    [ ( "repository_state"
      , E.object
            [ ( "kind", E.string "repository" )
            , ( "token", E.string draft.basisToken )
            , ( "head", E.string draft.basisHead )
            , ( "head_ref", Maybe.withDefault E.null (Maybe.map E.string draft.basisRef) )
            ]
      )
    , ( "actor"
      , E.object
            ([ ( "kind", E.string draft.actorKind ), ( "id", E.string draft.actorId ) ]
                ++ (if String.trim draft.actorModel == "" then [] else [ ( "model", E.string draft.actorModel ) ])
            )
      )
    ]
        ++ (if draft.action == Create then [] else [ ( "state_token", E.string draft.stateToken ) ])
        ++ optionalDigest "input_digest" draft.inputDigest
        ++ optionalDigest "prompt_digest" draft.promptDigest
        ++ optionalDigest "context_digest" draft.contextDigest


optionalDigest : String -> String -> List ( String, E.Value )
optionalDigest key content =
    if String.trim content == "" then [] else [ ( key, E.string (String.trim content) ) ]


lines : String -> List String
lines content =
    content
        |> String.split "\n"
        |> List.map String.trim
        |> List.filter ((/=) "")


strings : String -> E.Value
strings content =
    E.list E.string (lines content)


canonicalBody : String -> String
canonicalBody content =
    String.trim content ++ "\n"


view : { onField : Field -> String -> msg, onResolve : Bool -> msg, onMode : Action -> msg, onAdopt : msg, onSubmit : msg, canSubmit : Bool, canAdopt : Bool, canChooseAction : Bool, canChooseExisting : Bool, draft : Draft, status : String, freshRepository : Maybe Api.Repository, freshInspection : Maybe Api.Inspection } -> Html msg
view controls =
    let
        draft = controls.draft

        named field name content =
            div []
                [ label [ for name ] [ text name ]
                , input [ id name, value content, onInput (controls.onField field) ] []
                ]

        multiline field name content =
            div []
                [ label [ for name ] [ text name ]
                , textarea [ id name, value content, onInput (controls.onField field) ] []
                ]
    in
    div []
        [ h3 [] [ text "Checked operation" ]
        , label [ for "action" ] [ text "Action" ]
        , select [ id "action", disabled (not controls.canChooseAction), onInput (\value -> controls.onMode (actionFrom value)) ]
            (List.map (\action -> option [ value (actionName action), selected (draft.action == action), disabled (action /= Create && not controls.canChooseExisting) ] [ text (actionName action) ]) [ Create, Amend, Scope, Domain, Obsolete, Reactivate ])
        , p [] [ text ("Target: " ++ (if draft.target == "" then "new ADR" else draft.target)) ]
        , p [] [ text ("Original HEAD: " ++ draft.basisHead) ]
        , if draft.stale then p [] [ text "Draft is stale. Refresh the repository, inspect current heads and candidates, then adopt the new tokens." ] else text ""
        , if draft.action == Create || draft.action == Amend then
            div []
                ([ named Title "Title" draft.title
                 , multiline Summary "Summary" draft.summary
                 , multiline Body "Body" draft.body
                 ]
                    ++ (if draft.action == Amend then [ named ChangeSummary "Change summary" draft.changeSummary ] else [ multiline Domains "Domains, one per line" draft.domains, multiline Scopes "Scopes, one per line" draft.scopes ])
                )
          else
            div []
                ([ named Reason "Reason" draft.reason ]
                    ++ (if draft.action == Scope || draft.action == Domain then
                            [ label [ for "change-mode" ] [ text "Change mode" ]
                            , select [ id "change-mode", onInput (controls.onField Mode) ]
                                (List.map (\mode -> option [ value mode, selected (draft.mode == mode) ] [ text mode ]) (if draft.action == Domain then [ "delta", "reviewed", "refine" ] else [ "delta", "reviewed" ]))
                            ]
                                ++ (case draft.mode of
                                        "delta" -> [ multiline Add "Add, one per line" draft.add, multiline Remove "Remove, one per line" draft.remove ]
                                        "refine" -> [ multiline Refinements "Refinements, one per line" draft.refinements ]
                                        _ -> [ multiline (if draft.action == Scope then Scopes else Domains) "Reviewed set, one per line" (if draft.action == Scope then draft.scopes else draft.domains) ]
                                   )
                        else
                            [ label [ for "resolve" ] [ text "Resolve status conflict" ]
                            , input [ id "resolve", type_ "checkbox", checked draft.resolve, onCheck controls.onResolve ] []
                            ]
                       )
                    ++ (if draft.action == Obsolete then [ named Replacement "Replacement ADR (optional)" draft.replacement ] else [])
                )
        , fieldset []
            [ legend [] [ text "Actor and provenance" ]
            , label [ for "actor-kind" ] [ text "Actor kind" ]
            , select [ id "actor-kind", onInput (controls.onField ActorKind) ] (List.map (\kind -> option [ value kind, selected (draft.actorKind == kind) ] [ text kind ]) [ "human", "llm", "service" ])
            , named ActorId "Actor ID" draft.actorId
            , named ActorModel "Model (optional)" draft.actorModel
            , named InputDigest "Input digest (optional)" draft.inputDigest
            , named PromptDigest "Prompt digest (optional)" draft.promptDigest
            , named ContextDigest "Context digest (optional)" draft.contextDigest
            ]
        , h3 [] [ text "Review original and current state" ]
        , case draft.original of
            Just original -> reviewView "Original reviewed state" original
            Nothing -> p [] [ text "No original snapshot has been reviewed yet." ]
        , case controls.freshRepository of
            Just repository ->
                reviewView "Current freshly inspected state" (baseline repository controls.freshInspection)

            Nothing ->
                p [] [ text "Current state is not fresh. Refresh the repository and inspect both views." ]
        , button [ type_ "button", disabled (not controls.canAdopt), onClick controls.onAdopt ]
            [ text (if draft.action == Create then "Review repository and adopt current basis" else "Review heads and adopt current tokens") ]
        , button [ type_ "button", disabled (not controls.canSubmit), onClick controls.onSubmit ] [ text ("Submit " ++ actionName draft.action) ]
        , p [] [ text controls.status ]
        ]


reviewView : String -> ReviewBaseline -> Html msg
reviewView label review =
    div [ ]
        [ h4 [] [ text label ]
        , p [] [ text ("HEAD " ++ review.head ++ " · " ++ Maybe.withDefault "detached" review.headRef) ]
        , p [] [ text ("Repository basis token: " ++ review.basisToken) ]
        , p [] [ text ("ADR state token: " ++ review.stateToken) ]
        , p [] [ text ("Title: " ++ review.title) ]
        , p [] [ text ("Summary: " ++ review.summary) ]
        , pre [] [ text review.body ]
        , p [] [ text ("Domains: " ++ String.join ", " review.domains) ]
        , p [] [ text ("Scopes: " ++ String.join ", " review.scopes) ]
        , p [] [ text ("Heads: " ++ String.join ", " review.heads) ]
        , ul [] (List.map (\candidate -> li [] [ text candidate ]) review.candidates)
        ]


actionFrom : String -> Action
actionFrom raw =
    case raw of
        "amend" -> Amend
        "scope" -> Scope
        "domain" -> Domain
        "obsolete" -> Obsolete
        "reactivate" -> Reactivate
        _ -> Create
