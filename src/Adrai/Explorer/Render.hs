{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE LambdaCase #-}

-- | Terminal rendering functions for the explorer.
--
-- Produces formatted 'Text' output suitable for the TUI, using ANSI
-- escape codes for basic coloring and bold text.
--
-- Key functions:
--
-- * 'renderCollapsed' — render a collapsed ADR view
-- * 'renderExploded' — render an exploded / lineage view
-- * 'renderLineage' — render a lineage graph
-- * 'renderConflict' — render conflict resolution info
--
-- Colors follow a consistent palette:
--
-- * Cyan = headings and section separators
-- * Yellow = warnings / conflict markers
-- * Green = success / active indicators
-- * Red = error / obsolete indicators
-- * Blue = metadata labels

module Adrai.Explorer.Render
  ( -- * ANSI helpers
    ansiReset,
    ansiBold,
    ansiColor,
    ansiCyan,
    ansiYellow,
    ansiGreen,
    ansiRed,
    ansiBlue,
    ansiUnderline,
    ansiBright,

    -- * Search result rendering
    renderSearchResult,
    renderSearchResults,

    -- * ADR detail rendering
    renderCollapsed,
    renderExploded,

    -- * Lineage rendering
    renderLineage,

    -- * Conflict rendering
    renderConflict,

    -- * History rendering
    renderHistory,

    -- * Help text
    renderHelp,

    -- * Utility
    wrapText,
    terminalWidth,
  )
where

import Adrai.History
  ( EvolutionSummary (..),
    HistoryOperation (..),
    HistoryProjection (..),
    RevisionIdentity (..),
  )
import Adrai.Query
  ( CollapsedProjection (..),
    ExplodedProjection (..),
    ExplodedOperation (..),
    ExplodedItem (..),
    CollapsedProvenance (..),
    ProjectionCounts (..),
    PublicIssue (..),
    ProvenanceProjection (..),
    ResolutionCandidate (..),
    ResolutionEntry (..),
    ResolutionConflictKind (..),
    ResolutionState (..),
    SearchProjection (..),
    SearchResult (..),
  )
import Adrai.Types
  ( Actor (..),
    actorId,
    operationIdText,
    recordIdText,
    adrIdText,
    stateTokenText,
  )
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T

-- ---------------------------------------------------------------------------
-- ANSI helpers
-- ---------------------------------------------------------------------------

ansiReset   :: Text
ansiReset   = "\ESC[0m"

ansiBold    :: Text -> Text
ansiBold    txt = "\ESC[1m" <> txt

ansiCyan    :: Text -> Text
ansiCyan    txt = "\ESC[36m" <> txt

ansiYellow  :: Text -> Text
ansiYellow  txt = "\ESC[33m" <> txt

ansiGreen   :: Text -> Text
ansiGreen   txt = "\ESC[32m" <> txt

ansiRed     :: Text -> Text
ansiRed     txt = "\ESC[31m" <> txt

ansiBlue    :: Text -> Text
ansiBlue    txt = "\ESC[34m" <> txt

ansiUnderline :: Text
ansiUnderline = "\ESC[4m"

ansiBright :: Text -> Text
ansiBright  txt = ansiBold txt <> ansiReset

-- | Apply a color to text, always resetting afterwards.
ansiColor :: Text -> Text -> Text
ansiColor color txt = color <> txt <> ansiReset

-- ---------------------------------------------------------------------------
-- Text wrapping
-- ---------------------------------------------------------------------------

-- | Wrap text to a given column width, splitting on whitespace boundaries.
wrapText :: Int -> Text -> [Text]
wrapText width txt =
  let cols = max 8 width
  in concatMap (wrapLine cols) (T.lines txt)

wrapLine :: Int -> Text -> [Text]
wrapLine _ "" = []
wrapLine cols line
  | T.length line <= cols = [line]
  | otherwise =
      let (prefix, _) = T.break (\c -> c == ' ' || c == '\t') (T.drop (cols - 1) line)
      in case T.uncons (T.drop (T.length prefix + 1) line) of
           Nothing -> [line]
           Just (_, rest) ->
             let wrapped = T.take (cols - 1) line
             in wrapped : wrapLine cols (T.drop (T.length wrapped) rest)

-- ---------------------------------------------------------------------------
-- Terminal width detection
-- ---------------------------------------------------------------------------

-- | Attempt to detect terminal width, falling back to 100.
terminalWidth :: IO Int
terminalWidth = pure 100

-- ---------------------------------------------------------------------------
-- Search result rendering
-- ---------------------------------------------------------------------------

-- | Render a single search result line.
--
-- Format: @ 1. A0B1C2D3 Some Title  [0.1234]@
-- Flags like (conflict) or (obsolete) are appended when present.
renderSearchResult :: Int -> SearchResult -> Text
renderSearchResult index result =
  let id_ = T.take 12 (adrIdText (searchResultAdr result))
      title = searchResultTitle result
      score = searchResultScore result
      flags = buildFlags result
      suffix = if null flags then "" else " (" <> T.intercalate ", " flags <> ")"
  in  ansiGreen (T.pack (show index) <> ". ")
    <> ansiBold (id_ <> " ")
    <> title
    <> " "
    <> ansiBlue ("[" <> formatScore score <> suffix <> ansiReset)
  where
    formatScore v =
      let intPart = floor v :: Int
          frac = (abs (v - fromIntegral intPart)) * 10000
          fracStr = padLeft4 (show (floor frac :: Int))
      in T.pack (show intPart <> "." <> fracStr)

    padLeft4 :: String -> String
    padLeft4 s
      | length s >= 4 = take 4 s
      | otherwise     = replicate (4 - length s) '0' <> s

    buildFlags :: SearchResult -> [Text]
    buildFlags r =
      [ ansiYellow "conflict" | searchResultConflict r /= Nothing ]
      <> [ ansiRed "obsolete" | searchResultObsolete r ]

-- | Render a batch of search results with numbering.
renderSearchResults :: SearchProjection -> [Text]
renderSearchResults projection =
  let results = searchProjectionResults projection
  in if null results
        then [ansiRed "No results found."]
        else [ renderSearchResult (i + 1) r | (i, r) <- zip [0 :: Int ..] results ]

-- ---------------------------------------------------------------------------
-- Collapsed ADR rendering
-- ---------------------------------------------------------------------------

-- | Render a collapsed ADR view: title, status, domains, scope, body,
-- provenance, and issues.
renderCollapsed :: CollapsedProjection -> [Text]
renderCollapsed proj =
  let title      = collapsedTitle proj
      adr        = adrIdText (collapsedAdr proj)
      status     = collapsedStatus proj
      domains    = collapsedDomains proj
      appliesTo  = collapsedAppliesTo proj
      counts     = collapsedCounts proj
      evolution  = collapsedEvolution proj
      provenance = collapsedProvenance proj
      issues     = collapsedIssues proj
      resolution = collapsedResolution proj
  in  [ ansiCyan (ansiBold title) ]
     ++ [ ansiBold ("ADR " <> T.take 12 adr <> "  status=" <> statusText status) ]
     ++ conflictLine resolution
     ++ [ "record: " <> maybe "-" recordIdText (collapsedRecord proj) ]
     ++ [ "domains: " <> statusOr "-" (T.intercalate ", " domains) ]
     ++ [ "scope: "   <> statusOr "-" (T.intercalate ", " appliesTo) ]
     ++ [ "editions: " <> T.pack (show (projectionEditions counts))
        <> "  scope revisions: " <> T.pack (show (projectionScopeRevisions counts))
        <> "  domain revisions: " <> T.pack (show (projectionDomainRevisions counts))
        <> "  status revisions: " <> T.pack (show (projectionStatusRevisions counts))
        ]
     ++ evolutionLines evolution
     ++ ["", ansiCyan (ansiUnderline <> "SUMMARY" <> ansiReset)]
     ++ wrapText 100 (collapsedSummary proj)
     ++ ["", ansiCyan (ansiUnderline <> "DECISION" <> ansiReset)]
     ++ wrapText 100 (collapsedBody proj)
     ++ provenanceLines provenance
     ++ issueLines issues

  where
    conflictLine :: ResolutionState -> [Text]
    conflictLine r
      | resolutionStateRequired r =
          case resolutionStateConflicts r of
            conflict : _ -> [ansiYellow ("CONFLICT: " <> resolutionSummary conflict)]
            [] -> []
      | otherwise = []

    statusText :: Text -> Text
    statusText s
      | s == "active"   = ansiGreen s
      | s == "obsolete" = ansiRed s
      | otherwise       = ansiYellow s

    statusOr :: Text -> Text -> Text
    statusOr _ "" = "-"
    statusOr _ s  = s

    evolutionLines :: EvolutionSummary -> [Text]
    evolutionLines ev
      | evolutionSummaryText ev == "" = []
      | otherwise =
          [ "", ansiCyan (ansiUnderline <> "EVOLUTION" <> ansiReset)
          , evolutionSummaryText ev
          ]

    provenanceLines :: CollapsedProvenance -> [Text]
    provenanceLines prov
      | collapsedEffectiveProvenance prov == Nothing = []
      | otherwise =
          [ "", ansiCyan (ansiUnderline <> "PROVENANCE" <> ansiReset)
          ] ++ concatMap provenanceDetail
                (catMaybes [ collapsedCreatedProvenance prov
                          , collapsedEffectiveProvenance prov ])
      where
        provenanceDetail pp =
          [ "  actor: " <> actorId (projectedActor pp)
            <> "  claimed: " <> T.pack (show (projectedClaimedAt pp))
          ] ++
            [ "  basis: " <> T.take 16 (projectedBasis pp) ]

    issueLines :: [PublicIssue] -> [Text]
    issueLines iss
      | null iss = []
      | otherwise =
          ["", ansiCyan (ansiUnderline <> "ISSUES" <> ansiReset)]
          ++ map renderIssue iss

    renderIssue :: PublicIssue -> Text
    renderIssue issue =
      ansiRed (ansiBold (T.toUpper (publicIssueSeverity issue)))
        <> " "
        <> ansiYellow (publicIssueCode issue)
        <> ": "
        <> publicIssueMessage issue

-- ---------------------------------------------------------------------------
-- Exploded / Lineage rendering
-- ---------------------------------------------------------------------------

-- | Render an exploded ADR view.
renderExploded :: ExplodedProjection -> [Text]
renderExploded = renderLineage

-- | Render lineage information for an exploded view.
renderLineage :: ExplodedProjection -> [Text]
renderLineage proj =
  let adr  = adrIdText (explodedAdr proj)
      ops  = explodedOperations proj
  in  [ ansiCyan (ansiBold ("LINEAGE " <> T.take 12 adr)) ]
     ++ [ "state " <> ansiBlue (T.take 16 (stateTokenText (explodedStateToken proj))) ]
     ++ conflictLine proj
     ++ [""]
     ++ concat [ renderOperation (i + 1) op | (i, op) <- zip [0 :: Int ..] ops ]

  where
    conflictLine :: ExplodedProjection -> [Text]
    conflictLine p =
      case explodedResolution p of
        r
          | not (resolutionStateResolved r)
            && not (null (resolutionStateConflicts r)) ->
            case resolutionStateConflicts r of
              conflict : _ -> [ansiYellow ("! " <> resolutionSummary conflict)]
              [] -> []
        _ -> []

    renderOperation :: Int -> ExplodedOperation -> [Text]
    renderOperation idx op =
      let prov = explodedOperationProvenance op
          ops  = explodedOperationId op
      in  [ "● " <> ansiGreen (printf "%02d" (fromIntegral idx)) <> " "
               <> ansiBlue (T.take 12 (operationIdText ops))
               <> "  " <> actorId (projectedActor prov)
               <> "  " <> T.pack (show (projectedClaimedAt prov))
           ]
          ++ concat [ renderItem item | item <- explodedOperationItems op ]
          ++ [""]
      where
        printf :: String -> Double -> Text
        printf fmt n = T.pack (formatInt fmt n)

        formatInt :: String -> Double -> String
        formatInt ('0':'.':_:rest) v =
          let frac = abs v * 10000
              fracStr = padLeft4 (show (floor frac :: Int))
          in show (floor v :: Int) <> "." <> fracStr <> formatInt rest 0
        formatInt (h:t) v = h : formatInt t v
        formatInt "" _ = ""

        padLeft4 :: String -> String
        padLeft4 s
          | length s >= 4 = take 4 s
          | otherwise = replicate (4 - length s) '0' <> s

        renderItem :: ExplodedItem -> [Text]
        renderItem item =
          let prefix  = T.take 12 (explodedItemId item)
              indent  = "  "
              parents = if null (explodedItemParents item)
                        then ""
                        else "  ← " <> T.intercalate "," (map (T.take 10) (explodedItemParents item))
           in case explodedItemType item of
                "decision" ->
                  replicate
                    (length (explodedItemDiffs item))
                    (indent <> "D " <> ansiYellow (explodedItemEvent item) <> "  " <> ansiBlue prefix <> parents)
                _ ->
                  [ indent <> "C " <> ansiCyan (maybe "" (\r -> r <> " ") (explodedItemRelation item)) <> "  " <> ansiBlue prefix <> parents
                  ] ++ metadataLines
          where
            metadataLines = case explodedItemType item of
              "connection" -> case explodedItemRelation item of
                Just "applies_to" ->
                  case explodedItemSummary item of
                    Just s -> [ "    scope: " <> s ]
                    Nothing -> []
                Just "domains" ->
                  case explodedItemDomains item of
                    ds | not (null ds) -> [ "    domains: " <> T.intercalate ", " ds ]
                    _ -> []
                _ -> []
              _ -> []

-- ---------------------------------------------------------------------------
-- Conflict rendering
-- ---------------------------------------------------------------------------

-- | Render conflict resolution information.
renderConflict :: Text -> [ResolutionEntry] -> [Text]
renderConflict adr conflicts
  | null conflicts = [ ansiGreen ("No conflicts for " <> T.take 12 adr) ]
  | otherwise =
      [ ansiCyan (ansiBold ("CONFLICTS: " <> T.take 12 adr)) ]
      ++ [""]
      ++ concat (map renderConflictsEntry conflicts)

renderConflictsEntry :: ResolutionEntry -> [Text]
renderConflictsEntry entry =
  let label = conflictLabel (resolutionConflictKind entry)
      heads = resolutionHeads entry
      summary = resolutionSummary entry
  in  [ ansiYellow ("  " <> label <> ": " <> summary) ]
     ++ [ "    heads: " <> T.intercalate ", " (map (T.take 12) heads) ]
     ++ [ "    candidates: " <> T.intercalate ", " (map resolutionCandidateId (resolutionCandidates entry)) ]

  where
    conflictLabel :: ResolutionConflictKind -> Text
    conflictLabel k = case k of
      DecisionConflict  -> "decision"
      ScopeConflict     -> "scope"
      DomainConflict    -> "domain"
      StatusConflict    -> "status"
      IntegrityConflict -> "integrity"

-- ---------------------------------------------------------------------------
-- History rendering
-- ---------------------------------------------------------------------------

-- | Render a history projection.
renderHistory :: HistoryProjection -> [Text]
renderHistory proj =
  let ops   = historyProjectionOperations proj
      rev   = historyProjectionRevision proj
  in  [ ansiCyan (ansiBold ("HISTORY " <> T.take 12 (revisionResolved rev))
                 <> " (" <> T.pack (show (length ops)) <> " operations)") ]
     ++ [""]
     ++ map renderHistoryOp ops

renderHistoryOp :: HistoryOperation -> Text
renderHistoryOp op =
  ansiGreen (T.pack (show (historyOperationClaimedAt op)))
    <> "  "
    <> ansiBlue (T.take 12 (adrIdText (historyOperationAdr op)))
    <> "  "
    <> (historyOperationLabel op)
    <> "  "
    <> (historyOperationTitle op)
    <> extra
  where
    extra = case historyOperationReason op of
      Just r  -> "  " <> r
      Nothing -> ""

-- ---------------------------------------------------------------------------
-- Help text
-- ---------------------------------------------------------------------------

-- | Render the help text for the explorer.
renderHelp :: [Text]
renderHelp =
  [ ansiCyan (ansiBold "ADRAI Terminal Explorer")
  , ""
  , "This terminal explorer currently prints placeholders for read commands."
  , "At the shell for real data: adrai show ADR_ID, adrai search QUERY,"
  , "adrai history ADR_ID, or adrai web (from the worktree)."
  , "Start in an existing Git repository: adrai init, then adrai create --help."
  , ""
  , ansiBold "Explorer commands:"
  , "  " <> ansiCyan ":help" <> "                           Show this help (help also works)"
  , "  " <> ansiCyan "search QUERY" <> "                    Placeholder search; bare text also parses as search"
  , "  " <> ansiCyan "show ADR_ID" <> "                     Placeholder ADR detail"
  , "  " <> ansiCyan "view ADR_ID [collapsed|exploded]" <> "  Placeholder ADR view"
  , "  " <> ansiCyan "history [ADR_ID]" <> "                Placeholder operation history"
  , "  " <> ansiCyan "conflicts" <> "                       Placeholder conflict list"
  , ""
  , ansiBold "Editing:"
  , "  " <> ansiCyan "status ADR_ID active|obsolete" <> "  Commit a status change; exit on success"
  , "  Terminal create and amend input is unavailable."
  , "  At the shell use adrai create --help, adrai amend --help, or adrai web."
  , ansiBold "Exit: " <> ansiCyan "exit" <> " / " <> ansiCyan "quit" <> " / " <> ansiCyan ":q"
  ]
