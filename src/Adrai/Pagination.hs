{-# LANGUAGE OverloadedStrings #-}

module Adrai.Pagination
  ( Pagination (..),
    PageRequest (..),
    PaginationError (..),
    validatePage,
    paginate,
    unpagedPagination,
    paginationJson,
    renderPagination,
  )
where

import Adrai.Format.Json (JsonValue (..), object)
import Data.Text (Text)
import qualified Data.Text as Text

-- Nullable page fields also represent legacy, explicitly unpaged consumers.
data Pagination = Pagination
  { paginationPage :: Maybe Int,
    paginationPageSize :: Maybe Int,
    paginationTotalResults :: Int,
    paginationTotalPages :: Maybe Int,
    paginationReturned :: Int,
    paginationRemaining :: Int,
    paginationHasNext :: Bool
  }
  deriving (Eq, Show)

data PaginationError = InvalidPage Integer | InvalidPageSize Int
  deriving (Eq, Show)

data PageRequest = PageRequest
  { pageRequestPage :: Integer,
    pageRequestSize :: Int
  }
  deriving (Eq, Show)

unpagedPagination :: Int -> Pagination
unpagedPagination total = Pagination Nothing Nothing total Nothing total 0 False

validatePage :: Integer -> Either PaginationError ()
validatePage page
  | page < 1 || page > toInteger (maxBound :: Int) = Left (InvalidPage page)
  | otherwise = Right ()

-- Offset arithmetic stays in Integer, including valid pages near maxBound.
-- Convert only offsets proven to lie inside the actual list.
paginate :: Integer -> Int -> [a] -> Either PaginationError ([a], Pagination)
paginate page size values = do
  validatePage page
  if size < 1 then Left (InvalidPageSize size) else Right ()
  let total = length values
      offset = (page - 1) * toInteger size
      rows = if offset >= toInteger total then [] else take size (drop (fromInteger offset) values)
      returned = length rows
      remaining = fromInteger (max 0 (toInteger total - offset - toInteger returned))
      pages = if total == 0 then 0 else 1 + (total - 1) `div` size
  Right (rows, Pagination (Just (fromInteger page)) (Just size) total (Just pages) returned remaining (remaining > 0))

paginationJson :: Pagination -> JsonValue
paginationJson pagination = object
  [ ("page", optionalNumber (paginationPage pagination)),
    ("page_size", optionalNumber (paginationPageSize pagination)),
    ("total_results", number (paginationTotalResults pagination)),
    ("total_pages", optionalNumber (paginationTotalPages pagination)),
    ("returned", number (paginationReturned pagination)),
    ("remaining", number (paginationRemaining pagination)),
    ("has_next", JsonBool (paginationHasNext pagination))
  ]
  where
    number = JsonNumber . toInteger
    optionalNumber = maybe JsonNull number

renderPagination :: Pagination -> Text
renderPagination pagination =
  "Showing " <> count (paginationReturned pagination) <> " of " <> count (paginationTotalResults pagination) <> " results"
    <> (case (paginationPage pagination, paginationTotalPages pagination) of
          (Just page, Just 0) -> "; 0 pages (requested page " <> count page <> ")"
          (Just page, Just pages) -> "; page " <> count page <> "/" <> count pages <> if page > pages then " (out of range)" else ""
          _ -> "")
    <> "; " <> count (paginationRemaining pagination) <> " remaining.\n"
  where
    count = Text.pack . show
