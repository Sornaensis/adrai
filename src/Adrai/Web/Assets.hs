{-# LANGUAGE TemplateHaskell #-}

-- | Bytes embedded into the executable.  There is no filesystem fallback.
module Adrai.Web.Assets
  ( indexHtml,
    applicationCss,
    applicationJavaScript,
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

indexHtml :: ByteString
indexHtml = $(embedFile "web/static/index.html")

applicationCss :: ByteString
applicationCss = $(embedFile "web/static/app.css")

-- P7-02 serves the existing checked-in placeholder unchanged.  P7-04 owns its
-- replacement with the reproducible Elm explorer build.
applicationJavaScript :: ByteString
applicationJavaScript = $(embedFile "web/dist/app.js")
