{-# LANGUAGE OverloadedStrings #-}

module View.Error (render) where

import Text.Blaze.Html5.Attributes (href)
import Text.Blaze.Html.Renderer.Text
import View.Header
import qualified Data.Text.Lazy as D
import qualified Text.Blaze.Html5 as H

render :: String -> D.Text
render err = renderHtml . H.docTypeHtml $ do
  header
  H.body $ do
    H.h1 "Arrr, you've run aground matey!"
    H.h2 ("Error: " >> H.toHtml err)
    H.p ((H.a H.! href "/") "Home")
