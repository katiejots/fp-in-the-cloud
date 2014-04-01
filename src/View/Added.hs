{-# LANGUAGE OverloadedStrings #-}

module View.Added (render) where

import Text.Blaze.Html5.Attributes (href, class_)
import Text.Blaze.Html.Renderer.Text
import View.Header
import qualified Data.Text.Lazy as D
import qualified Text.Blaze.Html5 as H

-- |
--
-- >>> render
-- "<!DOCTYPE HTML>\n<html><head><title>Pirate Gold</title><link rel=\"stylesheet\" href=\"css/style.css\"></head><body><h2>Added definition</h2><p><a href=\"/\">Home</a></p></body></html>"
render :: D.Text
render = renderHtml . H.docTypeHtml $ do
  header
  H.body H.! class_ "added" $ do
     H.h2 "Added definition" 
     H.p ((H.a H.! href "/") "Home")
