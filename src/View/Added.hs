{-# LANGUAGE OverloadedStrings #-}

module View.Added (render) where

import Text.Blaze.Html5.Attributes (href)
import Text.Blaze.Html.Renderer.Text
import View.Header
import qualified Data.Text.Lazy as D
import qualified Text.Blaze.Html5 as H

render :: D.Text
render = renderHtml . H.docTypeHtml $ do
  header
  H.body $ do
     H.h2 "Added definition" 
     H.p ((H.a H.! href "/") "Home")
