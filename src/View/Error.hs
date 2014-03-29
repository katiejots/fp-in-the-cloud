{-# LANGUAGE OverloadedStrings #-}

module View.Error (render) where

import Text.Blaze.Html5.Attributes (href)
import Text.Blaze.Html.Renderer.Text
import View.Header
import qualified Data.Text.Lazy as D
import qualified Text.Blaze.Html5 as H

-- $setup
-- >>> import Data.List(isPrefixOf, isSuffixOf)

-- |
--
-- >>> render ""
-- "<!DOCTYPE HTML>\n<html><head><title>Pirate Gold</title><link rel=\"stylesheet\" href=\"css/style.css\"></head><body><h1>Arrr, you&#39;ve run aground matey!</h1><h2>Error: </h2><p><a href=\"/\">Home</a></p></body></html>"
--
-- >>> render "abc"
-- "<!DOCTYPE HTML>\n<html><head><title>Pirate Gold</title><link rel=\"stylesheet\" href=\"css/style.css\"></head><body><h1>Arrr, you&#39;ve run aground matey!</h1><h2>Error: abc</h2><p><a href=\"/\">Home</a></p></body></html>"
--
-- render "abc&def\"ghi"
-- "<!DOCTYPE HTML>\n<html><head><title>Pirate Gold</title><link rel=\"stylesheet\" href=\"css/style.css\"></head><body><h1>Arrr, you&#39;ve run aground matey!</h1><h2>Error: abc&amp;def&quot;ghi</h2><p><a href=\"/\">Home</a></p></body></html>"
--
-- prop> let r = D.unpack (render s) in "<!DOCTYPE HTML>\n<html><head><title>Pirate Gold</title><link rel=\"stylesheet\" href=\"css/style.css\"></head><body><h1>Arrr, you&#39;ve run aground matey!</h1><h2>Error: " `isPrefixOf` r
--
-- prop> let r = D.unpack (render s) in "</h2><p><a href=\"/\">Home</a></p></body></html>" `isSuffixOf` r
render :: String -> D.Text
render err = renderHtml . H.docTypeHtml $ do
  header
  H.body $ do
    H.h1 "Arrr, you've run aground matey!"
    H.h2 ("Error: " >> H.toHtml err)
    H.p ((H.a H.! href "/") "Home")
