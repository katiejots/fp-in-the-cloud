{-# LANGUAGE OverloadedStrings #-}

module View.Add (render) where

import Text.Blaze.Html5.Attributes (action, method, name, type_)
import Text.Blaze.Html.Renderer.Text
import View.Header
import qualified Data.Text.Lazy as D 
import qualified Text.Blaze.Html5 as H

-- |
--
-- >>> render
-- "<!DOCTYPE HTML>\n<html><head><title>Pirate Gold</title><link rel=\"stylesheet\" href=\"css/style.css\"></head><body><h2>Add a definition to the treasure chest</h2><form action=\"/add\" method=\"post\"><p>Phrase: </p><input name=\"phrase\" type=\"text\"><p>Meaning: </p><input name=\"meaning\" type=\"text\"><p><input type=\"submit\"></p></form></body></html>"
render :: D.Text
render = renderHtml . H.docTypeHtml $ do
  header
  H.body $ do
    H.h2 "Add a definition to the treasure chest"
    H.form H.! action "/add" H.! method "post" $ do
      H.p "Phrase: "
      H.input H.! name "phrase" H.! type_ "text"
      H.p "Meaning: "
      H.input H.! name "meaning" H.! type_ "text"
      H.p (H.input H.! type_ "submit")

