{-# LANGUAGE OverloadedStrings #-}

module View.Index (render) where

import Control.Monad (forM_)
import Model.Definition
import Text.Blaze.Html5.Attributes (href, class_)
import Text.Blaze.Html.Renderer.Text
import Data.Monoid((<>))
import View.Header
import qualified Data.Text.Lazy as D
import qualified Text.Blaze.Html5 as H

render :: [Definition] -> D.Text
render definitions = renderHtml . H.docTypeHtml $ do
  header
  H.body $ do
    H.h1 "Ahoy! Welcome to Pirate Gold."
    H.h2 "'ere be some golden terms ye ought to be using me hearties..."
    if null definitions 
    then H.p "There are no definitions yet."
    else H.ul . forM_ definitions $ (\def -> H.li $ do 
                                         (H.span H.! class_ "phrase") (H.toHtml (phrase def)) <> ": "
                                         (H.span H.! class_ "meaning") (H.toHtml (meaning def)))
    H.p ((H.a H.! href "/add") "Add definition")
