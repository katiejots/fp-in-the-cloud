{-# LANGUAGE OverloadedStrings #-}

module View.Header (header) where

import Text.Blaze.Html5.Attributes (href, rel)
import qualified Text.Blaze.Html5 as H

header :: H.Html 
header = do
  H.head $ do
    H.title "Pirate Gold"
    H.link H.! rel "stylesheet" H.! href "css/style.css"
