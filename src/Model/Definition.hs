{-# LANGUAGE OverloadedStrings #-}

module Model.Definition (
              Definition(..)
            , createDefinitionTable
            , allDefinitions
            , addDefinition
            ) where

import Data.Text
import Control.Applicative
import Data.Int (Int64)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.ToField
import System.IO.Error (catchIOError)

data Definition = Definition { phrase :: Text
                             , meaning :: Text
                             } deriving (Eq, Show)

instance FromRow Definition where
  fromRow = Definition <$> field <*> field

instance ToRow Definition where
  toRow (Definition p m) = [toField p, toField m]

createDefinitionTable :: Connection -> IO Int64 
createDefinitionTable conn = execute_ conn
    "CREATE TABLE IF NOT EXISTS definition (\
      \ id serial PRIMARY KEY, \
      \ phrase TEXT, \ 
      \ meaning TEXT);"

allDefinitions :: Connection -> IO [Definition]
allDefinitions conn = query_ conn "SELECT phrase, meaning FROM definition"

addDefinition :: Connection -> Definition -> IO (Either String Int64)
addDefinition conn def = executeAdd `catchIOError` handleFormatError 
                     where 
                       executeAdd = do 
                         rowsChanged <- execute conn "INSERT INTO definition (phrase, meaning) VALUES (?,?)"
                           (phrase def, meaning def)
                         return (case rowsChanged of 0 -> Left "Internal error"
                                                     _ -> Right rowsChanged)
                       handleFormatError _ = return (Left "Internal formatting error")
