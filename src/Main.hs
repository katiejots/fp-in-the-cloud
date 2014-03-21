{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import System.Environment
import System.Posix.Env 
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Data.Default (def)
import Network.Wai.Handler.Warp (settingsPort, settingsHost)
import Data.Conduit.Network
import Network.HTTP.Types.Status
import Database.PostgreSQL.Simple
import qualified Model.Definition as Def
import qualified View.Index
import qualified View.Add
import qualified View.Added
import qualified View.Error

opts :: String -> Int -> Options
opts ip port = def { verbose = 0
                   , settings = (settings def) { settingsHost = Host ip, settingsPort = port }
               }

dbConnInfo :: IO ConnectInfo 
dbConnInfo = do host <- getEnvDefault "OPENSHIFT_POSTGRESQL_DB_HOST" "127.0.0.1"
                port <- getEnvDefault "OPENSHIFT_POSTGRESQL_DB_PORT" "5432"
                user <- getEnvDefault "OPENSHIFT_POSTGRESQL_DB_USERNAME" "postgres"
                password <- getEnvDefault "OPENSHIFT_POSTGRESQL_DB_PASSWORD" ""
                dbName <- getEnvDefault "OPENSHIFT_APP_NAME" "postgres"
                return (ConnectInfo host (read port) user password dbName)

dbConn :: IO Connection
dbConn = do connInfo <- dbConnInfo
            connect connInfo

main :: IO ()
main = do 
     (ip:port:_) <- getArgs
     conn <- dbConn
     _ <- Def.createDefinitionTable conn

     scottyOpts (opts ip (read port)) $ do
         middleware (staticPolicy (noDots >-> addBase "resources"))
         middleware logStdoutDev
         get "/" $ do
           defs <- liftIO (Def.allDefinitions conn)
           html (View.Index.render defs)

         get "/add" $ do
           html View.Add.render

         post "/add" $ do
           phrase <- param "phrase" `rescue` (const next)
           meaning <- param "meaning" `rescue` (const next)
           added <- liftIO (Def.addDefinition conn (Def.Definition phrase meaning))
           case added of
             Left errorMessage -> do
               html (View.Error.render errorMessage)
               status internalServerError500

             Right _ -> 
               html View.Added.render

         post "/add" $ do
           html (View.Error.render "Bad request")
           status badRequest400 
