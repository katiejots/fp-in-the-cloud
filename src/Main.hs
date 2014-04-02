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
import qualified Data.Text.Lazy as D
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
    args <- getArgs
    case args of
      [ip,port] -> do conn <- dbConn
                      _ <- Def.createDefinitionTable conn

                      scottyOpts (opts ip (read port)) $ do
                          middleware (staticPolicy (noDots >-> addBase "resources"))
                          middleware logStdoutDev
                          processRoutes conn
      _ -> putStrLn "Required arguments [ip] [port]"

processRoutes :: Connection -> ScottyM ()
processRoutes conn = do
    get "/" (listDefinitions conn)
    get "/add" renderAddForm 
    post "/add" (postDefinition conn)

listDefinitions :: Connection -> ActionM ()
listDefinitions conn = do
    defs <- liftIO (Def.allDefinitions conn)
    html (View.Index.render defs)

renderAddForm :: ActionM ()
renderAddForm = 
    html View.Add.render

postDefinition :: Connection -> ActionM ()
postDefinition conn = do
    parameters <- params 
    maybe returnError addDef (mapM (getParam parameters) ["phrase", "meaning"])
    where getParam prms name = lookup name prms
          returnError = createResponse (View.Error.render "Missing parameter") badRequest400
          addDef (p:m:_) = do 
              added <- liftIO (Def.addDefinition conn (Def.Definition p m))
              case added of
                  Left errorMessage -> createResponse (View.Error.render errorMessage) internalServerError500
                  Right _ -> createResponse View.Added.render ok200 
          addDef _ =  
              createResponse (View.Error.render "Internal error") internalServerError500

createResponse :: D.Text -> Status -> ActionM ()
createResponse view stat = do
           html view
           status stat 
