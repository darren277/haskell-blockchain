{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Web where

import Web.Spock
import Web.Spock.Config
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Blockchain (Block(..), createGenesisBlock, createBlock, validateChain)
import Network.HTTP.Types.Status (status400)

import qualified Database.PostgreSQL.Simple as PG
import Data.Pool (Pool)
import System.Envy (decodeEnv)

import Database (initDbPool, withConn, DbConfig)
import WebUserRoutes (Api, userRoutes)

import AppTypes (AppState(..))

type ApiApp = SpockM () () AppState ()

instance ToJSON Block
instance FromJSON Block

app :: IO ()
app = do
   genesis <- createGenesisBlock
   ref <- newIORef [genesis]
   configResult <- decodeEnv
   case configResult of
       Left err -> error $ "Failed to load config: " ++ err
       Right config -> do
           pool <- initDbPool config
           spockCfg <- defaultSpockCfg () PCNoDatabase (AppState ref pool)
           runSpock 8999 (spock spockCfg appRoutes)

-- Update pattern match:
appRoutes :: Api
appRoutes = do
    get "blocks" $ do
       AppState ref pool <- getState  -- Add pool here
       chain <- liftIO $ readIORef ref
       json chain

    post "blocks" $ do
        AppState ref pool <- getState
        maybeNewData <- jsonBody
        chain <- liftIO $ readIORef ref
        case maybeNewData of
            Nothing -> do
                setStatus status400
                text "Invalid block data"
            Just newData -> do
                chain <- liftIO $ readIORef ref
                case chain of
                    [] -> text "Blockchain is empty"
                    (lastBlock:_) -> do
                        newBlock <- liftIO $ createBlock lastBlock (dataContent newData)
                        liftIO $ writeIORef ref (chain ++ [newBlock])
                        json newBlock

    get "validate" $ do
        AppState ref pool <- getState
        chain <- liftIO $ readIORef ref
        if validateChain chain
            then text "Blockchain is valid"
            else text "Blockchain is invalid"
    
    AppState _ pool <- getState
    userRoutes pool
