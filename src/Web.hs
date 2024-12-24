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

type ApiApp = SpockM () () AppState ()

data AppState = AppState
    { blockchain :: IORef [Block]
    }

instance ToJSON Block
instance FromJSON Block

app :: IO ()
app = do
    genesis <- createGenesisBlock
    ref <- newIORef [genesis]
    spockCfg <- defaultSpockCfg () PCNoDatabase (AppState ref)
    runSpock 8999 (spock spockCfg appRoutes)

-- Define API routes
appRoutes :: SpockM () () AppState ()
appRoutes = do
    get "blocks" $ do
        AppState ref <- getState
        chain <- liftIO $ readIORef ref
        json chain

    post "blocks" $ do
        AppState ref <- getState
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
        AppState ref <- getState
        chain <- liftIO $ readIORef ref
        if validateChain chain
            then text "Blockchain is valid"
            else text "Blockchain is invalid"
