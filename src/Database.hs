{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Database where

import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Migration as PG
import Database.PostgreSQL.Simple.FromField (FromField, fromField) 
import Database.PostgreSQL.Simple.FromRow (FromRow(fromRow), field)
import Data.Pool
import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import qualified System.Envy as Envy
import GHC.Generics

import Blockchain

instance FromField Block where
    fromField f mdata = Block 
        <$> fromField f mdata
        <*> fromField f mdata
        <*> fromField f mdata
        <*> fromField f mdata
        <*> fromField f mdata

instance FromRow Block where
    fromRow = do
        index <- field
        ts <- field  
        content <- field
        prevHash <- field 
        hash <- field
        return $ Block index ts content prevHash hash

data DbConfig = DbConfig
    { dbHost :: String
    , dbPort :: Int
    , dbUser :: String
    , dbPass :: String
    , dbName :: String
    } deriving (Generic, Show)

instance Envy.DefConfig DbConfig where
    defConfig = DbConfig "localhost" 5432 "postgres" "postgres" "blockchain"

instance Envy.FromEnv DbConfig where
    fromEnv _ = DbConfig 
        <$> Envy.envMaybe "PG_HOST" Envy..!= "localhost"
        <*> Envy.envMaybe "PG_PORT" Envy..!= 5432
        <*> Envy.envMaybe "PG_USER" Envy..!= "postgres"
        <*> Envy.envMaybe "PG_PASS" Envy..!= "postgres"
        <*> Envy.envMaybe "PG_DB"   Envy..!= "blockchain"

-- Create a connection string from config
createConnString :: DbConfig -> PG.ConnectInfo
createConnString DbConfig{..} = PG.ConnectInfo
    { PG.connectHost = dbHost
    , PG.connectPort = fromIntegral dbPort
    , PG.connectUser = dbUser
    , PG.connectPassword = dbPass
    , PG.connectDatabase = dbName
    }

-- Initialize the database pool
initDbPool :: DbConfig -> IO (Pool PG.Connection)
initDbPool config = createPool
    (PG.connect $ createConnString config)
    PG.close
    1  -- number of stripes
    10 -- timeout in seconds
    10 -- max connections per stripe

-- Initialize database schema
initializeDatabase :: Pool PG.Connection -> IO ()
initializeDatabase pool = withResource pool $ \conn -> do
    void $ PG.execute_ conn createBlockchainTable
    where
        createBlockchainTable = "CREATE TABLE IF NOT EXISTS blocks (\
            \id SERIAL PRIMARY KEY, \
            \index INTEGER NOT NULL, \
            \timestamp TIMESTAMP NOT NULL, \
            \data TEXT NOT NULL, \
            \previous_hash TEXT NOT NULL, \
            \hash TEXT NOT NULL)"

-- Add to Database.hs
withConn :: Pool PG.Connection -> (PG.Connection -> IO a) -> IO a
withConn = withResource

-- Database operations
saveBlock :: Pool PG.Connection -> Block -> IO ()
saveBlock pool block = void $ withResource pool $ \conn ->
    PG.execute conn
        "INSERT INTO blocks (index, timestamp, data, previous_hash, hash) \
        \VALUES (?, ?, ?, ?, ?)"
        (index block, timestamp block, dataContent block, previousHash block, currentHash block)

getBlocks :: Pool PG.Connection -> IO [Block]
getBlocks pool = withResource pool $ \conn ->
    PG.query_ conn "SELECT index, timestamp, data, previous_hash, hash FROM blocks ORDER BY index"
