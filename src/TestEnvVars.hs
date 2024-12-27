{-# LANGUAGE DeriveGeneric #-}

module TestEnvVars where

import Control.Applicative ((<*>), (<$>))
import qualified System.Envy as Envy
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import GHC.Generics

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

testDecode :: IO ()
testDecode = do
    result <- Envy.decodeEnv :: IO (Either String DbConfig)
    case result of
        Left err   -> putStrLn $ "Error: " ++ err
        Right conf -> print conf
