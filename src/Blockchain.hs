{-# LANGUAGE DeriveGeneric #-}

module Blockchain where

import Data.Time (getCurrentTime, UTCTime)
import Data.ByteString.Char8 (pack)
import Crypto.Hash (Digest, SHA256, hash)
import GHC.Generics (Generic)
import Data.List (find)

-- Block structure
data Block = Block
    { index        :: Int
    , timestamp    :: UTCTime
    , dataContent  :: String
    , previousHash :: String
    , currentHash  :: String
    } deriving (Show, Generic)

-- Function to hash a block
hashBlock :: Block -> String
hashBlock block =
    let blockString = show (index block) ++
                      show (timestamp block) ++
                      dataContent block ++
                      previousHash block
    in show (hash (pack blockString) :: Digest SHA256)

-- Genesis block
createGenesisBlock :: IO Block
createGenesisBlock = do
    now <- getCurrentTime
    let genesis = Block 0 now "Genesis Block" "0" ""
    return genesis { currentHash = hashBlock genesis }

-- Create a new block
createBlock :: Block -> String -> IO Block
createBlock prevBlock newData = do
    now <- getCurrentTime
    let newBlock = Block
            { index = index prevBlock + 1
            , timestamp = now
            , dataContent = newData
            , previousHash = currentHash prevBlock
            , currentHash = ""
            }
    return newBlock { currentHash = hashBlock newBlock }

-- Validate a block
validateBlock :: Block -> Block -> Bool
validateBlock prevBlock currentBlock =
    previousHash currentBlock == currentHash prevBlock &&
    currentHash currentBlock == hashBlock currentBlock

-- Validate the entire blockchain
validateChain :: [Block] -> Bool
validateChain [] = True
validateChain [_] = True
validateChain (b1:b2:bs) = validateBlock b1 b2 && validateChain (b2:bs)

-- Example usage
main :: IO ()
main = do
    genesis <- createGenesisBlock
    block1 <- createBlock genesis "First block"
    block2 <- createBlock block1 "Second block"
    let blockchain = [genesis, block1, block2]
    print blockchain
    print $ "Is blockchain valid? " ++ show (validateChain blockchain)
