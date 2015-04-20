--TODO: convert from Delta?
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Data.Aeson
import GHC.Generics
import Data.Maybe as Maybe

type Dtuple = ((String, Char), String)

data Delta =
    Delta ((String, Char), String)
      deriving (Read,Show)

readDelta :: String -> Maybe Delta
readDelta s = do
    b <- return $ read s
    return b

--fromDelta :: (Delta a, Dtuple b) => a -> b

d = "((\"q0\",\'1\'), \"q1\")"

x :: Delta
x = Delta (("q0", '2'), "q1")

data DfaJSON =
  DfaJSON { states :: [String]
          , sigma :: String
          , deltamap :: [String]
          , startstate :: String
          , acceptstates :: [String]
          } deriving (Show,Generic)

instance FromJSON DfaJSON
instance ToJSON DfaJSON

jsonFile :: FilePath
jsonFile = "dfa.json"

getJSON :: IO B.ByteString
getJSON = B.readFile jsonFile

maybeReadTup :: String -> Maybe (Int, Char)
maybeReadTup s = do
    [(n, [c])] <- return $ reads s
    return (n, c)

{- maybeReadMap :: String -> Maybe ((a, b), a)
maybeReadMap s = do
    [((is, c), ns)] <- return $ reads s
    return ((is, c), ns)
-}
