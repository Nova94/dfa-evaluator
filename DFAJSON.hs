{- Author - Lisa Gray
   Version - 1.0
   Date - 4/24/15
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module DFAJSON where

import qualified Data.ByteString.Lazy as B
import Control.Applicative
import Data.Aeson
import GHC.Generics
import Data.Maybe as Maybe

type Dtuple = ((String, Char), String)

toDelta :: String -> Maybe Dtuple
toDelta s = do
    b <- return $ read s
    return b

-- y <- getJSON
-- decode y :: Maybe DfaJSON
--fromJust y

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

readJSON = do
    y <- getJSON
    return (fromJust(decode y :: Maybe DfaJSON))

getDeltaMapJSON :: [String] -> [Dtuple]
getDeltaMapJSON [] = []
getDeltaMapJSON [x] = [(fromJust (toDelta x))]
getDeltaMapJSON (x:xs) =
    (fromJust (toDelta x)) : getDeltaMapJSON xs
