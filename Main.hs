{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy  as B
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Data
import           Data.HashMap.Lazy
import           Data.Text.Lazy        (Text)
import           GHC.Generics
import           Network.HTTP.Conduit
import           Prelude               hiding (FilePath)
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           System.Time

default (Text)

data Kommand = Kommand { summary  :: Text
                       , install  :: Maybe Text
                       , preArgs  :: Maybe Text
                       , postArgs :: Maybe Text
                       , commands :: Maybe (HashMap Text Kommand)
                       }
             deriving (Data, Generic, Show, Typeable)

instance FromJSON Kommand

main :: IO ()
main = do
  args <- getArgs
  whenM needToFetchKommands fetchKommandsFile
  putStrLn . show $ args
  putStrLn . show =<< readKommandsFile
  where
    whenM :: forall (m :: * -> *). Monad m => m Bool -> m () -> m ()
    whenM x y = join (ap (liftM when x) (return y))

needToFetchKommands :: IO Bool
needToFetchKommands = do
  fileName <- kommandsFileName
  exists   <- doesFileExist fileName
  if (not exists)
    then return True
    else do currentTime <- getClockTime
            modTime     <- getModificationTime fileName
            let diff = diffClockTimes currentTime modTime
            return (tdDay diff > 1)

fetchKommandsFile :: IO ()
fetchKommandsFile = do
  fileName <- kommandsFileName
  request  <- parseUrl "https://s3.amazonaws.com/knewton-public-src/kommands.json"
  withManager $ \manager -> do
    response <- http request manager
    responseBody response $$+- sinkFile fileName

readKommandsFile :: IO (Either String (HashMap Text Kommand))
readKommandsFile = do
  fileName   <- kommandsFileName
  byteString <- B.readFile fileName
  return . eitherDecode' $ byteString

kommandsFileName :: IO FilePath
kommandsFileName = return . flip (</>) ".kommands.json" =<< getHomeDirectory
