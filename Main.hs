{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import           Data.Char
import qualified Data.ByteString.Lazy  as BS
import           Data.Time.Clock
import           Network.HTTP
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath.Posix
import           System.Posix.Process

data Kommand = Kommand { _id          :: String
                       , _aliases     :: Maybe [String]
                       , _commands    :: Maybe [Kommand]
                       , _description :: Maybe [String]
                       , _install     :: Maybe String
                       , _path        :: Maybe FilePath
                       , _synopsis    :: String
                       , _url         :: Maybe String }

instance FromJSON Kommand where
  parseJSON (Object v) = Kommand             <$>
                         v .:  "id"          <*>
                         v .:? "aliases"     <*>
                         v .:? "commands"    <*>
                         v .:? "description" <*>
                         v .:? "install"     <*>
                         v .:? "path"        <*>
                         v .:  "synopsis"    <*>
                         v .:? "url"
  parseJSON _          = mzero

instance Show Kommand where
  show (Kommand {_path = Nothing,   ..}) = _id
  show (Kommand {_path = Just path, ..}) = path

data Stack = Stack [Kommand] [String]

main :: IO ()
main = do
  result <- kommands
  case result of
    Left er  -> print er >> exitImmediately (ExitFailure 1)
    Right ks -> do
      as <- getArgs
      let p@(Stack (k':ks') as') = split ks as
      case (_description k') of
        Just ls -> mapM_ putStrLn ls >> exitImmediately ExitSuccess
        Nothing -> exitImmediately (ExitFailure 1)

split :: [Kommand] -> [String] -> Stack
split ks as = Stack foundKommands leftOverArgs
  where
    (_, foundKommands, leftOverArgs) = foldl reduce (Just ks, [], []) as
    reduce (Nothing,    ks', as') a = (Nothing, ks', a:as')
    reduce (Just leafs, ks', as') a =
      case (filter (match a) leafs) of
        []    -> (Nothing,       ks', a:as')
        (k:_) -> (_commands k, k:ks',   as')
    match x (Kommand {_aliases = Nothing, ..}) = _id == x
    match x (Kommand {_aliases = Just xs, ..}) =
      any ((==) (map toLower x)) (_id:xs)

kommands :: IO (Either String [Kommand])
kommands = do
  fname  <- filename
  exists <- doesFileExist fname
  if (not exists)
    then fetchJSON >> readJSON
    else do rightNow <- getCurrentTime
            modTime  <- getModificationTime fname
            if (diffUTCTime rightNow modTime > 60*60)
              then fetchJSON >> readJSON
              else readJSON
  where
    readJSON  = filename >>= BS.readFile >>= return . eitherDecode
    fetchJSON =
      simpleHTTP (getRequest "http://hackage.haskell.org/") >> return ()

filename :: IO FilePath
filename = do
  home <- getHomeDirectory
  return $ home </> ".kommands.json"
