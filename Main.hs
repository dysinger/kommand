{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

{-|
Module      :  Main
Description :  Main module to interact with the command line user.
Copyright   :  (c) 2013 Knewton
License     :  Apache2

Maintainer  :  Tim Dysinger <tim@dysinger.net>
Stability   :  experimental
Portability :  non-portable (Linux or Mac)

This module will intercept & proxy command line commands.  But serving
as a proxy, this app can assist in the form of guidance This app
assists in search/discovery & help on the CLI.  It is intended to help
teams develop a common set of CLI tools.  It will (in the future)
provide Bash & Zsh completion of the commands it knows about
(described in a JSON document.)
|-}

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy as BS
import           Data.Char
import           Data.Time.Clock
import           Network.HTTP
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO
import           System.Process

-- | The Kommand data type contains the attributes of each Kommand.
data Kommand = Kommand { _id          :: String
                       , _aliases     :: Maybe [String]
                       , _commands    :: Maybe [Kommand]
                       , _description :: Maybe [String]
                       , _examples    :: Maybe [String]
                       , _path        :: Maybe FilePath
                       , _synopsis    :: String
                       , _url         :: Maybe String }

-- | Instance of FromJSOM so we can map JSON records to Kommand.
instance FromJSON Kommand where
  parseJSON (Object v) = Kommand             <$>
                         v .:  "id"          <*>
                         v .:? "aliases"     <*>
                         v .:? "commands"    <*>
                         v .:? "description" <*>
                         v .:? "examples"    <*>
                         v .:? "path"        <*>
                         v .:  "synopsis"    <*>
                         v .:? "url"
  parseJSON _          = mzero

-- | Printing a Kommand should show it's full path by default. If the
-- path isn't given then the kommand is a subcommand or flag.  In this
-- case we print it's id.
instance Show Kommand where
  show (Kommand {_path = Just path, ..}) = path
  show (Kommand {_path = Nothing,   ..}) = _id

-- | Define a Stack of Kommands and Strings
data Stack = Stack [Kommand] [String]

-- | Take a Stack and build a new Stack containing the Kommands (in
-- order), that matched args in the list (in order), plus the
-- remainder of the args that didn't match.
argsToKommands :: Stack -> Stack
argsToKommands (Stack ks as) = Stack foundKommands leftOverArgs
  where
    (_, foundKommands, leftOverArgs) = foldl sort (Just ks, [], []) as
    sort (Nothing,    ks', as') a = (Nothing, ks', a:as')
    sort (Just leafs, ks', as') a =
      case (filter (match a) leafs) of
        []    -> (Nothing,       ks', a:as')
        (k:_) -> (_commands k, k:ks',   as')
    match x (Kommand {_aliases = Nothing, ..}) = _id == x
    match x (Kommand {_aliases = Just xs, ..}) =
      any ((==) (map toLower x)) (_id:xs)

-- | Take a Stack and build a new Stack by popping Kommands onto the
-- args list until we can find a Kommand that has an executable path.
findExecutable :: Stack -> Stack
findExecutable _ = undefined

-- | Take a Stack and build a new Stack by popping Kommands onto the
-- args list until we can find a Kommand that has a description.
findHelp :: Stack -> Stack
findHelp _ = undefined

-- | Lazily keep the kommands json file in sync with what we can find
-- on the internet.
kommands :: IO (Either String [Kommand])
kommands = do
  fname  <- filename
  exists <- doesFileExist fname
  if (not exists)
    then fetchJSON
    else do rightNow <- getCurrentTime
            modTime  <- getModificationTime fname
            when (diffUTCTime rightNow modTime > 60*60)
              fetchJSON
  readJSON
  where
    readJSON  = filename >>= BS.readFile >>= return . eitherDecode
    -- FIXME inspect the response write the file from the response body
    fetchJSON =
      let req = getRequest "http://s3.amazonaws.com/knewton-public-src/kommands.json"
      in simpleHTTP req >> return ()

-- | Calculate the location of our cached json file.
filename :: IO FilePath
filename = do
  home <- getHomeDirectory
  return $ home </> ".kommands.json"

-- | Execute transforms the initial stack from args into Kommands.  It
-- then routes the stack to the appropriate function.
execute :: Stack -> IO ()
execute initialStack = do
  case (argsToKommands initialStack) of
    (Stack [] [])            -> mainHelp
    (Stack [] ("-h":_))      -> mainHelp
    (Stack [] ("--help":_))  -> mainHelp
    s@(Stack [] _)           -> run s
    s@(Stack _ ("-h":_))     -> stackHelp s
    s@(Stack _ ("--help":_)) -> stackHelp s
    s                        -> run s
  where
    mainHelp = putStrLn "OHAI!"
    stackHelp   (Stack []    _) = mzero
    stackHelp s@(Stack (k:_) _) = do
      case (_description k) of
        Just ls -> divider >> mapM_ putStrLn ls >> divider
        Nothing -> mzero
      run s
    divider = putStrLn $ "\n" ++ replicate 80 '-' ++ "\n"
    run (Stack []    _ ) = mzero
    run (Stack (k:_) as) = let n = Nothing in
      runProcess (show k) as n n n n n >>= waitForProcess >>= exitWith

-- | Main program entry point.
main :: IO ()
main = do
  result <- kommands
  case result of -- JSON parse error
    Left er  -> hPutStrLn stderr er >> exitWith (ExitFailure 1)
    Right ks -> execute . Stack ks =<< getArgs
