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
  show (Kommand {_path = Nothing, _id = i}) = i
  show (Kommand {_path = Just p}) = p

-- | Define a Stack of Kommands and Strings
data Stack = Stack [Kommand] [String] deriving Show

-- | Build a new Stack containing the Kommands (in order), that
-- matched args in the list (in order), plus the remainder of the args
-- that didn't match.
initialStack :: Stack -> Stack
initialStack (Stack ks as) = Stack foundKs leftOver
  where
    (_, foundKs, leftOver) = foldl sort (Just ks, [], []) as
    sort (Nothing, ks', as') a =
      case (filter (match a) internalKommands) of
        (k:_) -> (Nothing, k:ks', as')
        _     -> (Nothing, ks', a:as')
    sort (Just leafs, ks', as') a =
      case (filter (match a) internalKommands) of
        (k:_) -> (Just leafs, k:ks', as')
        _     -> case (filter (match a) leafs) of
          (k:_) -> (_commands k, k:ks', as')
          _     -> (Nothing, ks', a:as')
    match x (Kommand {_aliases = Nothing, _id = i}) = i == x
    match x (Kommand {_aliases = Just xs, _id = i}) =
      any ((==) (map toLower x)) (i:xs)

-- | Take a Stack and build a new Stack by popping Kommands onto the
-- args list until we can find a Kommand that has an executable path.
exeStack :: Stack -> Stack
exeStack (Stack [] _) = error "Kommand tree didn't have an executable path."
exeStack s@(Stack (Kommand{_path=Just _}:_) _)      = s
exeStack (Stack (k@(Kommand{_path=Nothing}):ks) as) = 
  exeStack (Stack ks ((show k):as))

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
      in -- simpleHTTP req
         return ()

-- | Calculate the location of our cached json file.
filename :: IO FilePath
filename = do
  home <- getHomeDirectory
  return $ home </> ".kommands.json"

-- | Route transforms the initial stack from args into Kommands.  It
-- then routes the stack to the appropriate function.
route :: Stack -> IO ()
route (Stack [] []) = showKommands
route (Stack ((Kommand{_id="help"}):k:ks) as) = showHelp (Stack (k:ks) as)
route (Stack (k:(Kommand{_id="help"}):ks) as) = showHelp (Stack (k:ks) as)
route s = run s

showKommands :: IO ()
showKommands = putStrLn "OHAI!" -- TODO list all Kommands + Synopsis of each

showHelp :: Stack -> IO ()
showHelp s = showDescription s >> showExamples s

showDescription :: Stack -> IO ()
showDescription (Stack (Kommand{_description = Just ls}:_) _) = do
  putStrLn "\nDescription:\n"
  mapM_ (putStrLn . (++) "  ") ls
  putStrLn "\n"
showDescription _ = return ()

showExamples :: Stack -> IO ()
showExamples (Stack (Kommand{_examples = Just ls}:_) _) = do
  putStrLn "\nExamples:\n"
  mapM_ (putStrLn . (++) "  ") ls
  putStrLn "\n"
showExamples _ = return ()

run :: Stack -> IO ()
run s = do
  case (exeStack s) of
    Stack [] []     -> return ()
    Stack [] (a:as) -> runProcess a as n n n n n >>= waitForProcess >>= exitWith
    Stack (k:_) as  -> runProcess (show k) as n n n n n >>= waitForProcess >>= exitWith
  where
    n = Nothing

-- | Main program entry point.
main :: IO ()
main = do
  result <- kommands
  case result of -- JSON parse error
    Left  er -> hPutStrLn stderr er >> exitWith (ExitFailure 1)
    Right ks -> route . initialStack . Stack ks =<< getArgs

internalKommands :: [Kommand]
internalKommands = [
  Kommand { _id          = "help"
          , _aliases     = Just [ "-?", "-h", "--help" ]
          , _synopsis    = "Built in Help Kommand"
          , _commands    = Nothing
          , _description = Nothing
          , _examples    = Nothing
          , _path        = Nothing
          , _url         = Nothing }
  ]
