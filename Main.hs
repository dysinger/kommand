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

-- | The Kommand data type contains the attributes of each Kommand.
data Kommand = Kommand { _id          :: String
                       , _aliases     :: Maybe [String]
                       , _commands    :: Maybe [Kommand]
                       , _description :: Maybe [String]
                       , _install     :: Maybe String
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
                         v .:? "install"     <*>
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

-- | Take the Stack of possible Kommands we read from disk and the
-- args given at the command line.  Build a new Stack containing the
-- Kommands, that matched args, plus the remainder of the args that
-- didn't match.
kmdStack :: Stack -> Stack
kmdStack (Stack ks as) = Stack foundKommands leftOverArgs
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

-- | Take the Stack containing Kommands that matched and remainer
-- args.  Build a new Stack by transforming Kommands into args until
-- we can find a parent that has a path.
exeStack :: Stack -> Stack
exeStack (Stack (k:ks) as) = undefined

-- | Lazily keep the kommands json file in sync with what we can find
-- on the internet.
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
    -- FIXME inspect the response write the file from the response body
    fetchJSON =
      simpleHTTP (getRequest "http://hackage.haskell.org/") >> return ()

-- | Calculate the location of our cached json file.
filename :: IO FilePath
filename = do
  home <- getHomeDirectory
  return $ home </> ".kommands.json"

-- | Main program entry point.
main :: IO ()
main = do
  result <- kommands
  case result of
    Left er  -> print er >> exitImmediately (ExitFailure 1)
    Right ks -> do
      as <- getArgs
      let p@(Stack (k':ks') as') = kmdStack (Stack ks as)

      -- if the first kommand in the stack is a path
      --   then if --help flag was given
      --        then print help
      --        else call the kommand with args
      --   else add the kommand to the stack of argsc
      --        and look at the parent kommand (tail)

      case (_description k') of
        Just ls -> mapM_ putStrLn ls >> exitImmediately ExitSuccess
        Nothing -> exitImmediately (ExitFailure 1)
