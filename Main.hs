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
import qualified Data.ByteString.Lazy       as DBL
import qualified Data.ByteString.Lazy.Char8 as DBLC
import           Data.Char
import           Data.Time.Clock
import           Network.HTTP
import           System.Cmd
import           System.Directory
import           System.Environment
import           System.Exit
import           System.FilePath
import           System.IO

-- | The Kommand data type contains the attributes of each Kommand.
data Kommand = K { _id          :: String
                 , _aliases     :: Maybe [String]
                 , _commands    :: Maybe [Kommand]
                 , _description :: Maybe [String]
                 , _examples    :: Maybe [String]
                 , _path        :: Maybe FilePath
                 , _synopsis    :: String
                 , _url         :: Maybe String }

-- | Instance of FromJSOM so we can map JSON records to Kommand.
instance FromJSON Kommand where
  parseJSON (Object v) = K                   <$>
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
  show (K{_path=Nothing,_id=i}) = i
  show (K{_path=Just p}) = p

-- | Does this lowercased string match a lowercase Kommand's _id ?
matchKommand :: String -> Kommand -> Bool
matchKommand x (K{_aliases=Nothing,_id=i}) = i == x
matchKommand x (K{_aliases=Just xs,_id=i}) =
  any ((==) (map toLower x) . (map toLower)) (i:xs)

-- | Help constructor
help :: Kommand
help = K { _id          = "help"
         , _aliases     = Nothing
         , _synopsis    = "Built in Help Kommand"
         , _commands    = Nothing
         , _description = Nothing
         , _examples    = Nothing
         , _path        = Nothing
         , _url         = Nothing }

-- | Produce a list of internal Kommands.  These are outside of the
-- JSON spec & can be interposed with the tree of JSON Kommands.
internalKommands :: [Kommand]
internalKommands = [ help ]

-- | Lazily keep the kommands JSON file in sync with what we can find
-- on the internet.
kommands :: IO (Either String [Kommand])
kommands = do
  fname  <- filename
  exists <- doesFileExist fname
  if (not exists)
    then fetchKommands
    else do rightNow <- getCurrentTime
            modTime  <- getModificationTime fname
            when (diffUTCTime rightNow modTime > 60*60)
              fetchKommands
  readKommands fname

-- | Calculate the location of our cached JSON file.
filename :: IO FilePath
filename = do
  home <- getHomeDirectory
  return $ home </> ".kommands.json"

-- | Read the kommands file from disk & parse the JSON
readKommands :: FilePath -> IO (Either String [Kommand])
readKommands fname = DBL.readFile fname >>= return . eitherDecode

fetchKommands :: IO ()
fetchKommands = do
  fname <- filename
  simpleHTTP req >>= getResponseBody >>= DBL.writeFile fname . DBLC.pack
  where
    req = getRequest "http://s3.amazonaws.com/knewton-public-src/kommands.json"

-- | Define a Stack of Kommands and Strings
data Stack = Stack [Kommand] [String] deriving Show

-- | Build a new Stack containing the Kommands (in order), that
-- matched args in the list (in order), plus the remainder of the args
-- that didn't match.
initialStack :: Stack -> Stack
initialStack (Stack ks as) = Stack found (reverse left)
  where (_, (Stack found left)) = foldl reduceArgs (Just ks, (Stack [] [])) as

-- | Take a tuple of maybe a list of Kommands and a String arg.  If
-- it's a match to any of the Kommands in our list then put it on the
-- Kommand stack.  If not put it on the arg stack.
reduceArgs :: (Maybe [Kommand], Stack) -> String -> (Maybe [Kommand], Stack)
reduceArgs (x, Stack ks as) a =
  case (filter (matchKommand a) internalKommands) of
    (k:_) -> (x, Stack (k:ks) as)
    _     -> case x of
      Nothing -> (Nothing, Stack ks (a:as))
      Just y  -> case (filter (matchKommand a) y) of
        (k:_) -> (_commands k, Stack (k:ks) as)
        _     -> (Nothing, Stack ks (a:as))

-- | Take a Stack and build a new Stack by replacing all help Kommands
-- with a single help Kommand at the top of the stack (in case someone
-- types `k -? -h help --help XYZ help`)
helpStack :: Stack -> Stack
helpStack s@(Stack ks as) =
  let noHelp = filter ((/=) "help" . _id) ks in
  if (length noHelp < length ks)
     then (Stack (help:noHelp) as)
     else s

-- | Take a Stack and build a new Stack by popping Kommands onto the
-- args list until we can find a Kommand that has an executable path.
exeStack :: Stack -> Stack
exeStack (Stack [] _) = error "Kommand didn't have an executable path."
exeStack s@(Stack (K{_path=Just _}:_) _) = s
exeStack (Stack (k@(K{_path=Nothing}):ks) as) =
  exeStack (Stack ks ((show k):as))

-- | Run analyzes the stack & routes it to the appropriate function.
run :: [Kommand] -> Stack -> IO ()
run ks   (Stack [] [])                     = showKommands ks        >> putStr "\n"
run _    (Stack [] (a:as))                 = exitWith =<< rawSystem a as
run ks   (Stack ((K{_id="help"}):[]) _)    = showKommands ks        >> putStr "\n"
run _    (Stack ((K{_id="help"}):ks) as)   = showHelp (Stack ks as) >> putStr "\n"
run _  s@(Stack (K{_path=Nothing}:[]) _)   = showHelp s             >> putStr "\n"
run ks s@(Stack (K{_path=Nothing}:_) _)    = run ks (exeStack s)
run _    (Stack (k@K{_path=Just _}:_) as)  = exitWith =<< rawSystem (show k) as

-- | Show the list of Kommands & their synopsis - 1 per line
showKommands :: [Kommand] -> IO ()
showKommands ks = do
  putStrLn "\nCommands:\n"
  mapM_ showKommand ks
  where
    showKommand (K{..}) =
      putStrLn $ "  " ++ _id ++ (replicate (20-length(_id)) ' ') ++ _synopsis

-- | Show help in the context of a Stack
showHelp :: Stack -> IO ()
showHelp s = do
  showDescription s
  showSubKommands s
  showExamples s

-- | Show a description if one exists for the current Stack
showDescription :: Stack -> IO ()
showDescription (Stack (K{_description=Just ls}:_) _) = do
  putStrLn "\nDescription:\n"
  mapM_ (putStrLn . (++) "  ") ls
showDescription _ = return ()

-- | Show the sub Kommands if they exist for the current Stack
showSubKommands :: Stack -> IO ()
showSubKommands (Stack (K{_commands=Just ls}:_) _) = showKommands ls
showSubKommands _ = return ()

-- | Show the examples if they exist for the current Stack
showExamples :: Stack -> IO ()
showExamples (Stack (K{_examples=Just ls}:_) _) = do
  putStrLn "\nExamples:\n"
  mapM_ (putStrLn . (++) "  ") ls
showExamples _ = return ()

-- | Main program entry point.
main :: IO ()
main = do
  result <- kommands
  case result of -- JSON parse error
    Left  er -> hPutStrLn stderr er >> exitWith (ExitFailure 1)
    Right ks -> run ks . helpStack . initialStack . Stack ks =<< getArgs
