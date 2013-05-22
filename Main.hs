{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

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
import           Data.List
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

instance Show Kommand where
  show (K{_path=Nothing,_id=i}) = show i
  show (K{_path=Just p})        = show p

data family   Arg a
data instance Arg Kommand = AK Kommand
data instance Arg String  = AS String

instance Show (Arg Kommand) where show (AK k) = show k
instance Show (Arg String)  where show (AS s) = show s

data Stack = Stack [Arg Kommand] [Arg String]

instance Show Stack where
  show (Stack ks as) = intercalate " " (map show (reverse ks) ++ map show as)

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

-- | Build a new Stack containing the Arg Kommands that matched Strings
-- in the list (in order), plus the remainder of the Arg Strings that
-- didn't match.
initialStack :: [Kommand] -> [String] -> Stack
initialStack ks as = Stack found (reverse left)
  where (_, (Stack found left)) = foldl reduceAS (Just ks, (Stack [] [])) as

-- | Take a tuple of maybe a list of Kommands and a String arg.  If
-- it's a match to any of the Kommands in our list then put it on the
-- Arg Kommand stack.  If not put it on the Arg String stack.
reduceAS :: (Maybe [Kommand], Stack) -> String -> (Maybe [Kommand], Stack)
reduceAS (x, Stack ks as) a =
  case (filter (matchKommand a) internalKommands) of
    (k:_) -> (x, Stack ((AK k):ks) as)
    _     -> case x of
      Nothing -> (Nothing, Stack ks ((AS a):as))
      Just y  -> case (filter (matchKommand a) y) of
        (k:_) -> (_commands k, Stack ((AK k):ks) as)
        _     -> (Nothing, Stack ks ((AS a):as))

-- | Take a Stack and build a new Stack by replacing all help Kommands
-- with a single help Kommand at the top of the stack (in case someone
-- types `k -? -h help --help XYZ help`)
cleanHelpArgs :: Stack -> Stack
cleanHelpArgs s@(Stack ks as) =
  let noHelp = filter (\(AK k) -> "help" /= (_id k)) ks in
  if (length noHelp < length ks)
     then (Stack ((AK help):noHelp) as)
     else s

-- | Take a Stack and build a new Stack by popping Kommands onto the
-- args list until we can find a Kommand that has an executable path.
exeStack :: Stack -> Stack
exeStack (Stack [] _) = error "Kommand didn't have an executable path."
exeStack s@(Stack (AK K{_path=Just _}:_) _) = s
exeStack (Stack (AK K{_path=Nothing,_id=i}:ks) as) =
  exeStack (Stack ks ((AS i):as))

-- | Route analyzes the stack & routes it to the appropriate function.
route :: [Kommand] -> Stack -> IO ()
route ks   (Stack [] [])                       = listKommands ks
route ks   (Stack (AK K{_id="help"}:[]) _)     = listKommands ks
route _    (Stack (AK K{_id="help"}:ks) as)    = showHelp (Stack ks as)
route _  s@(Stack (AK K{_path=Nothing}:[]) _)  = showHelp s
route ks s@(Stack (AK K{_path=Nothing}:_) _)   = route ks (exeStack s)
route _    (Stack [] (a:as))                   = run a as
route _    (Stack (AK k@K{_path=Just _}:_) as) = run k as

-- | Runs the command with specified arguments.  Stack is passed along
-- for debugging if the command didn't work.
run :: forall a b c. Show a => Show b => a -> [b] -> IO c
run a as = do
  code <- rawSystem (show a) (map show as)
  case code of
    ExitSuccess     -> exitWith ExitSuccess
    (ExitFailure c) -> do
      let bar = (replicate 80 '-')
      hPutStrLn stderr $ bar
      hPutStrLn stderr $ intercalate " " (show a:map show as)
      hPutStrLn stderr $ "^^^^^ command exited with code " ++ show c
      hPutStrLn stderr $ bar
      exitWith code

-- | List all the Kommands given
listKommands :: [Kommand] -> IO ()
listKommands ks = showKommands ks >> putStr "\n"

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
showHelp s =
  showDescription s >> showSubKommands s >> showExamples s >> putStr "\n"

-- | Show a description if one exists for the current Stack
showDescription :: Stack -> IO ()
showDescription (Stack (AK K{_description=Just ls}:_) _) = do
  putStrLn "\nDescription:\n"
  mapM_ (putStrLn . (++) "  ") ls
showDescription _ = return ()

-- | Show the examples if they exist for the current Stack
showExamples :: Stack -> IO ()
showExamples (Stack (AK K{_examples=Just ls}:_) _) = do
  putStrLn "\nExamples:\n"
  mapM_ (putStrLn . (++) "  ") ls
showExamples _ = return ()

-- | Show the sub Kommands if they exist for the current Stack
showSubKommands :: Stack -> IO ()
showSubKommands (Stack (AK K{_commands=Just ls}:_) _) = showKommands ls
showSubKommands _ = return ()

-- | Main program entry point.
main :: IO ()
main = do
  result <- kommands
  case result of -- JSON parse error
    Left  er -> hPutStrLn stderr er >> exitWith (ExitFailure 1)
    Right ks -> route ks . cleanHelpArgs . initialStack ks =<< getArgs
