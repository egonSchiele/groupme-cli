{-# Language OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}
-- import Control.Concurrent.Async
import Data.Monoid
import qualified Data.Text as TL
import Network.Curl
import GroupMe
import GroupMe.Handshake
import Control.Monad
import Control.Monad.State
import Control.Monad.IO.Class
import Data.Maybe
import Control.Applicative
import GroupMe.Utils
import Control.Concurrent
import Shelly hiding (FilePath)
import System.Environment
import System.Console.CmdArgs
import Data.IORef
import System.IO
import System.Directory
import System.FilePath

data Args = Args { token :: String, user_id :: Int, group_id :: Int, list_groups :: Bool, about_me :: Bool, notification :: String, debug :: Bool } deriving (Show, Data, Typeable)

pargs = Args { token = "" &= help "Your access token (get this from http://dev.groupme.com)",
              user_id = def &= help "Your user id (if you don't know it, run -a)",
              group_id = def &= help "The id for the group you want to join (if you dont know it, run -l)",
              list_groups = False &= help "list all the groups you belong to",
              about_me = False &= help "information about yourself",
              notification = "^$" &= help "If a message includes this, you will get notified",
              debug = False &= help "Print debug messages"
} &= summary "Simple groupme command line client"

-- For this to wor you need to install terminal-notifier:
-- gem install terminal-notifier.
--
-- Note that it only works on OSX 10.8 and up.

notifyAndPrint notifyMe msg = do
    case (pmsgText msg) of
      Nothing -> return ()
      Just text_ -> do
        when (text_ `match` notifyMe) $ do
          shelly . silently $ do
            return ()
            -- run_ "terminal-notifier"  ["-message", TL.pack . show $ text_]
          return ()
    putStrLn . highlightPushMsg notifyMe $ msg
    hFlush stdout
    appDir <- getAppUserDataDirectory "groupme-cli"
    appendFile (joinPath [appDir, "/chatlog"]) ((maybe "" id (pmsgUserName msg)) ++ ": " ++ (maybe "" id (pmsgText msg)) ++ "\n")

printGroups tok = do
    grps <- groups tok
    case grps of
      Left str -> putStrLn $ "Error parsing response from groupme: " ++ str
      Right groups_ -> mapM_ (\grp -> print grp >> putStrLn "") groups_

printMe tok = do
    me_ <- me tok
    case me_ of
      Nothing -> putStrLn "Error parsing response from groupme"
      Just member_ -> print member_

main = do
    _args <- cmdArgs pargs
    let uid = user_id _args
    let gid = group_id _args
    let tok = token _args
    debugMode $= (debug _args)

    case _args of
      Args "" _ _ _ _ _ _ -> fail "You need to pass in an access token with -t"
      Args _ 0 0 False False _ _-> fail "you need to pass in a user id and a group id"
      Args tok 0 0 True _ _ _ -> printGroups tok
      Args tok 0 0 _ True _ _ -> printMe tok
      Args tok uid gid _ _ notifyMe _ -> runProgram tok uid gid notifyMe

runProgram tok uid gid notifyMe = do
    clientId <- handshake uid gid tok
    group_ <- group tok gid

    case group_ of
      Nothing -> fail $ "no group found with id " ++ show gid
      (Just grp) -> putStrLn . green $ "Connecting to group: " ++ groupName grp

    msgs <- preloadMessages tok gid 100
    case msgs of
      [] -> putStrLn "no messages found."
      otherwise -> mapM_ (putStrLn . highlightMsg notifyMe) $ reverse msgs

    -- And now we can read messages!
    runStateT (readMessages clientId (notifyAndPrint notifyMe)) 4 >> return ()
    -- forkIO $ runStateT (readMessages clientId (notifyAndPrint notifyMe)) 4 >> return ()
    -- forever $ do
    --   putStr "> "
    --   input <- getLine
    --   resp <- sendMessage tok gid input
    --   print . respStatus $ resp
    --   hFlush stdout
