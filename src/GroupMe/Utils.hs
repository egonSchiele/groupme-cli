{-# Language OverloadedStrings, ScopedTypeVariables #-}

module GroupMe.Utils where
import Data.Char
import Data.Aeson
import qualified Data.HashMap.Strict as M
import Network.Curl
import Language.Haskell.HsColour.ANSI
import Control.Monad
import Data.Maybe

unwrapResult (Success a) = Just a
unwrapResult _ = Nothing

ifJust (Just x) f = f x
ifJust Nothing _ = return ()

unwrapString (String s) = s

-- functions for working with generic Aeson types
(!) (Object v) key = v M.! key

sendJSON :: String -> IO CurlResponse
sendJSON data_ = sendJSONToUrl "https://push.groupme.com/faye" Nothing data_

sendJSONToUrl :: String -> Maybe String -> String -> IO CurlResponse
sendJSONToUrl url token data_ = do
    curl <- initialize
    setopt curl (CurlURL url)
    setopt curl (CurlPost True)
    setopt curl (CurlPostFields [data_])
    
    if (isJust token)
      then setopt curl (CurlHttpHeaders ["Content-Type: application/json", "X-Access-Token: " ++ fromJust token])
      else setopt curl (CurlHttpHeaders ["Content-Type: application/json"])
      
    response <- perform_with_response curl
    reset curl
    return response


red = highlight [Foreground Red]
green = highlight [Foreground Green]
yellow = highlight [Foreground Yellow]
blue = highlight [Foreground Blue]
cyan = highlight [Foreground Cyan]
dim = highlight [Dim]

-- case insensitive match
match :: String -> String -> Bool
match str1 str2 = (map toLower str2) `elem` (map toLower str1)
