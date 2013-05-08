{-# Language OverloadedStrings, ScopedTypeVariables #-}

module GroupMe.Handshake where
import GroupMe.Utils
import Data.Aeson
import Network.HTTP.Wget
import Network.Curl
import Data.Time.Clock.POSIX
import Control.Applicative
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T



handshakeInit = "[ { \"channel\":\"/meta/handshake\", \"version\":\"1.0\", \"supportedConnectionTypes\":[\"long-polling\"], \"id\":\"1\" } ]"

data Subscribe = Subscribe { channel :: String,
                             subscriptionClientId :: String,
                             subscription :: String,
                             subscriptionId :: Int,
                             ext :: Ext
} deriving (Show)

data Ext = Ext { accessToken :: String,
                 timestamp :: Int
} deriving (Show)

instance ToJSON Subscribe where
    toJSON (Subscribe channel_ clientid_ sub_ id_ ext_) = object ["channel" .= channel_, "clientId" .= clientid_, "subscription" .= sub_, "id" .= (show id_), "ext" .= ext_]

instance ToJSON Ext where
    toJSON (Ext accessToken_ timestamp_) = object ["access_token" .= accessToken_, "timestamp" .= timestamp_]

data Connect = Connect { connectionChannel :: String,
                         connectionClientId :: String,
                         connectionType :: String,
                         connectionId :: Int
} deriving (Show)

instance ToJSON Connect where
    toJSON (Connect channel_ clientid_ type_ id_) = object ["channel" .= channel_, "clientId" .= clientid_, "connectionType" .= type_, "id" .= (show id_)]


-- Handshake as explained here: http://dev.groupme.com/tutorials/push
handshake :: (Show a, Num a) => a -> a -> String -> IO String
handshake userId groupId token = do
    -- Step #1
    body <- sendJSON handshakeInit
    let res1_ = head . fromJust . decode . LBS.pack . respBody $ body :: Value
    let clientId = T.unpack . unwrapString $ res1_ ! "clientId"

    now <- getPOSIXTime
    -- Step #2 (part 1)
    let subscribeUser = Subscribe "/meta/subscribe" clientId ("/user/" ++ (show userId)) 2 (Ext token (round now))
    body3 <- sendJSON . LBS.unpack . encode $ [subscribeUser]

    -- Step #2 (part 2)
    let subscribeGroup = Subscribe "/meta/subscribe" clientId ("/group/" ++ (show groupId)) 3 (Ext token (round now))
    body4 <- sendJSON . LBS.unpack . encode $ [subscribeGroup]
    return clientId
