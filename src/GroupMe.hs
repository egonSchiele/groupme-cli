{-# Language OverloadedStrings, ScopedTypeVariables #-}
module GroupMe where
import Control.Concurrent  
import Language.Haskell.HsColour.ANSI  
import qualified Network.HTTP as HTTP
import Network.Curl
import Data.Aeson
import Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.ByteString.Char8 as BS
import Data.Text.Encoding
import Data.Text.Encoding.Error
import qualified Debug.Trace as T
import Control.Monad
import GroupMe.Utils
import GroupMe.Handshake
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.UUID.V4
import qualified Data.Map as M

data Group = Group { createdAt :: Int,
                     creatorUserId :: Int,
                     description :: Maybe String,
                     groupId :: Int,
                     groupImageUrl :: Maybe String,
                     members :: [Member],
                     groupMessages :: MessageInfo,
                     groupName :: String,
                     officeMode :: Bool,
                     phoneNumber :: String,
                     shareUrl :: Maybe String,
                     grouptype :: String,
                     updatedAt :: Int
}

instance FromJSON Group where
    parseJSON (Object v) = Group <$> (v .: "created_at")
                                 <*> (read <$> (v .: "creator_user_id"))
                                 <*> (v .: "description")
                                 <*> (read <$> (v .: "group_id"))
                                 <*> (v .: "image_url")
                                 <*> (v .: "members")
                                 <*> (v .: "messages")
                                 <*> (v .: "name")
                                 <*> (v .: "office_mode")
                                 <*> (v .: "phone_number")
                                 <*> (v .: "share_url")
                                 <*> (v .: "type")
                                 <*> (v .: "updated_at")

label :: String -> String -> String
label k v = blue (k ++ ": ") ++ v

(!>) k v = label k v

instance Show Group where
    show (Group _ creator_id_ desc_ group_id_ _ members_ _ name_ _ _ _ type_ _) = intercalate "\n" [ green name_,
                                                                                                     "Creator" !> show creator_id_,
                                                                                                     "Description" !> maybe "" id desc_,
                                                                                                     "Group Id" !> show group_id_,
                                                                                                     "Members" !> (intercalate ", " . map show $ members_),
                                                                                                     "Type" !> type_
                                                                                                   ]


data Member = Member { autokicked :: Bool,
                       memberImageUrl :: Maybe String,
                       muted :: Bool,
                       nickname :: String,
                       userId :: Int
}

instance Show Member where
    show (Member _ _ _ nick_ id_) = nick_ ++ " (#" ++ show id_ ++ ")"

instance FromJSON Member where
    parseJSON (Object v) = Member <$> (v.: "autokicked")
                                  <*> (v.: "image_url")
                                  <*> (v.: "muted")
                                  <*> (v.: "nickname")
                                  <*> (liftM read (v.: "user_id"))

data MessageInfo = MessageInfo { count :: Int,
                                 lastMessageCreatedAt :: Int,
                                 lastMessageId :: Int,
                                 preview :: MessagePreview
} deriving (Show)

instance FromJSON MessageInfo where
    parseJSON (Object v) = MessageInfo <$> (v.: "count")
                                       <*> (v.: "last_message_created_at")
                                       <*> (liftM read (v.: "last_message_id"))
                                       <*> (v.: "preview")

data MessagePreview = MessagePreview { msgImageUrl :: Maybe String,
                                       senderNick :: String,
                                       msgPreviewText :: String
} deriving (Show)

instance FromJSON MessagePreview where
    parseJSON (Object v) = MessagePreview <$> (v.: "image_url")
                                   <*> (v.: "nickname")
                                   <*> (v.: "text")

data Response = Response { responseGroup :: [Group] } deriving (Show)

instance FromJSON Response where
    parseJSON (Object v) = Response <$> (v .: "response")

data PushMessage = PushMessage { pmsgChannel :: String,
                                 pmsgType :: String,
                                 pmsgUserName :: Maybe String,
                                 pmsgText :: Maybe String,
                                 pmsgPicUrl :: Maybe String
}

instance Show PushMessage where
    show (PushMessage _ _ name_ text_ pic) = if (isJust name_)
                                           then (green ((fromJust name_) ++ ": ")) ++ (fromJust text_) ++ picUrl
                                           else ""
      where picUrl = case pic of
                       Nothing -> ""
                       (Just url) -> blue $ " <" ++ url ++ ">"

go str v = v .: str

instance FromJSON PushMessage where
    parseJSON (Object v) = PushMessage <$> v .: "channel"
                                   <*> (v .: "data" >>= go "type")
                                   <*> (v .: "data" >>= go "subject" >>= go "name")
                                   <*> (v .: "data" >>= go "subject" >>= go "text")
                                   <*> (v .: "data" >>= go "subject" >>= go "picture_url")

data Attachment = Attachment { attType :: String, attUrl :: Maybe String }

instance Show Attachment where
    show (Attachment type_ url_) = case url_ of
                                     Nothing -> ""
                                     (Just url) -> blue $ "<" ++ url ++ ">"

data Message = Message { msgId :: Int,
                         msgCreatedAt :: Int,
                         msgUserId :: Int,
                         msgGroupId :: Int,
                         msgUserName :: String,
                         msgText :: String,
                         favoritedBy :: [Int],
                         attachments :: [Attachment]
}

instance Show Message where
    show (Message _ _ _ _ name_ text_ favs atts) = (green (name_ ++ ": ")) ++ (dim text_) ++ attachmentsText ++ likesText
      where likesText = case favs of
                      [] -> ""
                      (x:[]) -> " [" ++ (red "1 like") ++ "]"
                      otherwise -> " [" ++ (red ((show . length $ favs) ++ " likes")) ++ "]"
            attachmentsText = case atts of
                                [] -> ""
                                otherwise -> " " ++ (intercalate "," $ map show atts)

instance FromJSON Message where
    parseJSON (Object v) = Message <$> (read <$> v .: "id")
                                   <*> v .: "created_at"
                                   <*> (read <$> v .: "user_id")
                                   <*> (read <$> v .: "group_id")
                                   <*> v .: "name"
                                   <*> v .: "text"
                                   <*> (map read <$> v .: "favorited_by")
                                   <*> (v .: "attachments")

highlightMsg str msg = if (msgUserName msg `match` str || msgText msg `match` str)
                         then highlight [Background Red] . show $ msg
                         else show msg

highlightPushMsg str msg@(PushMessage _ _ Nothing _ _) = show msg
highlightPushMsg str msg@(PushMessage _ _ (Just username_) (Just text_) _) = if (username_ `match` str || text_ `match` str)
                                                                 then highlight [Background Red] . show $ msg
                                                                 else show msg

instance FromJSON Attachment where
    parseJSON (Object v) = Attachment <$> v .: "type" <*> v .: "url"

readMessages :: String -> (PushMessage -> IO ()) -> StateT Int IO String
readMessages clientid func = do
    forever $ do
      num <- get
      put (num + 1)
      let connect = Connect "/meta/connect" clientid "long-polling" num
      body <- liftIO . sendJSON . LBS.unpack . encode $ [connect]
      let _result = decode . LBS.pack . respBody $ body :: Maybe [Value]
      ifJust _result $ \result -> do
        when (length result > 1) $ do
          let _msg = unwrapResult . fromJSON $ result !! 1 :: Maybe PushMessage
          case _msg of
            Nothing -> return ()
            (Just msg) -> liftIO $ do
              forkIO $ func msg
              return ()
    return "done!"

extractFromResponse :: FromJSON a => String -> (Value -> Value) -> IO (Maybe a)
extractFromResponse body func = do
    let response = decode . LBS.pack $ body :: Maybe Value
    case response of
      Nothing -> return Nothing
      (Just res) -> return . unwrapResult . fromJSON . func $ res

base :: String
base = "http://api.groupme.com/v3"

api path token = apiParams path token []

apiParams :: String -> String -> [(String, Maybe String)] -> IO String
apiParams path token params = do
    let pairs = (mapMaybe concatParams $ [("token", Just token)] ++ params)
    rsp <- HTTP.simpleHTTP (HTTP.getRequest $ base ++ path ++ "?" ++ (urlEncodePairs pairs))
    HTTP.getResponseBody rsp

urlEncodePairs :: [(String, String)] -> String
urlEncodePairs = intercalate "&" . map urlEncodePair

urlEncodePair :: (String, String) -> String
urlEncodePair (x, y) = HTTP.urlEncode x ++ '=' : HTTP.urlEncode y

concatParams (key, (Just val)) = Just (key, val)
concatParams (key, Nothing) = Nothing

groups :: String -> IO (Either String [Group])
groups token = do
    body <- api "/groups" token
    -- body <- readFile "data.json"
    let body_ = encodeUtf8 $ decodeUtf8With lenientDecode (BS.pack body)
    return $ (responseGroup <$> (eitherDecode . LBS.pack . BS.unpack $ body_ :: Either String Response))

messages :: String -> Int -> Maybe Int -> Maybe Int -> IO (Maybe [Message])
messages token group_id before_id since_id = do
    body <- apiParams ("/groups/" ++ (show group_id) ++ "/messages") token [("before_id", show <$> before_id), ("since_id", show <$> since_id)]
    extractFromResponse body $ \res -> res ! "response" ! "messages"

preloadMessages :: String -> Int -> Int -> IO [Message]
preloadMessages token group_id count = _preloadMessages token group_id count [] Nothing

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast xs = Just $ last xs

_preloadMessages :: String -> Int -> Int -> [Message] -> Maybe Int -> IO [Message]
_preloadMessages token group_id count acc before_id = do
    msgs <- messages token group_id before_id Nothing
    case msgs of
      Nothing -> return acc
      (Just msgs_) -> if (null msgs_)
                        then return acc
                        else do
                          let new_acc = acc ++ msgs_
                          if (length new_acc < count)
                            then do
                              let lastMsgId = msgId <$> maybeLast new_acc
                              _preloadMessages token group_id count new_acc lastMsgId
                            else return $ new_acc

group :: String -> Int -> IO (Maybe Group)
group token group_id = do
    body <- api ("/groups/" ++ show group_id) token
    extractFromResponse body $ \res -> res ! "response"

me :: String -> IO (Maybe Member)
me token = do
    body <- api "/users/me" token
    putStrLn body
    extractFromResponse body $ \res -> res ! "response"

data SendMessage = SendMessage { sourceGUID :: String, sendMsgText :: String } deriving (Show)

instance ToJSON SendMessage where
    toJSON (SendMessage guid_ txt_) = object ["source_guid" .= guid_, "text" .= txt_, "attachments" .= ([] :: [String])]

sendMessage token group_id text = do
    guid <- nextRandom
    let msg = SendMessage (show guid) text
    let json = LBS.unpack . encode $ M.fromList [("message" :: String, msg)]
    sendJSONToUrl (base ++ "/groups/" ++ show group_id ++ "/messages") (Just token) json
