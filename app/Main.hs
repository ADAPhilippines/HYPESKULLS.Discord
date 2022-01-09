module Main where

import           Calamity hiding (title)
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext, useFullContext)
import           Calamity.Metrics.Noop
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Map (Map, fromList, (!), toList)
import           Data.Generics.Labels       ()
import           Data.Maybe
import           Data.Text                  (Text, pack, toUpper, splitOn, toLower)
import qualified Data.Text                  as T
import qualified Di
import           DiPolysemy
import qualified Polysemy                   as P
import qualified Calamity.Types.Model.Presence.Activity
import Calamity.Gateway.Types (StatusUpdateData(StatusUpdateData), since, game, afk, SentDiscordMessage (StatusUpdate))
import Data.Colour
import Calamity.Internal.IntColour
import Data.Word (Word64)
import Network.HTTP.Req (req, https, GET (GET), (/:), jsonResponse, responseBody, runReq, defaultHttpConfig, NoReqBody (NoReqBody), (=:), responseStatusCode, JsonResponse)
import Data.Aeson
import Data.Aeson.TH
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import TextShow (TextShow(showt), fromString)
import System.Environment (getEnv)
import Data.ByteString.Lazy.UTF8 as BLU hiding (length, decode)
import Calamity (Snowflake(fromSnowflake))
import Data.Flags (BoundedFlags(allFlags))
import Control.Exception (try, Exception, SomeException (SomeException))
import Data.Semigroup (Any(Any))
import Control.Concurrent ( threadDelay )

newtype WalletAddress = WalletAddress {
  unWalletAddress :: Text
} deriving (Show, Eq, Generic)

newtype HypeSkullPropertyOccurence = HypeSkullPropertyOccurence {
  occurrence :: Double
} deriving (Show, Generic, Eq)

newtype HypeSkullRarityScore = HypeSkullRarityScore {
  score :: Double
} deriving (Show, Generic, Eq)

newtype HypeSkullRank = HypeSkullRank {
  rank :: Int
} deriving (Show, Generic, Eq)

data HypeSkull = HypeSkull {
  title :: Text,
  id :: Text,
  imageCID :: Text,
  skull :: Text,
  background :: Text,
  smoke :: Text,
  frame :: Text,
  eyes :: Text,
  specials :: Text,
  glitch :: Bool,
  dateCreated :: Text,
  logo :: Text,
  tokenRarityScore :: Maybe HypeSkullRarityScore
} deriving (Generic, Show, Eq)

data Identity = Identity {
  assetName :: Text,
  avatar :: Text,
  expiresIn :: Int,
  key :: Text,
  policyId :: Text,
  username :: Text
} deriving (Generic, Show, Eq)

deriveJSON defaultOptions 'HypeSkullPropertyOccurence
deriveJSON defaultOptions 'HypeSkullRarityScore
deriveJSON defaultOptions 'HypeSkullRank
deriveJSON defaultOptions 'HypeSkull
deriveJSON defaultOptions 'Main.Identity

hypeRoles :: Map Text Word64
hypeRoles = fromList [
    ("Holy Grail", 881636191672352828),
    ("Ergo Eyes", 881664269354168360),
    ("3rd Eye", 881713830546247761),
    ("Glitch", 881636119870058526),
    ("Onyx", 885600733150453821),
    ("KoK", 887366025174196284),
    ("Diamond", 887686335622701116),
    ("Black Diamond", 929689208715612190),
    ("Gold", 929689587259932713),
    ("Pearl", 929689751978639360),
    ("Platinum", 929690081277661194),
    ("Rose Gold", 929690274475692032)
  ]

main :: IO ()
main = do
  token <- T.pack <$> getEnv "BOT_TOKEN"
  Di.new $ \di ->
    void
    . P.runFinal
    . P.embedToFinal @IO
    . runDiToIO di
    . runCacheInMemory
    . runMetricsNoop
    . useFullContext
    . useConstantPrefix "*"
    . runBotIO (BotToken token) allFlags
    $ do
      info @Text "Bot starting up!"
      react @'MessageCreateEvt $ \(msg,_,_) -> 
        when True $
        void . invoke $ CreateReaction msg msg (UnicodeEmoji "😄")
      react @ 'ReadyEvt \_ -> 
        sendPresence $ StatusUpdateData Nothing (Just (Calamity.Types.Model.Presence.Activity.activity "H.Y.P.E." Game)) Online False
      addCommands $ do
        helpCommand
        command @'[Int, Maybe GuildChannel] "slowmode" $ \ctx seconds mchan -> do
          let cid = maybe (ctx ^. #channel . to getID) getID mchan :: Snowflake Channel
          void . invoke $ ModifyChannel cid $ def
            & #rateLimitPerUser ?~ seconds
          void . invoke $
            CreateReaction (ctx ^. #channel) (ctx ^. #message) (UnicodeEmoji "✅")

        command @'[Int] "hypeskull" $ \ctx skullID -> do
          if skullID <= 0 || skullID > 1500
            then void $ reply @Text (ctx ^. #message) "Invalid SKULL ID... ☠️"
            else do
              partialSkull <- P.embed $ querySkull skullID
              skull <- P.embed $ queryFullSkull $ Main.id partialSkull
              case Main.tokenRarityScore skull of
                Nothing           -> void $ P.embed $ putStrLn "H.Y.P.E. SKULL score not found"
                Just skullRarity  -> do
                  skullRank <- P.embed $ querySkullRank $ Main.score skullRarity
                  skullPropertyOccurences <- P.embed $ querySkullPropertyOccurences skull
                  let fields          = skullFields skull skullPropertyOccurences
                      embededSkull    = embedSkull skullID skull
                  void $ tell @Embed ctx $ embededSkull
                    & #fields .~ (EmbedField "RARITY" ("```" <> rarityName (Main.score skullRarity) <> " #" <> T.pack (show $ Main.rank skullRank) <> "```") True:fields)
                    & #color ?~ rarityColor (Main.score skullRarity)

        command @'[] "idt" $ \ctx -> do
          eitherDMC <- invoke $ CreateDM $ ctx ^. #user
          case eitherDMC of
            Left _ -> info @Text "DM Channel not found"
            Right dmChannel -> do
              addr <- P.embed requestAuthAddress
              void $ tell @Embed dmChannel $ embedAuthAddr addr
              identity <- P.embed $ getIdentity addr 0
              case identity of
                Nothing -> void $ tell @Text dmChannel "You have failed to authenticate at the given time. Please try again."
                Just identity' -> do
                  let user = ctx ^. #user
                  void $ tell @Text ctx $ mention user 
                    <> " has successfully linked his IdentityToken.\n" 
                    <> "https://cardanoscan.io/token/" 
                    <> policyId identity' 
                    <> "." 
                    <> assetName identity'

        command @'[] "roles" $ \ctx -> do
          let guild = ctx ^. #guild
          case guild of
            Nothing -> info @Text "Not in a guild"
            Just g -> do
              roles <- invoke $ GetGuildRoles g
              case roles of
                Left _ -> info @Text "Roles not found"
                Right r -> do
                  let roleNames = map (\r -> r ^. #name) r
                      roleIds = map (\r -> r ^. #id) r
                  void $ reply @Text (ctx ^. #message) $ "**Server Roles**: \n```" <> T.intercalate "\n" (zipWith (\r n -> T.pack (show r) <> ": " <> n) roleIds roleNames) <> "```"

        command @'[Maybe (Snowflake User), Maybe (Snowflake Role)] "addrole" $ \ctx user role -> do
          void $ info @Text $ "User: " <> showt user
          void $ info @Text $ "Role: " <> showt role
          admins <- P.embed adminIds
          case admins of
            Just as ->  do
              let callingUser = fromSnowflake $ ctx ^. #message . #author
              if callingUser `elem` as
                then do
                  let guild = ctx ^. #guild
                  case guild of
                    Nothing -> info @Text "Not in a guild"
                    Just g -> do
                      case user of
                        Nothing -> info @Text "No user specified"
                        Just u -> do
                          case role of
                            Nothing -> info @Text "No role specified"
                            Just r -> do
                              void $ invoke $ AddGuildMemberRole g u r
                              void $ reply @Text (ctx ^. #message) "Role added"
                else
                  void $ reply @Text (ctx ^. #message) "You are not an admin"
            Nothing -> info @Text "No Admin Id has been set" -- Can't be possible?

        command @'[Maybe (Snowflake User), Maybe (Snowflake Role)] "removerole" $ \ctx user role -> do
          void $ info @Text $ "User: " <> showt user
          void $ info @Text $ "Role: " <> showt role
          admins <- P.embed adminIds
          case admins of
            Just as ->  do
              let callingUser = fromSnowflake $ ctx ^. #message . #author
              if callingUser `elem` as
                then do
                  let guild = ctx ^. #guild
                  case guild of
                    Nothing -> info @Text "Not in a guild"
                    Just g -> do
                      case user of
                        Nothing -> info @Text "No user specified"
                        Just u -> do
                          case role of
                            Nothing -> info @Text "No role specified"
                            Just r -> do
                              void $ invoke $ RemoveGuildMemberRole g u r
                              void $ reply @Text (ctx ^. #message) "Role removed"
                else
                  void $ reply @Text (ctx ^. #message) "You are not an admin"
            Nothing -> info @Text "No Admin Id has been set" -- Can't be possible?

        command @'[] "purgeroles" $ \ctx -> do
          admins <- P.embed adminIds
          case admins of
            Just as ->  do
              let callingUser = fromSnowflake $ ctx ^. #message . #author
              if callingUser `elem` as
                then do
                  let guild = ctx ^. #guild
                  case guild of
                    Nothing -> info @Text "Not in a guild"
                    Just g -> do
                      members <- getMembers [] g
                      let memberIds = map (\m -> m ^. #id) members
                      info @Text $ "Purging roles from " <> showt (length memberIds) <> " members"
                      mapM_ (\m -> 
                            mapM_ (\(k, v) -> do
                            info @Text $ "Removing role " <> showt k <> " from " <> showt m
                            void $ invoke $ RemoveGuildMemberRole g m (Snowflake v :: Snowflake Role)
                          ) (toList hypeRoles :: [(Text, Word64)])
                        ) memberIds
                      void $ reply @Text (ctx ^. #message) "Roles purged"
                else
                  void $ reply @Text (ctx ^. #message) "You are not an admin"
            Nothing -> info @Text "No Admin Id has been set" -- Can't be possible?
            where
              getMembers initialMembers g = do
                members <- getMembersInternal initialMembers g
                case members of
                  Left _ -> return initialMembers
                  Right m -> 
                    if length members < 1000 then return (initialMembers <> m) else do 
                      getMembers (initialMembers <> m) g

              getLastMemberSnowflake :: [Member] -> Snowflake User
              getLastMemberSnowflake members = getID $ last members

              getMembersInternal [] g = do
                invoke $ ListGuildMembers g $ ListMembersOptions (Just 1000) Nothing

              getMembersInternal initialMembers g = do
                invoke $ ListGuildMembers g $ ListMembersOptions (Just 1000) (Just $ getLastMemberSnowflake initialMembers)



embedAuthAddr :: Text -> Embed
embedAuthAddr addr = def
  & #title ?~ "Authenticate with your Identity Token"
  & #description ?~ "Please send **1.2 $ADA** within **5 minutes** to the wallet address provided below. Your **1 $ADA** will be returned once authentication is complete."
  & #fields .~ [
    EmbedField "AUTH ADDRESS" ("```" <> addr <> "```") True
  ]
  & #image ?~ embedImage ("https://api.identity.adaph.io/qr/generate?data=" <> addr)

requestAuthAddress :: IO Text
requestAuthAddress = runReq defaultHttpConfig $ do
  r <- req GET ( https "api.identity.adaph.io" /: "identity" /: "auth" ) NoReqBody jsonResponse mempty
  let addr = responseBody r :: Text
  return addr

queryIdentity :: Text -> IO Main.Identity
queryIdentity addr = runReq defaultHttpConfig $ do
  r <- req GET ( https "api.identity.adaph.io" /: "identity" /: "token" /: addr ) NoReqBody jsonResponse mempty
  let identity = responseBody r :: Main.Identity
  return identity

getIdentity :: Text -> Int -> IO (Maybe Main.Identity)
getIdentity addr retries = do
  result <- try (queryIdentity addr) :: IO (Either SomeException Main.Identity)
  case result of
    Right identity  -> do
      return $ Just identity
    Left _          ->  
      if retries >= maxRetries
      then return Nothing
      else do
        threadDelay $ retryInterval * 1000 * 1000
        getIdentity addr $ retries + 1
    where
      retryInterval = 20
      maxRetries    = 15

embedSkull :: Int -> HypeSkull -> Embed
embedSkull skullID skull = def
  & #title ?~ skullIdToText skullID
  & #description ?~ "```" <> Main.title skull <> "```"
  & #image ?~ embedImage ("https://ipfs.infura.io/ipfs/" <> Main.imageCID skull)

skullIdToText :: Int -> Text
skullIdToText skullId = T.pack $ "HYPESKULL" ++ padLeft 4 '0' (show skullId)

querySkull :: Int -> IO HypeSkull
querySkull skullID = runReq defaultHttpConfig $ do
  let params = "query" =: ("hypeskull" ++ padLeft 4 '0' (show skullID) :: String)
  r <- req GET ( https "hsapi-cka5aaecrq-uw.a.run.app" /: "api" /: "v1.0" /: "skulls" ) NoReqBody jsonResponse params
  let skull = responseBody r :: [HypeSkull]
  return $ head skull

queryFullSkull :: Text -> IO HypeSkull
queryFullSkull skullGUID = runReq defaultHttpConfig $ do
  r <- req GET ( https "hsapi-cka5aaecrq-uw.a.run.app" /: "api" /: "v1.0" /: "skulls" /: skullGUID ) NoReqBody jsonResponse mempty
  let skull = responseBody r :: HypeSkull
  return skull

querySkullRank :: Double -> IO HypeSkullRank
querySkullRank score = runReq defaultHttpConfig $ do
  let params = "score" =: (show score :: String)
  r <- req GET ( https "hsapi-cka5aaecrq-uw.a.run.app" /: "api" /: "v1.0" /: "skulls" /: "properties" /: "rank" ) NoReqBody jsonResponse params
  let skullRank = responseBody r :: HypeSkullRank
  return skullRank

querySkullPropertyOccurence :: Text -> Text -> IO HypeSkullPropertyOccurence
querySkullPropertyOccurence key value = runReq defaultHttpConfig $ do
  let params =
        "key"   =:  key    <>
        "value" =:  value
  r <- req GET ( https "hsapi-cka5aaecrq-uw.a.run.app" /: "api" /: "v1.0" /: "skulls" /: "properties" ) NoReqBody jsonResponse params
  let skullPropertyOccurence = responseBody r :: HypeSkullPropertyOccurence
  return skullPropertyOccurence


querySkullPropertyOccurences :: HypeSkull -> IO (Map Text HypeSkullPropertyOccurence)
querySkullPropertyOccurences skull = do
  let properties = [
          ("skull", Main.skull skull),
          ("background", Main.background skull),
          ("smoke", Main.smoke skull),
          ("frame", Main.frame skull),
          ("eyes", Main.eyes skull),
          ("specials", Main.specials skull),
          ("glitch", toLower (T.pack(show (Main.glitch skull)))),
          ("logo", Main.logo skull)
        ]
  result <- mapM (\(key, value) -> do
      occurence <- querySkullPropertyOccurence key value
      return (key, occurence)
    ) properties
  return $ fromList result

skullFields :: HypeSkull -> Map Text HypeSkullPropertyOccurence -> [EmbedField]
skullFields skull occurences = [
    EmbedField "OWNER" "```UNKNOWN```" True,
    EmbedField "MINT DATE" ("```" <> formatField (head (splitOn "T" (Main.dateCreated skull))) <> "```") True,
    EmbedField "SKULL" ("```" <> formatField (Main.skull skull) <> " " <> getOccurence "skull" <> "%```") True,
    EmbedField "BACKGROUND" ("```" <> formatField (Main.background skull) <> " " <> getOccurence "background" <> "%```") True,
    EmbedField "SMOKE" ("```" <> formatField (Main.smoke skull) <> " " <> getOccurence "smoke" <> "%```") True,
    EmbedField "FRAME" ("```" <> formatField (Main.frame skull) <> " " <> getOccurence "frame" <> "%```") True,
    EmbedField "EYES" ("```" <> formatField (Main.eyes skull) <> " " <> getOccurence "eyes" <> "%```") True,
    EmbedField "SPECIALS" ("```" <> formatField (Main.specials skull) <> " " <> getOccurence "specials" <> "%```") True,
    EmbedField "GLITCH" ("```" <> formatField (glitchString (Main.glitch skull)) <> " " <> getOccurence "glitch" <>"%```") True,
    EmbedField "LOGO" ("```" <> formatField (Main.logo skull) <> " " <> getOccurence "logo" <>"%```") True,
    EmbedField "FULL PREVIEW" ("https://www.seehype.com/explore/" <> Main.id skull) False
 ]
 where
    glitchString :: Bool -> Text
    glitchString b = if b then "ACTIVATED" else "NONE"

    formatField :: Text -> Text
    formatField = toUpper . T.pack . padRight 30 ' ' . T.unpack

    getOccurence :: Text -> Text
    getOccurence key = showt $ truncate' (Main.occurrence (occurences ! key)) 2

adminIds :: IO (Maybe [Word64])
adminIds = do
  adminIds <- getEnv "ADMIN_IDS"
  let adminIdList = decode (BLU.fromString adminIds) :: Maybe [Word64]
  return adminIdList


rarityName :: Double -> Text
rarityName score
  | score > 900 = "HOLY GRAIL"
  | score > 550 = "LEGENDARY"
  | score > 200 = "SUPER RARE"
  | score > 100 = "VERY RARE"
  | score > 1 = "RARE"
  | otherwise = "UNKNOWN"

rarityColor :: Double -> Colour Double
rarityColor score
  | score > 900 = colour 0xb1bb1a
  | score > 550 = colour 0x460086
  | score > 200 = colour 0xa20f0f
  | score > 100 = colour 0x412fb5
  | score > 1   = colour 0x8A78FF
  | otherwise   = colour 0x8A78FF
  where
    colour :: Word64 -> Colour Double
    colour hex = extractColour $ colourFromWord64 hex

    extractColour :: IntColour -> Colour Double
    extractColour (IntColour c) = c

padLeft :: Int -> Char -> String -> String
padLeft n c s = replicate (n - length s) c ++ s

padRight :: Int -> Char -> String -> String
padRight n c s = s ++ replicate (n - length s) c

truncate' :: Double -> Int -> Double
truncate' x n = fromIntegral (floor (x * t)) / t
    where t = 10^n