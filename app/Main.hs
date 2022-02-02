module Main where

import Calamity (Snowflake (fromSnowflake))
import Calamity hiding (title)
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Commands.Context (FullContext, useFullContext)
import Calamity.Gateway.Types (RequestGuildMembersData (guildID), SentDiscordMessage (StatusUpdate), StatusUpdateData (StatusUpdateData), afk, game, since)
import Calamity.Internal.IntColour
import Calamity.Metrics.Noop
import qualified Calamity.Types.Model.Presence.Activity
import Control.Concurrent (threadDelay)
import Control.Exception (Exception, SomeException (SomeException), try)
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString.Lazy.UTF8 as BLU hiding (decode, length)
import Data.Colour
import Data.Default
import Data.Flags (BoundedFlags (allFlags))
import Data.Generics.Labels ()
import Data.Hex
import Data.Map (Map, fromList, lookup, toList, (!), elems)
import Data.Maybe
import Data.Semigroup (Any (Any))
import Data.Text (Text, isInfixOf, pack, splitOn, toLower, toUpper)
import qualified Data.Text as T
import Data.Text.Encoding
import Data.Time.Clock.POSIX
import Data.Word (Word64)
import qualified Di
import DiPolysemy
import GHC.Generics (Generic)
import Network.HTTP.Req (GET (GET), JsonResponse, NoReqBody (NoReqBody), POST (POST), QueryParam, ReqBodyJson (ReqBodyJson), defaultHttpConfig, header, https, ignoreResponse, jsonResponse, req, responseBody, responseStatusCode, runReq, (/:), (=:))
import qualified Polysemy as P
import System.Directory
import System.Environment (getEnv)
import TextShow (TextShow (showt), fromString)
import qualified Data.Generics.Product as Data.Generics.Product.Fields
import Data.Vector.Unboxing (Vector, toList)

newtype WalletAddress = WalletAddress
  { unWalletAddress :: Text
  }
  deriving (Show, Eq, Generic)

newtype HypeSkullPropertyOccurence = HypeSkullPropertyOccurence
  { occurrence :: Double
  }
  deriving (Show, Generic, Eq)

newtype HypeSkullRarityScore = HypeSkullRarityScore
  { score :: Double
  }
  deriving (Show, Generic, Eq)

newtype HypeSkullRank = HypeSkullRank
  { rank :: Int
  }
  deriving (Show, Generic, Eq)

data HypeSkull = HypeSkull
  { title :: Text,
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
  }
  deriving (Generic, Show, Eq)

data Identity = Identity
  { assetName :: Text,
    avatar :: Text,
    expiresIn :: Int,
    key :: Text,
    policyId :: Text,
    username :: Text
  }
  deriving (Generic, Show, Eq)

data DiscordIdt = DiscordIdt
  { discordId :: Int,
    identityToken :: Text
  }
  deriving (Generic, Show, Eq)

newtype AssetAddress = AssetAddress
  { address :: Text
  }
  deriving (Generic, Show, Eq)

data Address = Address
  { stake_address :: Text,
    script :: Bool
  }
  deriving (Generic, Show, Eq)

data Asset = Asset
  { unit :: Text,
    quantity :: Text
  }

deriveJSON defaultOptions 'HypeSkullPropertyOccurence
deriveJSON defaultOptions 'HypeSkullRarityScore
deriveJSON defaultOptions 'HypeSkullRank
deriveJSON defaultOptions 'HypeSkull
deriveJSON defaultOptions 'Main.Identity
deriveJSON defaultOptions 'AssetAddress
deriveJSON defaultOptions 'Address
deriveJSON defaultOptions 'Asset
deriveJSON defaultOptions 'DiscordIdt

hypeRoles :: Map Text Word64
hypeRoles =
  fromList
    [ ("Holy Grail", 881636191672352828),
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

        react @'GuildMemberUpdateEvt $ \(m1, m2) ->
          when (m1 == m2) do
            info @Text "Guild Update"
            onGuildUpdate m1 m2

        react @'MessageCreateEvt $ \(msg, _, _) ->
          when False $
            void . invoke $ CreateReaction msg msg (UnicodeEmoji "😄")

        react @'ReadyEvt \_ ->
          sendPresence $ StatusUpdateData Nothing (Just (Calamity.Types.Model.Presence.Activity.activity "H.Y.P.E." Game)) Online False

        addCommands $ do
          helpCommand
          command @'[Int, Maybe GuildChannel] "slowmode" $ \ctx seconds mchan -> do
            let cid = maybe (ctx ^. #channel . to getID) getID mchan :: Snowflake Channel
            void . invoke $
              ModifyChannel cid $
                def
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
                  Nothing -> void $ P.embed $ putStrLn "H.Y.P.E. SKULL score not found"
                  Just skullRarity -> do
                    skullRank <- P.embed $ querySkullRank $ Main.score skullRarity
                    skullPropertyOccurences <- P.embed $ querySkullPropertyOccurences skull
                    let fields = skullFields skull skullPropertyOccurences
                        embededSkull = embedSkull skullID skull
                    void $
                      tell @Embed ctx $
                        embededSkull
                          & #fields .~ (EmbedField "RARITY" ("```" <> rarityName (Main.score skullRarity) <> " #" <> T.pack (show $ Main.rank skullRank) <> "```") True : fields)
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
                    void $
                      tell @Text ctx $
                        mention user
                          <> " has successfully linked his IdentityToken.\n"
                          <> "https://cardanoscan.io/token/"
                          <> policyId identity'
                          <> "."
                          <> assetName identity'
                    let assetNameHex = toLower $ T.pack $ hex $ T.unpack $ assetName identity'
                        idt = policyId identity' <> assetNameHex
                    P.embed $ recordIdt (read $ T.unpack $ showt $ getID @User user) idt
                    skulls <- P.embed $ getHypeSkulls idt
                    let guild = ctx ^. #guild
                    case guild of
                      Nothing -> info @Text "Not in a guild"
                      Just g -> do
                        let roles = getHypeRoles g user skulls
                        info @Text $ "Assigning hype role to " <> showt user
                        mapM_
                          ( \roleKey -> do
                              info @Text $ "Assigning " <> roleKey <> " role to " <> showt user
                              let findRoleByKey key = Data.Map.lookup key hypeRoles
                                  role = findRoleByKey roleKey
                              case role of
                                Nothing -> info @Text "Role not found"
                                Just role -> do
                                  void $ P.embed $ threadDelay $ 1 * 1000 * 100
                                  void $ invoke $ AddGuildMemberRole g user $ Snowflake @Role role
                          ) roles

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
              Just as -> do
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
                  else void $ reply @Text (ctx ^. #message) "You are not an admin"
              Nothing -> info @Text "No Admin Id has been set" -- Can't be possible?
          command @'[Maybe (Snowflake User), Maybe (Snowflake Role)] "removerole" $ \ctx user role -> do
            void $ info @Text $ "User: " <> showt user
            void $ info @Text $ "Role: " <> showt role
            admins <- P.embed adminIds
            case admins of
              Just as -> do
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
                  else void $ reply @Text (ctx ^. #message) "You are not an admin"
              Nothing -> info @Text "No Admin Id has been set" -- Can't be possible?
          command @'[] "purgeroles" $ \ctx -> do
            admins <- P.embed adminIds
            case admins of
              Just as -> do
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
                        mapM_
                          ( \m ->
                              mapM_
                                ( \(k, v) -> do
                                    info @Text $ "Removing role " <> showt k <> " from " <> showt m
                                    void $ invoke $ RemoveGuildMemberRole g m (Snowflake v :: Snowflake Role)
                                )
                                (Data.Map.toList hypeRoles :: [(Text, Word64)])
                          )
                          memberIds
                        void $ reply @Text (ctx ^. #message) "Roles purged"
                  else void $ reply @Text (ctx ^. #message) "You are not an admin"
              Nothing -> info @Text "No Admin Id has been set" -- Can't be possible?
  where
    getMembers initialMembers g = do
      members <- getMembersInternal initialMembers g
      case members of
        Left _ -> return initialMembers
        Right m ->
          if length members < 1000
            then return (initialMembers <> m)
            else do
              getMembers (initialMembers <> m) g

    getLastMemberSnowflake :: [Member] -> Snowflake User
    getLastMemberSnowflake members = getID $ last members

    getMembersInternal [] g = do
      invoke $ ListGuildMembers g $ ListMembersOptions (Just 1000) Nothing
    getMembersInternal initialMembers g = do
      invoke $ ListGuildMembers g $ ListMembersOptions (Just 1000) (Just $ getLastMemberSnowflake initialMembers)

onGuildUpdate m1 m2 =  do
  let userTmpFile m1' = "/tmp/" ++ show (fromSnowflake (m1' ^. #id))
  userTmpFileExist <- P.embed $ doesFileExist $ userTmpFile m1
  if userTmpFileExist
    then do
      lastTime <- P.embed $ readFile $ userTmpFile m1
      currentTimePOSIX <- P.embed getPOSIXTime
      let lastTimePOSIX = read lastTime :: POSIXTime
      if currentTimePOSIX - lastTimePOSIX > (read "30s" :: POSIXTime) then
        proceed m1 m2
      else
        info @Text $ "Ignoring update for " <> showt (fromSnowflake (m1 ^. #id))
    else do
      time <- P.embed getPOSIXTime
      void $ P.embed $ writeFile (userTmpFile m1) $ show time
      proceed m1 m2

proceed m1 m2 = do
  info @Text $ showt m1
  user <- upgrade $ m1 ^. #id
  guild <- upgrade $ m1 ^. #guildID
  case (guild, user) of
    (Nothing, _) -> info @Text "Not in a guild"
    (_, Nothing) -> info @Text "User not found"
    (Just g, Just u) -> do
      info @Text $ "Processing User Role: " <> showt u
      idt <- P.embed $ tryGetMatchingIdt (read $ T.unpack $ showt $ getID @User u)
      case idt of
        Nothing -> info @Text "No matching record"
        Just idt' -> do
          skulls <- P.embed $ getHypeSkulls idt'

          let userRoles = (m1 ^. #roles) :: Vector (Snowflake Role)
          mapM_ (\r -> do
              if fromSnowflake r `elem` elems hypeRoles 
                then do
                  info @Text $ "Removing role " <> showt r <> " from " <> showt m1
                  void $ P.embed $ threadDelay $ 1 * 1000 * 100
                  void $ invoke $ RemoveGuildMemberRole g u r 
                else
                  info @Text $ "Invalid role to remove."
            ) (Data.Vector.Unboxing.toList userRoles :: [Snowflake Role])

          P.embed $ threadDelay $ 1 * 1000 * 1000

          let roles = getHypeRoles g u skulls
          info @Text $ "Assigning hype role to " <> showt user
          mapM_ (\roleKey -> do
            info @Text $ "Assigning " <> roleKey <> " role to " <> showt user
            let findRoleByKey key = Data.Map.lookup key hypeRoles
                role = findRoleByKey roleKey
            case role of
              Nothing -> info @Text "Role not found"
              Just role -> do
                void $ P.embed $ threadDelay $ 1 * 1000 * 100
                void $ invoke $ AddGuildMemberRole g u $ Snowflake @Role role) roles

getHypeRoles :: Guild -> User -> [HypeSkull] -> [Text]
getHypeRoles guild user = Prelude.foldr processSkull []
  where
    processSkull :: HypeSkull -> [Text] -> [Text]
    processSkull skull roles = Prelude.foldr (\(f, r) roles' -> if f skull && notElem r roles' then r : roles' else roles') roles roleChecks

    roleChecks :: [(HypeSkull -> Bool, Text)]
    roleChecks =
      [ (isHolyGrail, "Holy Grail"),
        (isErgoEyes, "Ergo Eyes"),
        (is3rdEye, "3rd Eye"),
        (isGlitch, "Glitch"),
        (isKoK, "KoK"),
        (isOnyx, "Onyx"),
        (isBlackDiamond, "Black Diamond"),
        (isDiamond, "Diamond"),
        (isGold, "Gold"),
        (isPearl, "Pearl"),
        (isPlatinum, "Platinum"),
        (isRoseGold, "Rose Gold")
      ]

    isHolyGrail :: HypeSkull -> Bool
    isHolyGrail skull = case tokenRarityScore skull of
      Just skullRarity -> "HOLY GRAIL" == rarityName (Main.score skullRarity)
      _ -> False

    isErgoEyes :: HypeSkull -> Bool
    isErgoEyes skull = "ergo" `isInfixOf` Main.specials skull

    is3rdEye :: HypeSkull -> Bool
    is3rdEye skull = "3rd" `isInfixOf` Main.specials skull

    isGlitch :: HypeSkull -> Bool
    isGlitch skull = Main.glitch skull

    isKoK :: HypeSkull -> Bool
    isKoK skull = "kush" `isInfixOf` Main.eyes skull && "kush" `isInfixOf` Main.smoke skull

    isOnyx :: HypeSkull -> Bool
    isOnyx skull = "onyx" == Main.skull skull

    isBlackDiamond :: HypeSkull -> Bool
    isBlackDiamond skull = "black diamond" == Main.skull skull

    isDiamond :: HypeSkull -> Bool
    isDiamond skull = "diamond" == Main.skull skull

    isGold :: HypeSkull -> Bool
    isGold skull = "gold" == Main.skull skull

    isPearl :: HypeSkull -> Bool
    isPearl skull = "pearl" == Main.skull skull

    isPlatinum :: HypeSkull -> Bool
    isPlatinum skull = "platinum" == Main.skull skull

    isRoseGold :: HypeSkull -> Bool
    isRoseGold skull = "rosegold" == Main.skull skull

    findRoleByKey :: Text -> Maybe Word64
    findRoleByKey key = Data.Map.lookup key hypeRoles

getHypeSkulls :: Text -> IO [HypeSkull]
getHypeSkulls idt = do
  addr <- getUserAddress idt
  stakeAddr <- getStakeAddress addr
  assets <- getAllAssets stakeAddr [] 1
  partialSkulls <- filterSkulls assets
  fullSkulls <- hydrateSkulls partialSkulls
  return $ catMaybes fullSkulls

bfProjectId :: IO Text
bfProjectId = do
  str <- getEnv "BF_PROJECT_ID"
  return $ pack str

getUserAddress :: Text -> IO Text
getUserAddress idt = do
  bfProjectId' <- bfProjectId
  runReq defaultHttpConfig $ do
    let header' = header "project_id" $ encodeUtf8 bfProjectId'
    r <-
      req GET (https "cardano-mainnet.blockfrost.io" /: "api" /: "v0" /: "assets" /: idt /: "addresses") NoReqBody jsonResponse $
        header'
          <> "order" =: ("desc" :: Text)
          <> "count" =: (1 :: Int)
    let addresses = responseBody r :: [AssetAddress]
    return $ address $ head addresses

getStakeAddress :: Text -> IO Text
getStakeAddress addr = do
  bfProjectId' <- bfProjectId
  runReq defaultHttpConfig $ do
    r <-
      req GET (https "cardano-mainnet.blockfrost.io" /: "api" /: "v0" /: "addresses" /: addr) NoReqBody jsonResponse $
        header "project_id" $ encodeUtf8 bfProjectId'
    let addr = responseBody r :: Address
    return $ stake_address addr

getAssets :: Text -> Int -> IO [Asset]
getAssets addr page = do
  bfProjectId' <- bfProjectId
  runReq defaultHttpConfig $ do
    let header' = header "project_id" $ encodeUtf8 bfProjectId'
    r <-
      req GET (https "cardano-mainnet.blockfrost.io" /: "api" /: "v0" /: "accounts" /: addr /: "addresses" /: "assets") NoReqBody jsonResponse $
        header'
          <> "page" =: (page :: Int)
    let assets = responseBody r :: [Asset]
    return assets

getAllAssets :: Text -> [Asset] -> Int -> IO [Asset]
getAllAssets addr currentAssets page = do
  assets <- getAssets addr page
  if length assets < 100
    then return $ currentAssets ++ assets
    else getAllAssets addr (currentAssets ++ assets) (page + 1)

filterSkulls :: [Asset] -> IO [Asset]
filterSkulls assets =
  return $
    filter
      ( \a ->
          T.unpack hypeSkullsSig == Prelude.take (length $ T.unpack hypeSkullsSig) (T.unpack $ unit a)
            && 82 == length (T.unpack $ unit a)
      )
      assets

hydrateSkulls :: [Asset] -> IO [Maybe HypeSkull]
hydrateSkulls = mapM f
  where
    f :: Asset -> IO (Maybe HypeSkull)
    f asset = do
      case skullNumber asset of
        "" -> return Nothing
        _ -> do
          partialSkull <- querySkull $ read $ T.unpack $ skullNumber asset
          fullSkull <- queryFullSkull $ Main.id partialSkull
          return $ Just fullSkull

    skullNumber :: Asset -> Text
    skullNumber asset = hexToAscii $ T.pack $ lastN 8 $ T.unpack $ unit asset

    -- taken from https://stackoverflow.com/questions/17252851/how-do-i-take-the-last-n-elements-of-a-list
    lastN :: Int -> [a] -> [a]
    lastN n xs = let m = length xs in Prelude.drop (m - n) xs

hexToAscii :: Text -> Text
hexToAscii str = case unhex $ T.unpack str of
  Left _ -> ""
  Right x -> T.pack x

hypeSkullsSig :: Text
hypeSkullsSig = "2f459a0a0872e299982d69e97f2affdb22919cafe1732de01ca4b36c48595045534b554c4c"

hsApiPassword :: IO Text
hsApiPassword = do
  str <- getEnv "HSAPI_PASSWORD"
  return $ pack str

recordIdt :: Int -> Text -> IO ()
recordIdt user idt = do
  hsApiPassword' <- hsApiPassword
  runReq defaultHttpConfig $ do
    let discordIdt =
          DiscordIdt
            { discordId = user,
              identityToken = idt
            }
    req POST (https "hsapi-cka5aaecrq-uw.a.run.app" /: "api" /: "v1.0" /: "discord") (ReqBodyJson discordIdt) ignoreResponse $
      header "api_password" $ encodeUtf8 hsApiPassword'
    return ()

getMatchingIdt :: Int -> IO Text
getMatchingIdt user = do
  hsApiPassword' <- hsApiPassword
  runReq defaultHttpConfig $ do
    r <-
      req GET (https "hsapi-cka5aaecrq-uw.a.run.app" /: "api" /: "v1.0" /: "discord" /: T.pack (show user)) NoReqBody jsonResponse $
        header "api_password" $ encodeUtf8 hsApiPassword'
    let discordIdt = responseBody r :: [Main.DiscordIdt]
    return $ identityToken $ head discordIdt

tryGetMatchingIdt :: Int -> IO (Maybe Text)
tryGetMatchingIdt user = do
  result <- try (getMatchingIdt user) :: IO (Either SomeException Text)
  case result of
    Left _ -> return Nothing
    Right idt -> return $ Just idt

embedAuthAddr :: Text -> Embed
embedAuthAddr addr =
  def
    & #title ?~ "Authenticate with your Identity Token"
    & #description ?~ "Please send **1.2 $ADA** within **5 minutes** to the wallet address provided below. Your **1 $ADA** will be returned once authentication is complete."
    & #fields
      .~ [ EmbedField "AUTH ADDRESS" ("```" <> addr <> "```") True
         ]
    & #image ?~ embedImage ("https://api.identity.adaph.io/qr/generate?data=" <> addr)

requestAuthAddress :: IO Text
requestAuthAddress = runReq defaultHttpConfig $ do
  r <- req GET (https "api.identity.adaph.io" /: "identity" /: "auth") NoReqBody jsonResponse mempty
  let addr = responseBody r :: Text
  return addr

queryIdentity :: Text -> IO Main.Identity
queryIdentity addr = runReq defaultHttpConfig $ do
  r <- req GET (https "api.identity.adaph.io" /: "identity" /: "token" /: addr) NoReqBody jsonResponse mempty
  let identity = responseBody r :: Main.Identity
  return identity

getIdentity :: Text -> Int -> IO (Maybe Main.Identity)
getIdentity addr retries = do
  result <- try (queryIdentity addr) :: IO (Either SomeException Main.Identity)
  case result of
    Right identity -> do
      return $ Just identity
    Left _ ->
      if retries >= maxRetries
        then return Nothing
        else do
          threadDelay $ retryInterval * 1000 * 1000
          getIdentity addr $ retries + 1
  where
    retryInterval = 20
    maxRetries = 15

embedSkull :: Int -> HypeSkull -> Embed
embedSkull skullID skull =
  def
    & #title ?~ skullIdToText skullID
    & #description ?~ "```" <> Main.title skull <> "```"
    & #image ?~ embedImage ("https://ipfs.infura.io/ipfs/" <> Main.imageCID skull)

skullIdToText :: Int -> Text
skullIdToText skullId = T.pack $ "HYPESKULL" ++ padLeft 4 '0' (show skullId)

querySkull :: Int -> IO HypeSkull
querySkull skullID = runReq defaultHttpConfig $ do
  let params = "query" =: ("hypeskull" ++ padLeft 4 '0' (show skullID) :: String)
  r <- req GET (https "hsapi-cka5aaecrq-uw.a.run.app" /: "api" /: "v1.0" /: "skulls") NoReqBody jsonResponse params
  let skull = responseBody r :: [HypeSkull]
  return $ head skull

queryFullSkull :: Text -> IO HypeSkull
queryFullSkull skullGUID = runReq defaultHttpConfig $ do
  r <- req GET (https "hsapi-cka5aaecrq-uw.a.run.app" /: "api" /: "v1.0" /: "skulls" /: skullGUID) NoReqBody jsonResponse mempty
  let skull = responseBody r :: HypeSkull
  return skull

querySkullRank :: Double -> IO HypeSkullRank
querySkullRank score = runReq defaultHttpConfig $ do
  let params = "score" =: (show score :: String)
  r <- req GET (https "hsapi-cka5aaecrq-uw.a.run.app" /: "api" /: "v1.0" /: "skulls" /: "properties" /: "rank") NoReqBody jsonResponse params
  let skullRank = responseBody r :: HypeSkullRank
  return skullRank

querySkullPropertyOccurence :: Text -> Text -> IO HypeSkullPropertyOccurence
querySkullPropertyOccurence key value = runReq defaultHttpConfig $ do
  let params =
        "key" =: key
          <> "value" =: value
  r <- req GET (https "hsapi-cka5aaecrq-uw.a.run.app" /: "api" /: "v1.0" /: "skulls" /: "properties") NoReqBody jsonResponse params
  let skullPropertyOccurence = responseBody r :: HypeSkullPropertyOccurence
  return skullPropertyOccurence

querySkullPropertyOccurences :: HypeSkull -> IO (Map Text HypeSkullPropertyOccurence)
querySkullPropertyOccurences skull = do
  let properties =
        [ ("skull", Main.skull skull),
          ("background", Main.background skull),
          ("smoke", Main.smoke skull),
          ("frame", Main.frame skull),
          ("eyes", Main.eyes skull),
          ("specials", Main.specials skull),
          ("glitch", toLower (T.pack (show (Main.glitch skull)))),
          ("logo", Main.logo skull)
        ]
  result <-
    mapM
      ( \(key, value) -> do
          occurence <- querySkullPropertyOccurence key value
          return (key, occurence)
      )
      properties
  return $ fromList result

skullFields :: HypeSkull -> Map Text HypeSkullPropertyOccurence -> [EmbedField]
skullFields skull occurences =
  [ EmbedField "OWNER" "```UNKNOWN```" True,
    EmbedField "MINT DATE" ("```" <> formatField (head (splitOn "T" (Main.dateCreated skull))) <> "```") True,
    EmbedField "SKULL" ("```" <> formatField (Main.skull skull) <> " " <> getOccurence "skull" <> "%```") True,
    EmbedField "BACKGROUND" ("```" <> formatField (Main.background skull) <> " " <> getOccurence "background" <> "%```") True,
    EmbedField "SMOKE" ("```" <> formatField (Main.smoke skull) <> " " <> getOccurence "smoke" <> "%```") True,
    EmbedField "FRAME" ("```" <> formatField (Main.frame skull) <> " " <> getOccurence "frame" <> "%```") True,
    EmbedField "EYES" ("```" <> formatField (Main.eyes skull) <> " " <> getOccurence "eyes" <> "%```") True,
    EmbedField "SPECIALS" ("```" <> formatField (Main.specials skull) <> " " <> getOccurence "specials" <> "%```") True,
    EmbedField "GLITCH" ("```" <> formatField (glitchString (Main.glitch skull)) <> " " <> getOccurence "glitch" <> "%```") True,
    EmbedField "LOGO" ("```" <> formatField (Main.logo skull) <> " " <> getOccurence "logo" <> "%```") True,
    EmbedField "FULL PREVIEW" ("https://old.seehype.com/explore/" <> Main.id skull) False
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
  | score > 1 = colour 0x8A78FF
  | otherwise = colour 0x8A78FF
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
  where
    t = 10 ^ n