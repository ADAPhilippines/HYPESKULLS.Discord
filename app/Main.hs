module Main where

import           Calamity hiding (title)
import           Calamity.Cache.InMemory
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext, useFullContext)
import           Calamity.Metrics.Noop
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.Map (Map, fromList, (!))
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
import Network.HTTP.Req (req, https, GET (GET), (/:), jsonResponse, responseBody, runReq, defaultHttpConfig, NoReqBody (NoReqBody), (=:))
import Data.Aeson
import Data.Aeson.TH
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import TextShow (TextShow(showt))
import System.Environment (getEnv)

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

deriveJSON defaultOptions 'HypeSkullPropertyOccurence
deriveJSON defaultOptions 'HypeSkullRarityScore
deriveJSON defaultOptions 'HypeSkullRank
deriveJSON defaultOptions 'HypeSkull

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
    . runBotIO (BotToken token) defaultIntents
    $ do
      info @Text "Bot starting up!"
      react @'MessageCreateEvt $ \(msg,_,_) -> do
        when ("Haskell" `T.isInfixOf` (msg ^. #content)) $
          void . invoke $ CreateReaction msg msg (UnicodeEmoji "😄")
      react @ 'ReadyEvt \_ -> do
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

embedSkull :: Int -> HypeSkull -> Embed
embedSkull skullID skull = def
  & #title ?~ skullIdToText skullID
  & #description ?~ "```" <> Main.title skull <> "```"
  & #image ?~ embedImage ("https://ipfs.blockfrost.dev/ipfs/" <> Main.imageCID skull)
  & #color ?~ testEmbedColour

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


rarityName :: Double -> Text
rarityName score
  | score > 900 = "HOLY GRAIL"
  | score > 550 = "LEGENDARY"
  | score > 200 = "SUPER RARE"
  | score > 100 = "VERY RARE"
  | score > 1 = "RARE"
  | otherwise = "UNKNOWN"

testEmbedColour :: Colour Double
testEmbedColour = extractColour $ colourFromWord64 10181046
  where extractColour (IntColour c) = c

padLeft :: Int -> Char -> String -> String
padLeft n c s = replicate (n - length s) c ++ s

padRight :: Int -> Char -> String -> String
padRight n c s = s ++ replicate (n - length s) c

truncate' :: Double -> Int -> Double
truncate' x n = fromIntegral (floor (x * t)) / t
    where t = 10^n