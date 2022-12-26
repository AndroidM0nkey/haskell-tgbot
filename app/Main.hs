{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

import Telegram.Bot.API
import Telegram.Bot.Simple
import Telegram.Bot.Simple.UpdateParser

import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import           Network.HTTP.Simple
import qualified Data.Yaml             as Yaml
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.ByteString as S (ByteString, unpack)
import Data.Char as Char

import Control.Monad.Trans (liftIO)

import Data.Aeson.Text
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.=), (.:?), object, fromJSON)
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import Data.Text.Read
import Data.Either

import System.IO
import System.Random

import Data.Text.Encoding as TSE

data Model = Model
  { modelStatus :: Text
  , modelResult :: Result
  } deriving (Show, Eq, Ord)

data ProblemStatistics = ProblemStatistics
  { problemStatisticsSolvedCount :: Int
  , problemStatisticsIndex :: Text
  , problemStatisticsContestId :: Int
  } deriving (Show, Eq, Ord)

data Problems = Problems
  { problemsName :: Text
  , problemsType :: Text
  , problemsIndex :: Text
  , problemsTags :: [Text]
  , problemsRating :: Maybe Int
  , problemsPoints :: Maybe Int
  , problemsContestId :: Int
  } deriving (Show, Eq, Ord)

data Result = Result
  { resultProblems :: [Problems]
  , resultProblemStatistics :: [ProblemStatistics]
  } deriving (Show, Eq, Ord)

instance ToJSON Model where
  toJSON Model{..} = object
    [ "status" .= modelStatus
    , "result" .= modelResult
    ]

instance ToJSON ProblemStatistics where
  toJSON ProblemStatistics{..} = object
    [ "solvedCount" .= problemStatisticsSolvedCount
    , "index" .= problemStatisticsIndex
    , "contestId" .= problemStatisticsContestId
    ]

instance ToJSON Problems where
  toJSON Problems{..} = object
    [ "name" .= problemsName
    , "type" .= problemsType
    , "index" .= problemsIndex
    , "tags" .= problemsTags
    , "rating" .= problemsRating
    , "points" .= problemsPoints
    , "contestId" .= problemsContestId
    ]

instance ToJSON Result where
  toJSON Result{..} = object
    [ "problems" .= resultProblems
    , "problemStatistics" .= resultProblemStatistics
    ]

instance FromJSON Model where
  parseJSON (Object v) = do
    modelStatus <- v .: "status"
    modelResult <- v .: "result"
    pure $ Model{..}
  parseJSON invalid = do
    prependFailure "parsing Model failed, "
      (typeMismatch "Object" invalid)

instance FromJSON ProblemStatistics where
  parseJSON (Object v) = do
    problemStatisticsSolvedCount <- v .: "solvedCount"
    problemStatisticsIndex <- v .: "index"
    problemStatisticsContestId <- v .: "contestId"
    pure $ ProblemStatistics{..}
  parseJSON invalid = do
    prependFailure "parsing ProblemStatistics failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Problems where
  parseJSON (Object v) = do
    problemsName <- v .: "name"
    problemsType <- v .: "type"
    problemsIndex <- v .: "index"
    problemsTags <- v .: "tags"
    problemsRating <- v .:? "rating"
    problemsPoints <- v .:? "points"
    problemsContestId <- v .: "contestId"
    pure $ Problems{..}
  parseJSON invalid = do
    prependFailure "parsing Problems failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Result where
  parseJSON (Object v) = do
    resultProblems <- v .: "problems"
    resultProblemStatistics <- v .: "problemStatistics"
    pure $ Result{..}
  parseJSON invalid = do
    prependFailure "parsing Result failed, "
      (typeMismatch "Object" invalid)


filter' min max x = let Problems {..} = x in
                        if problemsRating == Nothing then False else
                          problemsRating >= min && problemsRating <= max

atRandIndex :: [a] -> IO a
atRandIndex l = do
    i <- randomRIO (0, length l - 1)
    return $ l !! i

data ModelB = ModelB
  { username :: Text}

defaultUsername :: Text
defaultUsername = "anonymus"

initialModelB :: ModelB
initialModelB = ModelB
  { username = defaultUsername}

data Action
  =  Start
  | Find Text
  deriving (Show, Read)


codeforcesBot :: BotApp ModelB Action
codeforcesBot = BotApp
  { botInitialModel = initialModelB
  , botAction = flip updateToAction
  , botHandler = handleAction
  , botJobs = []
  }
  where
    updateToAction :: ModelB -> Update -> Maybe Action
    updateToAction _ = parseUpdate $
          Find     <$> plainText
      <|> Start    <$  command "start"
      <|> Find     <$> command "find"
      <|> callbackQueryDataRead

    handleAction :: Action -> ModelB -> Eff Action ModelB
    handleAction action modelB = case action of
      Start -> modelB <# do
        reply (toReplyMessage startMessage)
          { replyMessageReplyMarkup = Just (SomeReplyKeyboardMarkup startKeyboard) }
      Find argv -> modelB <# do
        let sp = (Text.split (==' ') argv)
        let mn = (decimal (sp!!1))
        let mx = (decimal (sp!!2))
        request' <- liftIO $ parseRequest "POST https://codeforces.com"
        let request
                = setRequestMethod "POST"
                $ setRequestPath "/api/problemset.problems"
                $ setRequestQueryString [("tags", Just(TSE.encodeUtf8 (sp!!0)))]
                $ request'
        response <- httpJSON request


        let Model {..} = getResponseBody response
        if modelStatus == (Text.pack "OK") then do
          let Result {..} = modelResult
          let probs = filter (filter' (mnd mn) (mnd mx)) resultProblems

          if length probs == 0
              then do
                replyText "Нет задач с такими ограничениями"
              else do
                task <- liftIO $ (atRandIndex probs)
                let Problems {..} = task
                replyText problemsName
          else do
            replyText "Codeforces умер"


    startMessage = Text.unlines
      [ "Умеет искать задачи по запросу /find <Тематика> <Мин рейтинг> <Макс Рейтинг>"
      ]

    startKeyboard :: ReplyKeyboardMarkup
    startKeyboard = ReplyKeyboardMarkup
      { replyKeyboardMarkupKeyboard =
          [ [ "strings 1000 1500", "2-sat 2000 3000" ]
          , [ "trees 1200 1700", "math 500 1000" ]
          ]
      , replyKeyboardMarkupResizeKeyboard = Just True
      , replyKeyboardMarkupOneTimeKeyboard = Just True
      , replyKeyboardMarkupSelective = Nothing
      , replyKeyboardMarkupInputFieldSelector = Nothing
      }


mnd (Right number) = Just (fst number)
mnd (Left _)       = Nothing

run :: Token -> IO ()
run token = do
  env <- defaultTelegramClientEnv token
  startBot_ (conversationBot updateChatId codeforcesBot) env

main :: IO ()
main = do
  putStrLn "Please, enter Telegram bot's API token:"
  token <- Token . Text.pack <$> getLine
  run token
