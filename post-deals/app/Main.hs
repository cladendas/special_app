{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Aeson
import Data.Text (Text, unpack)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import qualified Data.Vector as V

import Data.Time
import qualified Data.ByteString.Char8 as BS
import Database.MySQL.Base
import Database.MySQL.Protocol.MySQLValue
import Control.Exception (try, SomeException)

data Deal = Deal
    { ufCrm1743768048407 :: Maybe Text
    , ufCrm67ED674CBBD90 :: Maybe Text
    , categoryId :: Maybe Text
    , assignedById :: Maybe Text
    , dealId :: Maybe Text
    , leadId :: Maybe Text
    , stageId :: Maybe Text
    , dateCreate :: Maybe Text
    , dateModify :: Maybe Text
    , opportunity :: Maybe Text
    , utmSource :: Maybe Text
    , utmMedium :: Maybe Text
    , utmCampaign :: Maybe Text
    , utmContent :: Maybe Text
    , utmTerm :: Maybe Text
    } deriving (Show, Generic)

-- это нужно для чётко сопоставления полей
instance FromJSON Deal where
    parseJSON = withObject "Deal" $ \v -> Deal
        <$> v .:? "UF_CRM_1743768048407"
        <*> v .:? "UF_CRM_67ED674CBBD90"
        <*> v .:? "CATEGORY_ID"
        <*> v .:? "ASSIGNED_BY_ID"
        <*> v .:? "ID"
        <*> v .:? "LEAD_ID"
        <*> v .:? "STAGE_ID"
        <*> v .:? "DATE_CREATE"
        <*> v .:? "DATE_MODIFY"
        <*> v .:? "OPPORTUNITY"
        <*> v .:? "UTM_SOURCE"
        <*> v .:? "UTM_MEDIUM"
        <*> v .:? "UTM_CAMPAIGN"
        <*> v .:? "UTM_CONTENT"
        <*> v .:? "UTM_TERM"

-- Структура для всего ответа
data ApiResponse = ApiResponse
    { result :: Maybe (V.Vector Deal)
    , next :: Maybe Int
    , total :: Maybe Int
    } deriving (Show, Generic)

instance FromJSON ApiResponse

requestBody = object [
    "order" .= object ["DATE_MODIFY" .= ("ASC" :: String)],
    "filter" .= object [
        ">=DATE_MODIFY" .= ("2025-07-19" :: String),
        "<DATE_MODIFY" .= ("2025-07-20" :: String)
    ],
    "select" .= ([
        "UF_CRM_1743768048407",
        "UF_CRM_67ED674CBBD90",
        "CATEGORY_ID",
        "ASSIGNED_BY_ID",
        "ID",
        "LEAD_ID",
        "STAGE_ID",
        "DATE_CREATE",
        "DATE_MODIFY",
        "OPPORTUNITY",
        "UTM_SOURCE",
        "UTM_MEDIUM",
        "UTM_CAMPAIGN",
        "UTM_CONTENT",
        "UTM_TERM"
    ] :: [String]),
    "start" .= (0 :: Int)
    ]

-- Подключение к MySQL
connectDB :: IO MySQLConn
connectDB = 
  connect defaultConnectInfo
    { ciHost = "host"
    , ciUser = "user"
    , ciPassword = "password"
    , ciDatabase = "database"
    }

-- Вставка или обновление записи
upsertDeal :: MySQLConn -> Deal -> IO ()
upsertDeal conn deal = do
    let query = "INSERT INTO SOME_TABLE (id_deal, state, funnel, date_create, date_modify) VALUES (?, ?, ?, ?, ?)"
    
    let params =
            [ maybe MySQLNull MySQLText (dealId deal)
            , maybe MySQLNull MySQLText (stageId deal)
            , MySQLText "TEST_TEST"  -- фиксированное значение для funnel
            , maybe MySQLNull MySQLText (dateCreate deal)
            , maybe MySQLNull MySQLText (dateModify deal)
            ]
  
    _ <- execute conn (Query $ BSL.pack query) params
    return ()

printDeal :: Deal -> IO ()
printDeal deal = do
    putStrLn $ "id - " ++ fromMaybe "_" (fmap unpack (dealId deal)) -- fmap unpack: преобразовывает Text -> String
    putStrLn $ "дата создания - " ++ fromMaybe "_" (fmap unpack (dateCreate deal))
    putStrLn $ "сумма - " ++ fromMaybe "_" (fmap unpack (opportunity deal))
    putStrLn $ ""

main :: IO ()
main = do
    let url = "URL"

    request <- parseRequest url
    let finalRequest = setRequestMethod "POST"
                $ setRequestBodyLBS (encode requestBody)
                $ addRequestHeader "Content-Type" "application/json"
                $ request
    
    response <- httpLBS finalRequest
    putStrLn $ "Status code: " ++ show (getResponseStatusCode response)
    putStrLn "Response body:"

    let responseBody = getResponseBody response
    -- BSL.putStrLn responseBody

    case eitherDecode responseBody of
        Left err -> putStrLn $ "Error parsing JSON: " ++ err
        Right apiResponse -> do
            putStrLn "\nExtracted values:"

            let totalDeals = show (total apiResponse)
            putStrLn totalDeals

            let nextDeals = show (next apiResponse)
            putStrLn "Value of next:"
            putStrLn nextDeals

            case result apiResponse of
                Just deals -> do 
                    putStrLn "All deals"
                    V.mapM_ printDeal deals
                    printDeal (V.head deals)
                    conn <- connectDB
                    -- upsertDeal conn (V.head deals)
                    V.mapM_ (\deal -> upsertDeal conn deal) deals
                Nothing -> putStrLn "No deals in response"
