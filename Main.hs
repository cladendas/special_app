{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
-- import Data.Aeson (Value, object, (.=))
import Data.Aeson (ToJSON, encode)
import Data.Csv (FromNamedRecord, decodeByName)
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (Value, object, (.=), Array)
import qualified Data.Vector as V
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL

-- Определяем тип данных, соответствующий структуре CSV
data Lead = Lead
  { created :: Text
  , name  :: Text
  , phone :: Text
  , referer :: Text
  } deriving (Generic, Show)

-- Делаем тип сериализуемым в JSON
instance ToJSON Lead

-- Делаем тип десериализуемым из CSV
instance FromNamedRecord Lead

main :: IO ()
main = do
    csvData <- BL.readFile "input.csv"

    case decodeByName csvData of
      Left err -> putStrLn $ "Ошибка парсинга CSV: " ++ err
      Right (_, peopleVector) -> do
        let peopleList = V.toList peopleVector
        putStrLn $ "Успешно прочитано " ++ show (length peopleList) ++ " записей"

        let jsonData = encode (peopleList :: [Lead])
        BSL.putStrLn jsonData
