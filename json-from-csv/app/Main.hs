{-# LANGUAGE DeriveGeneric #-} -- позволяет автоматически генерировать экземпляры классов типов (например, Generic) для пользовательских типов данных
{-# LANGUAGE OverloadedStrings #-} -- позволяет использовать строковые литералы для разных строковых типов (Text, ByteString и др.)

import Network.HTTP.Simple
-- import Data.Aeson (Value, object, (.=))
import Data.Char
import Data.Aeson (ToJSON, encode)
import Data.Csv (FromNamedRecord(..), decodeByName, (.:))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (Value, object, (.=), Array)
import qualified Data.Vector as V
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BSL

-- Определяем тип данных, соответствующий структуре CSV
-- автоматическое создание:
-- Generic экземпляра для сериализации/десериализации
-- Show экземпляра для преобразования в строку (для отладки)
data Lead = Lead
  { created :: Text
  , name  :: Text
  , phone :: Text
  , referer :: Text
  } deriving (Generic, Show)

-- Делаем тип сериализуемым в JSON
instance ToJSON Lead

-- Делаем тип десериализуемым из CSV
-- instance FromNamedRecord Lead -- Эта строка говорит: "Создай автоматическую реализацию для парсинга CSV в Lead"

-- можно указать явное соответствие
-- instance FromNamedRecord Lead where
--   parseNamedRecord r = Lead
--     <$> r .: "created"  -- берём поле "created"
--     <*> r .: "name"     -- затем поле "name"
--     <*> r .: "phone"    -- затем поле "phone"
--     <*> r .: "referer"  -- затем поле "referer"

instance FromNamedRecord Lead where
  parseNamedRecord r = do
    created <- r .: "created"
    name <- r .: "name"
    referer <- r .: "referer"
    rawPhone <- r .: "phone"
    let phone = T.filter isDigit rawPhone
    return $ Lead created name phone referer


main :: IO ()
main = do
    -- Чтение CSV-файла "input.csv" в ленивую байтовую строку
    -- требуется, чтобы CSV-файл имел заголовки, соответствующие полям типа Lead
    csvData <- BL.readFile "input.csv"

    case decodeByName csvData of
      Left err -> putStrLn $ "Ошибка парсинга CSV: " ++ err
      Right (_, itemsVector) -> do
        putStrLn $ "Успешно прочитано " ++ show (V.length itemsVector) ++ " записей"

        let jsonData = encode (itemsVector :: V.Vector Lead)
        BSL.putStrLn jsonData