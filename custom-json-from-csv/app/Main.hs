{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.Csv as Csv
import qualified Data.Aeson as Aeson
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import System.Environment (getArgs)
import Data.Csv (FromNamedRecord, ToNamedRecord)

-- Тип для представления строки CSV как набора пар ключ-значение
type CsvRow = HM.HashMap Text Text

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> processCsv inputFile outputFile
        _ -> putStrLn "Usage: csv2json <input.csv> <output.json>"

processCsv :: FilePath -> FilePath -> IO ()
processCsv inputFile outputFile = do
    csvData <- BL.readFile inputFile
    
    -- Явно указываем тип для decode
    case Csv.decodeByName csvData :: Either String (Csv.Header, V.Vector CsvRow) of
        Left err -> putStrLn $ "Error parsing CSV: " ++ err
        Right (_, rows) -> do
            let jsonData = Aeson.encode rows
            BL.writeFile outputFile jsonData
            putStrLn "Conversion successful. Result:"
            BLC.putStrLn jsonData