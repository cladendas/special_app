{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, status404)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as B
import Data.CaseInsensitive (original)

import Data.Time

import System.IO

-- Получить текущее время и отформатировать его
getCurrentTimeString :: IO String
getCurrentTimeString = do
    currentTime <- getCurrentTime
    let timeZone = hoursToTimeZone 3  -- Например, для UTC+3
    return $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" 
           $ utcToLocalTime timeZone currentTime

pathToFile :: String
pathToFile = "test.txt"

appendText :: String -> IO ()
appendText text = appendFile pathToFile ( "\n" ++ text ++ "\n")

respondOK :: (Response -> b) -> b
respondOK respond = respond $ responseLBS status200 
    [("Content-Type", "application/json")] 
    "{\"status\": \"ok\", \"code\": 200}"

-- применит указанную функцию к заголовкам
executeFuncForHeaders :: Monad m => ([Char] -> m b) -> Request -> m ()
executeFuncForHeaders someFunc req = mapM_ (\(name, value) -> someFunc $ "  " ++ B.unpack (original name) ++ ": " ++ B.unpack value) (requestHeaders req)

-- Обработчик POST запросов
app :: Application
app req respond = do
    case (requestMethod req, pathInfo req) of
        ("GET", ["raw"]) -> do
            contents <- BL.readFile pathToFile
            respond $ responseLBS status200 
                [("Content-Type", "text/plain")]
                contents

        ("POST", ["marquiz"]) -> do
            -- Читаем тело запроса
            body <- strictRequestBody req
            
            -- Выводим в консоль
            putStrLn "POST /marquiz"
            putStrLn "Headers:"
            executeFuncForHeaders putStrLn req
            putStrLn "Body:"
            putStrLn $ "  " ++ B.unpack (BL.toStrict body)
            putStrLn "---"
            
            respondOK respond

        ("POST", ["raw"]) -> do
            body <- lazyRequestBody req
            timeStr <- getCurrentTimeString

            handle <- openFile pathToFile WriteMode
            hPutStr handle $ timeStr ++ " --- "

            B.hPutStr handle (rawPathInfo req)
            B.hPutStr handle (rawQueryString req)
            BL.hPutStr handle "\nHeaders:\n"

            mapM_ (\(name, value) -> do
                BL.hPutStr handle "  "
                BL.hPutStr handle (BL.fromStrict $ original name)
                BL.hPutStr handle ": "
                BL.hPutStr handle (BL.fromStrict value)
                BL.hPutStr handle "\n"
                ) (requestHeaders req)

            BL.hPutStr handle "\nBody:\n"
            BL.hPutStr handle body

            hClose handle
            appendText (show req)

            respondOK respond
        
        _ -> do
            putStrLn $ "Not found: " ++ show (requestMethod req) ++ " " ++ show (pathInfo req)
            respond $ responseLBS status404 
                [("Content-Type", "application/json")] 
                "{\"status\": \"error\", \"code\": 404}"

main :: IO ()
main = do
    let port = 8080
    putStrLn $ "Server started on port " ++ show port
    putStrLn "Waiting for POST requests to /marquiz..."
    run port app