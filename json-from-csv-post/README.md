# json-from-csv-post

Считывает csv и выводит его в формате json, также отправляет post-запросом на указанную в коде ссылку

Работает только по указанному типу
```haskell
data Lead = Lead
  { created :: Text
  , name  :: Text
  , phone :: Text
  , referer :: Text
  } deriving (Generic, Show)
```
