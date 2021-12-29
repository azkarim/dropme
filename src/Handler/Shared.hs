{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Shared where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Database.Persist
import Database.Persist.Sqlite
import Data.Time (getCurrentTime, addUTCTime, secondsToNominalDiffTime) 
import Data.Fixed
import Data.UUID (UUID, toString)
import Data.UUID.V4 (nextRandom)

data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }


postSharedR :: Handler Html
postSharedR = do
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  uuid <- liftIO $ nextRandom
  case result of
     FormSuccess (FileForm file desc) -> do
        created <- liftIO $ getCurrentTime
        --liftIO $ fileMove file "/sombin"
        liftIO $ writeToServer file uuid
        _ <- runDB $ insert (File (toString uuid) (unpack $ fileName file) (unpack desc) created (addTime 5 created))
        return ()
     _ -> return ()
  defaultLayout $ do
    setTitle "File is shared successfully!"
    $(widgetFile "shared")

-- Write to disk logic

writeToServer :: FileInfo -> UUID -> IO ()
writeToServer file uuid = do
    let path_ = generateFilePath $ toString uuid
    fileMove file path_

generateFilePath :: String -> FilePath
generateFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "shared_files"

-- form render function

sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file to share"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "Name the file"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File name")
                ]
            }

-- Utility function
addTime :: Pico -> UTCTime -> UTCTime
addTime min utct =
  addUTCTime (secondsToNominalDiffTime (min * 60)) utct

utctToString :: UTCTime -> String
utctToString utct =
  formatTime defaultTimeLocale "%H:%M" utct
