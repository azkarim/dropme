{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Share where
import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Database.Persist
import Database.Persist.Sqlite
import Data.Time (getCurrentTime, addUTCTime, secondsToNominalDiffTime) 
import Data.Fixed
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)

-- Define our data that will be used for creating the form.
data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

getShareR :: Handler Html
getShareR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    defaultLayout $ do
        setTitle "Share a file"
        $(widgetFile "share")

addTime :: Pico -> UTCTime -> UTCTime
addTime min utct =
  addUTCTime (secondsToNominalDiffTime (min * 60)) utct

utctToString :: UTCTime -> String
utctToString utct =
  formatTime defaultTimeLocale "%H:%M" utct

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
