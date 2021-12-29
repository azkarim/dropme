{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

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

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes.yesodroutes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe FileForm
        handlerName = "getHomeR" :: Text
    allComments <- runDB $ getAllComments
    allCreateds <- runDB $ getAllCreated

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yeso!"
        $(widgetFile "homepage")

-- formatTime defaultTimeLocale "%H:%M" <$> getCurrentTime
utctToString :: UTCTime -> String
utctToString utct =
  formatTime defaultTimeLocale "%H:%M" utct

addTime :: Pico -> UTCTime -> UTCTime
addTime min utct =
  addUTCTime (secondsToNominalDiffTime (min * 60)) utct

-- receive a http request and check for expired files

getAllCreated :: DB [Entity File]
getAllCreated = selectList [] [Asc FileCreated]


getAllComments :: DB [Entity Comment]
getAllComments = selectList [] [Asc CommentId]

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    case result of
       FormSuccess (FileForm file conn) -> do
          liftIO $ writeToServer file
           --liftIO $ fileMove file "/sombin"
          uuid <- liftIO $ nextRandom
          created <- liftIO $ getCurrentTime
          _ <- runDB $ insert (File (toString uuid) "hellofile" created (addTime 5 created))
          return ()
       _ -> return ()
    allComments <- runDB $ getAllComments
    allCreateds <- runDB $ getAllCreated

    defaultLayout $ do
        let (commentFormId, commentTextareaId, commentListId) = commentIds
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")



writeToServer :: FileInfo -> IO ()
writeToServer file = do
    let filename = unpack $ fileName file
        path_ = generateFilePath filename
    fileMove file path_

generateFilePath :: String -> FilePath
generateFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "shared_files"


sampleForm :: Form FileForm
sampleForm = renderBootstrap3 BootstrapBasicForm $ FileForm
    <$> fileAFormReq "Choose a file"
    <*> areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
    where textSettings = FieldSettings
            { fsLabel = "What's on the file?"
            , fsTooltip = Nothing
            , fsId = Nothing
            , fsName = Nothing
            , fsAttrs =
                [ ("class", "form-control")
                , ("placeholder", "File description")
                ]
            }

commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")

