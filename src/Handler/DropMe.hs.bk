{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.DropMe where

import Import
import Text.Julius

data FileForm = FileForm
    { fileInfo :: FileInfo
    , fileDescription :: Text
    }

postDropMeR :: Handler Html
postDropMeR = do
  {--
  let fs = FieldSettings "fileUpload" mzero (Just "fileUpload") mzero mzero
      mForm = mreq fileField fs Nothing
  --(tRes, tView) <- mreq fileField fs Nothing
  ((result, widget_), enctype) <- runFormPostNoToken (\_ -> mForm)
  let maybeFile = 
        case result of
          FormSuccess file -> Just file
          _ -> Nothing
  
--}
  --formResponseData <- runRequestBody
  (param, files) <- runRequestBody
  let files'   = (show ([ show name | (name, value) <- files]))
  defaultLayout $ do
    toWidget [hamlet|
                Hello World
                #{files'}
                <ul>
              |]
    {--
    toWidget [hamlet|
                Hello World
                <ul>
                $forall file <- processForm formResponseData []
                    <li>#{fileContentType file}
              |]
    --}
    -- toWidget [hamlet|
                -- $maybe file <- maybeFile
                  -- \ #{fileContentType file}
                -- $nothing
                  -- Nothing
              -- |]
    --toWidget [hamlet|#{fmap fileContentType maybeFile}|]
    {--[whamlet|
      $maybe maybeFile
        Your file type #{fileContentType maybeFile}
      $nothing 
        File can't be fetched
    |]
--}

--type RequestBodyContents = ([(Text, Text)], [(Text, FileInfo)]) 
processForm :: RequestBodyContents -> [FileInfo] -> [FileInfo]
processForm (formInfoList,fileInfoList) fileAcc =
    case fileInfoList of
      (_,firstFile) : rest ->
        processForm (formInfoList, rest) (firstFile : fileAcc)
      [] ->
        fileAcc

  {--
someHamlet maybeFile =
  $maybe maybeFile
    Your file type_ #{fileContentType maybeFile}
  $nothing 
    File can't be fetched
--}

getDropMeR :: Handler Html
getDropMeR = defaultLayout $ do
  addStylesheet $ StaticR css_main_css
  [whamlet|<div #root>|]
  addScript $ StaticR js_main_js
  --addStyleSheet $ StaticR css_main_css
