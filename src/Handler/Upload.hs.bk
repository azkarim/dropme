{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Upload where

import Import
import Text.Julius

postUploadR :: Handler Html
postUploadR = do
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
    toWidget [hamlet|#{files'}|]
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
