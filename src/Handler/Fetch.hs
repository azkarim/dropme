{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Fetch where

import Import
import System.Directory

getFetchR :: Handler ()
getFetchR = do
  maybeParam <- lookupGetParam "id"
  case maybeParam of
    Just param -> do
      maybeFile <- runDB $ getFilename $ unpack param
      case maybeFile of
        Just file -> do
          let oldPath = generateFilePath $ unpack param
              newPath =  generateFilePath filename_
              filename_ = fileFilename $ entityVal file
          liftIO $ renamePath oldPath newPath
          redirect (("/download/" <> filename_ <> "?id=" <> (unpack param)) :: String)
        Nothing -> return ()
    Nothing -> return ()

generateFilePath :: String -> FilePath
generateFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "shared_files"

getFilename :: String -> DB (Maybe (Entity File))
getFilename uuid =
  selectFirst [FileUuid ==. uuid ] []

{--
getFetchR :: String -> Handler Html
getFetchR string = 
  defaultLayout $ do
    setTitle "Fetch a file"
    $(widgetFile "fetch")
--}
