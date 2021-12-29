{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Download where

import Import
import System.Directory

getDownloadR :: String -> Handler ()
getDownloadR filename = do
  let path_ = generateFilePath filename
  _ <- sendFile typeOctet path_
  maybeParam <- lookupGetParam "id"
  case maybeParam of
    Just param -> do
      let newPath = generateFilePath $ unpack param
      liftIO $ renamePath path_ newPath
    Nothing -> return ()

generateFilePath :: String -> FilePath
generateFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "shared_files"
