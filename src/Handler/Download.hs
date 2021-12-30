{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Handler.Download where

import Import

getDownloadR :: String -> Handler ()
getDownloadR filename = do
  maybeUuid <- lookupGetParam "id"
  case maybeUuid of
    Just uuid -> sendFile typeOctet (generateFilePath $ unpack uuid)
    Nothing -> return ()

generateFilePath :: String -> FilePath
generateFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "shared_files"
