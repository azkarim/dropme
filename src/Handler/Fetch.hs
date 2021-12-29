{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Fetch where

import Import

getFetchR :: String -> Handler Html
getFetchR uuid = do
  let path_ = generateFilePath uuid
  sendFile typeOctet path_


generateFilePath :: String -> FilePath
generateFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "shared_files"


{--
getFetchR :: String -> Handler Html
getFetchR string = 
  defaultLayout $ do
    setTitle "Fetch a file"
    $(widgetFile "fetch")
--}
