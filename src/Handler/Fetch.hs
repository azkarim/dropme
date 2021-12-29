{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Fetch where

import Import
import System.Directory


getFetchR :: Handler ()
getFetchR = do
  maybeParam <- lookupGetParam "id"
  case maybeParam of
    Just param -> do
      let oldPath = generateFilePath $ unpack param
          newPath =  generateFilePath "hello.txt"
      liftIO $ renamePath oldPath newPath
      redirect (("/download/" <> "hello.txt") :: String)
    Nothing -> return ()

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
