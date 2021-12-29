module Handler.Download where

import Import

getDownloadR :: String -> Handler Html
getDownloadR filename = do
  let path_ = generateFilePath filename
  sendFile typeOctet path_

generateFilePath :: String -> FilePath
generateFilePath f = uploadDirectory </> f

uploadDirectory :: FilePath
uploadDirectory = "shared_files"
