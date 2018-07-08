{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}


module Foundation where

import Control.Concurrent.STM
import Data.ByteString.Lazy (ByteString)
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Hamlet
import Yesod
import Yesod.Default.Util

import Data.IntMap

-- show
-- type StoredFile = (Text, ByteString)
-- type Store = [StoredFile]
-- data App = App (TVar Store)
data StoredFile = StoredFile !Text !ByteString
-- type Store = [(Int, StoredFile)]
type Store = IntMap StoredFile
data App = App (TVar Int) (TVar Store)

instance Yesod App where
  defaultLayout widget = do
    pc <- widgetToPageContent $ $(widgetFileNoReload def "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

mkYesodData "App" $(parseRoutesFile "config/routes")

getNextId :: App -> STM Int
getNextId (App tnextId _) = do
    nextId <- readTVar tnextId
    writeTVar tnextId $ nextId + 1
    return nextId

-- getList :: Handler [StoredFile]
getList :: Handler [(Int, StoredFile)]
getList = do
    App _ tstore <- getYesod
    -- liftIO $ readTVarIO tstore
    store <- liftIO $ readTVarIO tstore
    return $ Data.IntMap.toList store

addFile :: App -> StoredFile -> Handler ()
addFile app@(App _ tstore) file =
    liftIO . atomically $ do
        ident <- getNextId app
        -- modifyTVar tstore $ \ files -> (nextId, file) : files
        modifyTVar tstore $ Data.IntMap.insert ident file

-- getById :: Text -> Handler ByteString
getById :: Int -> Handler StoredFile
getById ident = do
    App _ tstore <- getYesod
    store <- liftIO $ readTVarIO tstore
    -- case Prelude.lookup ident store of
    case Data.IntMap.lookup ident store of
      Nothing -> notFound
      Just file -> return file
