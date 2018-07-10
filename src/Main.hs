{-# LANGUAGE OverloadedStrings #-}
-- https://www.schoolofhaskell.com/school/advanced-haskell/building-a-file-hosting-service-in-yesod/part%204


module Main where

import Control.Concurrent.STM

import Yesod

import Dispatch ()
import Foundation

main :: IO ()
main = do
    -- tnextid <- newTVarIO 1
    -- tstore <- newTVarIO []
    tstore <- atomically $ newTVar mempty
    tident <- atomically $ newTVar 0
    warp 3000 $ App tident tstore