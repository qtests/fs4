{-# LANGUAGE OverloadedStrings #-}

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