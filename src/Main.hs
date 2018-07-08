{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent.STM

import Yesod

import Dispatch ()
import Foundation

main :: IO ()
main = do
    tnextid <- newTVarIO 1
    tstore <- newTVarIO []
    warp 3000 $ App tnextid tstore