{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Dispatch where

import Yesod

import Foundation
import Handler.Home
import Handler.Preview

mkYesodDispatch "App" resourcesApp