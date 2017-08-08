{-# LANGUAGE NoImplicitPrelude #-}
module UI.Widgets.Help
( mkWidget
) where

import ClassyPrelude

import Brick.Types (Widget, Padding(..))
import Brick.Widgets.Core (str, withAttr, padLeft)

import qualified UI.Widgets.Status as Status

mkWidget :: Widget n
mkWidget = withAttr Status.attrName $ padLeft Max $ str "?: help"
