{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module PackageInfo_connector_ffi_test (
    name,
    version,
    synopsis,
    copyright,
    homepage,
  ) where

import Data.Version (Version(..))
import Prelude

name :: String
name = "connector_ffi_test"
version :: Version
version = Version [0,1,0,0] []

synopsis :: String
synopsis = "Haskell FFI tests for connector-service-FFI"
copyright :: String
copyright = ""
homepage :: String
homepage = ""
