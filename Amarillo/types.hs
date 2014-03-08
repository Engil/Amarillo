{-# LANGUAGE  DeriveDataTypeable, GeneralizedNewtypeDeriving #-}
module Amarillo.Types where

import Web.Routes               (PathInfo(..))
import Web.Routes.Happstack     (implSite)
import Data.Data                (Data, Typeable)

newtype ArticleId = ArticleId { unArticleId :: Int }
    deriving (Eq, Ord, Enum, Read, Show, Data, Typeable, PathInfo)
