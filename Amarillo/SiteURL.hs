{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE  DeriveDataTypeable, GeneralizedNewtypeDeriving #-}

module Amarillo.SiteURL where

import Web.Routes.TH  (derivePathInfo)
import Amarillo.Types (ArticleId)
import Data.Data      (Data, Typeable)

data Sitemap
    = Home
    | Article ArticleId
    deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''Sitemap)
