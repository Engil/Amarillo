{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving,
    TemplateHaskell #-}
module Main where

import Prelude                 hiding (head)
import Control.Monad           (msum)
import Data.Monoid             (mconcat)
import Data.Text               (pack)
import Happstack.Server
    ( Response, ServerPartT, ok, toResponse, simpleHTTP
    , nullConf, seeOther, dir, notFound, seeOther)
import qualified Text.Blaze.Html4.Strict as H
import qualified Text.Blaze.Html4.Strict.Attributes as A
import Web.Routes             (RouteT, showURL, runRouteT, Site(..), setDefault, mkSitePI)
import Web.Routes.Happstack    (implSite)

import Amarillo.Types           as Amarillo
import Amarillo.SiteURL         (Sitemap (..))

route :: Sitemap -> RouteT Sitemap (ServerPartT IO) Response
route url =
    case url of
      Home                -> homePage
      (Article articleId) -> articlePage articleId

homePage :: RouteT Sitemap (ServerPartT IO) Response
homePage = do
  articles <- mapM mkArticle [(ArticleId 1) .. (ArticleId 10)]
  ok $ toResponse $
    H.html $ do
      H.head $ H.title $ (H.toHtml "Welcome Home!")
      H.body $ do
        H.ol $ mconcat articles
  where
    mkArticle articleId =
        do url <- showURL (Article articleId)
           return $ H.li $ H.a H.! A.href (H.toValue url) $
              H.toHtml $ "Article " ++ (show $ unArticleId articleId)

articlePage :: Amarillo.ArticleId -> RouteT Sitemap (ServerPartT IO) Response
articlePage (Amarillo.ArticleId articleId) = do
  homeURL <- showURL Home
  ok $ toResponse $
     H.html $ do
       H.head $ H.title $ (H.toHtml $ "Article " ++ show articleId)
       H.body $ do
        H.p $ do H.toHtml $ "You are now reading article "
                 H.toHtml $ show articleId
        H.p $ do H.toHtml "Click "
                 H.a H.! A.href (H.toValue homeURL) $ H.toHtml "here"
                 H.toHtml " to return home."

site :: Site Sitemap (ServerPartT IO Response)
site =
       setDefault Home $ mkSitePI (runRouteT route)

main :: IO ()
main = simpleHTTP nullConf $ msum
  [ dir "favicon.ico" $ notFound (toResponse ())
  , implSite (pack "http://localhost:8000") (pack "/route") site
  , seeOther "/route" (toResponse ())
  ]