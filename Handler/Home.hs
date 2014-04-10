{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Language.Haskell.TH ( Exp(..) )

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "LeeGauthier.ca: Welcome!"
        $(widgetFile "homepage")
        -- $(fayFile' (ConE 'StaticR) "Home")

getProjR :: Handler Html
getProjR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "LeeGauthier.ca: Projects"
        $(widgetFile "projects")
        -- $(fayFile' (ConE 'StaticR) "Home")
