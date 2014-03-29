{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Yesod.Auth
import Yesod.Default.Config
import Language.Haskell.TH ( Exp(..) )

isAdmin :: App -> Text -> Bool
isAdmin master email =
    if email == (extraAdminEmail $ appExtra $ settings master)
        then True
        else False

getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "LeeGauthier.ca: Welcome!"
        $(widgetFile "homepage")
        $(fayFile' (ConE 'StaticR) "Home")

getBlogR :: Handler Html
getBlogR = do
    master <- getYesod
    maybeUser <- maybeAuth
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    (formWidget, formEnctype) <- generateFormPost entryForm
    (articleWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitle "LeeGauthier.ca: Blog"
        $(widgetFile "new_entry")

postBlogR :: Handler Html
postBlogR = do
    master <- getYesod
    maybeUser <- maybeAuth
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    ((result, formWidget), formEnctype) <- runFormPost entryForm
    case result of
         FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage $ toHtml $ (articleTitle article) <> " created"
            redirect $ ArticleR articleId
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                aDomId <- newIdent
                $(widgetFile "new_entry")

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")

entryForm :: Form Article
entryForm = renderDivs $ Article
     <$> areq textField "Title" Nothing
     <*> areq htmlField "Content" Nothing

getProjR :: Handler Html
getProjR = do
    (formWidget, formEnctype) <- generateFormPost entryForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "LeeGauthier.ca: Projects"
        $(widgetFile "projects")
        $(fayFile' (ConE 'StaticR) "Home")

getNewEntryR :: Handler Html
getNewEntryR = do
    (formWidget, formEnctype) <- generateFormPost entryForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "What are you doing here!?"
        $(widgetFile "new_article")
        $(fayFile' (ConE 'StaticR) "Home")
