{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Blog where

import Import
import Yesod.Auth
import Language.Haskell.TH ( Exp(..) )
import Utils.Utils

getBlogR :: Handler Html
getBlogR = do
    master <- getYesod
    maybeUser <- maybeAuth
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    (formWidget, formEnctype) <- generateFormPost entryForm
    (articleWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitle "LeeGauthier.ca: Blog"
        $(widgetFile "blog")

postBlogR :: Handler Html
postBlogR = do
    master <- getYesod
    maybeUser <- maybeAuth
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    ((result, formWidget), formEnctype) <- runFormPost entryForm
    case result of
         FormSuccess article -> do
            articleId <- runDB $ insert article
            setMessage $ toHtml $ articleTitle article <> " created"
            redirect $ ArticleR articleId
         _ -> defaultLayout $ do
                setTitle "Please correct your entry form"
                aDomId <- newIdent
                $(widgetFile "blog")

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")
        -- $(fayFile' (ConE 'StaticR) "Article")

deleteArticleR :: ArticleId -> Handler Html
deleteArticleR articleId = do
    _ <- runDB $ delete articleId
    getBlogR

entryForm :: Form Article
entryForm = renderDivs $ Article
     <$> areq textField "Title" Nothing
     <*> areq htmlField "Content" Nothing

getNewEntryR :: Handler Html
getNewEntryR = do
    (formWidget, formEnctype) <- generateFormPost entryForm
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "What are you doing here!?"
        $(widgetFile "new_article")
        -- $(fayFile' (ConE 'StaticR) "Home")
