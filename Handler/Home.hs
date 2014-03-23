{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Language.Haskell.TH ( Exp(..) )

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    -- (formWidget, formEnctype) <- generateFormPost sampleForm
    -- let submission = Nothing :: Maybe (FileInfo, Text)
    --     handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome!"
        $(widgetFile "homepage")
        $(fayFile' (ConE 'StaticR) "Home")

-- The view showing the list of articles
getBlogR :: Handler Html
getBlogR = do
    -- Get the list of articles inside the database.
    articles <- runDB $ selectList [] [Desc ArticleTitle]
    -- We'll need the two "objects": articleWidget and enctype
    -- to construct the form (see templates/articles.hamlet).
    (articleWidget, enctype) <- generateFormPost entryForm
    defaultLayout $ do
        setTitle "Welcome To Yesod!"
        $(widgetFile "new_entry")

getArticleR :: ArticleId -> Handler Html
getArticleR articleId = do
    article <- runDB $ get404 articleId
    defaultLayout $ do
        setTitle $ toHtml $ articleTitle article
        $(widgetFile "article")

entryForm :: Form (FileInfo, Text)
entryForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
-- entryForm :: Form Article
-- entryForm = renderDivs $ Article
--     <$> areq   textField "Title" Nothing
--     <*> areq   nicHtmlField "Content" Nothing

getProjR :: Handler Html
getProjR = do
    -- (formWidget, formEnctype) <- generateFormPost sampleForm
    -- let submission = Nothing :: Maybe (FileInfo, Text)
    --     handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
        $(fayFile' (ConE 'StaticR) "Home")

postBlogR :: Handler Html
postBlogR = undefined

-- postHomeR :: Handler Html
-- postHomeR = do
--     ((result, formWidget), formEnctype) <- runFormPost sampleForm
--     let handlerName = "postHomeR" :: Text
--         submission = case result of
--             FormSuccess res -> Just res
--             _ -> Nothing
--
--     defaultLayout $ do
--         aDomId <- newIdent
--         setTitle "Welcome To Yesod!"
--         $(widgetFile "homepage")
--
-- sampleForm :: Form (FileInfo, Text)
-- sampleForm = renderDivs $ (,)
--     <$> fileAFormReq "Choose a file"
--     <*> areq textField "What's on the file?" Nothing
