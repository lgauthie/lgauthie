module Utils.Utils where

import Import
import Yesod.Default.Config

isAdmin :: App -> Text -> Bool
isAdmin master email =
    if email == (extraAdminEmail $ appExtra $ settings master)
        then True
        else False
