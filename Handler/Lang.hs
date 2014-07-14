{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Handler.Lang where

import Import

postLangR :: Handler ()
postLangR = do
    lang <- runInputPost $ ireq textField "lang"
    setLanguage lang
    redirect HomeR