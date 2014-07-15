{-# LANGUAGE TupleSections, OverloadedStrings, RecordWildCards #-}
module Handler.Home where

import Import
import Data.Time.Clock (getCurrentTime)
import Yesod.Form.Bootstrap3


getHomeR :: Handler Html
getHomeR = do
    rides <- runDB $ selectList [] [Desc RidesAdded]
    r <- getMessageRender 
    ((res, widget), enctype) <- runFormPost (ridesForm (r MsgName) (r MsgDestination) (r MsgPhone) (r MsgLeaving) (r MsgNumSpots)) 
    defaultLayout $(widgetFile "homepage") 

postHomeR :: Handler ()
postHomeR = do
    r <- getMessageRender 
    ((res, _), _) <- runFormPost (ridesForm (r MsgName) (r MsgDestination) (r MsgPhone) (r MsgLeaving) (r MsgNumSpots)) 
    case res of
        FormSuccess ride -> do
            _ <- runDB $ insert ride 
            redirect HomeR
        _ -> redirect HomeR
        
ridesForm :: Text -> Text -> Text -> Text -> Text -> Form Rides
ridesForm name dest phone leaving sports = renderBootstrap3 BootstrapInlineForm $ Rides
              <$> areq textField (withPlaceholder name $ bfs ("Name" :: Text)) Nothing
              <*> areq textField (withPlaceholder dest $ bfs ("Destination" :: Text)) Nothing
              <*> areq textField (withPlaceholder phone $ bfs ("Phone" :: Text)) Nothing
              <*> areq textField (withPlaceholder leaving $ bfs ("Leaving" :: Text)) Nothing
              <*> areq intField (withPlaceholder sports $ bfs ("Number of spots" :: Text)) Nothing
              <*> lift (liftIO getCurrentTime)
   