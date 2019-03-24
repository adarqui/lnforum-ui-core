{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE ExplicitForAll   #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE RecordWildCards  #-}

module LN.UI.Core.App.Board (
    setDisplayName
  , setDescription
  , clearDescription
  , setIsAnonymous
  , setCanCreateBoards
  , setCanCreateThreads
  , setTag
  , addTag
  , deleteTag
  , clearTags
) where



import           Data.Text          (Text)

import           LN.T
import qualified LN.UI.Core.App.Tag as Tag
import           LN.UI.Core.State



setDisplayName :: BoardRequest -> Text -> Action
setDisplayName !request@BoardRequest{..} !input =
  ApplyState (\st->st{_m_boardRequest = Just $ request{boardRequestDisplayName = input}})



setDescription :: BoardRequest -> Text -> Action
setDescription !request@BoardRequest{..} !input =
  ApplyState (\st->st{_m_boardRequest = Just $ request{boardRequestDescription = Just input}})



clearDescription :: BoardRequest -> Action
clearDescription !request@BoardRequest{..} =
  ApplyState (\st->st{_m_boardRequest = Just $ request{boardRequestDescription = Nothing }})



setIsAnonymous :: BoardRequest -> Bool -> Action
setIsAnonymous !request@BoardRequest{..} !input =
  ApplyState (\st->st{_m_boardRequest = Just $ request{boardRequestIsAnonymous = input }})



setCanCreateBoards :: BoardRequest -> Bool -> Action
setCanCreateBoards !request@BoardRequest{..} !input =
  ApplyState (\st->st{_m_boardRequest = Just $ request{boardRequestCanCreateBoards = input }})



setCanCreateThreads :: BoardRequest -> Bool -> Action
setCanCreateThreads !request@BoardRequest{..} !input =
  ApplyState (\st->st{_m_boardRequest = Just $ request{boardRequestCanCreateThreads = input }})



setTag :: BoardRequest -> Text -> Action
setTag _ _ =
   ApplyState id
-- setTag t@BoardRequest{..} !input =
   -- ApplyState (\st->
   --   st{
   --     _m_boardRequest = Just $ request{boardRequestStateTag = Just input}
   --   })



addTag :: BoardRequest -> Action
addTag _ =
  ApplyState id
-- addTag !request@BoardRequest{..} =
  -- ApplyState (\st->
  --   st{
  --     _m_boardRequest = Just $ request{boardRequestTags = tags, boardRequestStateTag = Nothing}
  --   })
  -- where
  -- (tags, _) = Tag.addTag boardRequestTags boardRequestStateTag



deleteTag :: BoardRequest -> Int -> Action
deleteTag !request@BoardRequest{..} !idx =
  ApplyState (\st->
    st{
      _m_boardRequest = Just $ request{boardRequestTags = tags}
    })
  where
  tags = Tag.deleteTag boardRequestTags idx



clearTags :: BoardRequest -> Action
clearTags !request@BoardRequest{..} =
  ApplyState (\st->st{_m_boardRequest = Just $ request{boardRequestTags = []}})
