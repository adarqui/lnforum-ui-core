{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

module LN.UI.Core.State.Internal (
  Store (..),
  defaultStore,
  ImmutableStore (..),
  defaultImmutableStore,
  Action (..)
) where



import           Control.DeepSeq            (NFData)
import           Data.Int                   (Int64)
import           Data.Map                   (Map)
import qualified Data.Map                   as Map
import           Data.Text                  (Text)
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Haskell.Api.Helpers
import           Haskell.Api.Helpers.Shared

import           LN.T
import           LN.UI.Core.Loader          (Loader (..))
import           LN.UI.Core.PageInfo        (PageInfo, defaultPageInfo)
import           LN.UI.Core.Router          (Route (..), RouteWith, routeWith')
import           LN.UI.Core.Types



data Store = Store {
        _route                    :: !RouteWith
      , _pageInfo                 :: !PageInfo
      , _errors                   :: ![Text]
      , _l_m_me                   :: !(Loader (Maybe UserResponse))
      , _meId                     :: !UserId
      , _usersCache               :: !(Map UserId UserSanitizedPackResponse)
      , _l_users                  :: !(Loader (Map UserId UserSanitizedPackResponse))
      , _l_boards                 :: !(Loader (Map BoardId BoardPackResponse))
      , _l_threads                :: !(Loader (Map ThreadId ThreadPackResponse))
      , _l_threadPosts            :: !(Loader (Map ThreadId ThreadPostPackResponse))
      , _l_m_user                 :: !(Loader (Maybe UserSanitizedPackResponse))
      , _l_m_forum                :: !(Loader (Maybe ForumPackResponse))
      , _m_forumRequest           :: !(Maybe ForumRequest)
      , _l_m_board                :: !(Loader (Maybe BoardPackResponse))
      , _m_boardRequest           :: !(Maybe BoardRequest)
      , _l_m_thread               :: !(Loader (Maybe ThreadPackResponse))
      , _m_threadRequest          :: !(Maybe ThreadRequest)
      , _l_m_threadPost           :: !(Loader (Maybe ThreadPostPackResponse))
      , _m_threadPostRequest      :: !(Maybe ThreadPostRequest)
      , _l_recentThreadPosts      :: !(Loader (Map ThreadPostId ThreadPostPackResponse))
      , _m_profileRequest         :: !(Maybe ProfileRequest)
      , _render                   :: !Bool
    } deriving (Typeable, Generic, NFData)



defaultStore :: Store
defaultStore = Store {
        _route                    = routeWith' Home
      , _pageInfo                 = defaultPageInfo
      , _errors                   = []
      , _l_m_me                   = Loaded Nothing
      , _meId                     = 0
      , _usersCache               = Map.empty
      , _l_users                  = Loaded Map.empty
      , _l_boards                 = Loaded Map.empty
      , _l_threads                = Loaded Map.empty
      , _l_threadPosts            = Loaded Map.empty
      , _l_m_user                 = Loaded Nothing
      , _l_m_forum                = Loaded Nothing
      , _m_forumRequest           = Nothing
      , _l_m_board                = Loaded Nothing
      , _m_boardRequest           = Nothing
      , _l_m_thread               = Loaded Nothing
      , _m_threadRequest          = Nothing
      , _l_m_threadPost           = Loaded Nothing
      , _m_threadPostRequest      = Nothing
      , _l_recentThreadPosts      = Loaded Map.empty
      , _m_profileRequest         = Nothing
      , _render                   = True
    }



data ImmutableStore = ImmutableStore {
    _apiOptions :: ApiOptions SpecificApiOptions
  } deriving (Typeable, Generic)



defaultImmutableStore :: ImmutableStore
defaultImmutableStore = ImmutableStore {
  _apiOptions = (defaultApiOptions :: ApiOptions SpecificApiOptions)
}



data Action
  = Nop
  | Init
  | Route RouteWith

  | MergeUsers [UserSanitizedResponse]   -- ^ Merge users into usersCache, fetching if they don't exist
  | MergeUserIds [UserId]                -- ^ Merge user id's into usersCache, fetching if they don't exist

  | MachNext Action                      -- ^ Used by an ln-ui-* frontend to obtain the next state

  | Save                                 -- ^ A global "Save", which uses State & current route to
                                         -- figure out what we are saving.

  | SaveThreadPost
                                         -- ^ Save, but specific to thread posts. Since thread posts
                                         -- can be saved from:
                                         -- OrganizationsForumsBoardsThreads and
                                         -- OrganizationsForumsBoardsThreadsPosts
                                         -- It needs its' own type
                                         --
  | SaveThreadPostInPlace

  | DoLike Ent Int64 (Maybe LikeRequest) -- Like, Neutral, Dislike, or Unlike something

  | ApplyState (Store -> Store)          -- ^ used only be ln-ui-* frontends. This is how they can set
                                         -- the Store state, using the same Action sum type

  | ApplyStateRender Bool (Store -> Store) -- ^ With this we can tell the frontend to render or not
                                           -- this might work in the future..

  | Goto RouteWith                       -- ^ used only by ln-ui-* frontends. This should change the
                                         -- current route, directory, etc.

  deriving (Typeable, Generic, NFData)
