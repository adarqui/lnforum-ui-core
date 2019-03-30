{-# LANGUAGE ExplicitForAll    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | MUST REDUCE THE SIZE OF THIS FILE TO ~500 LINES
-- Too much redundant insanity.

module LN.UI.Core.App (
  runCore
) where



import           Control.Monad.IO.Class     ()
import           Control.Monad.RWS.Strict
import           Control.Monad.Trans.Either
import           Data.Int                   (Int64)
import           Data.List                  (nub)
import qualified Data.Map                   as Map
import           Data.Rehtie
import           Haskell.Api.Helpers
import           Haskell.Api.Helpers.Shared
import           Haskell.Helpers.Either

import           LN.Api
import qualified LN.Api.String              as ApiS
import           LN.Generate.Default
import           LN.T
import           LN.T.Convert
import           LN.T.Param
import           LN.UI.Core.Api
import           LN.UI.Core.Control
import           LN.UI.Core.Helpers.Map
import           LN.UI.Core.Loader
import           LN.UI.Core.PageInfo
import           LN.UI.Core.Router
import           LN.UI.Core.State
import           LN.UI.Core.Types



-- | Our state machine
--
runCore
  :: forall m. MonadIO m
  => CoreState                 -- ^ Our current State
  -> CoreResult                -- ^ We fetch data differently based on CoreResult
  -> Action                    -- ^ The action we are operating on
  -> m (CoreResult, CoreState) -- ^ The newly computed route & state

runCore st core_result (ApplyStateRender b f) = pure (core_result, (f st) { _render = b })
runCore st core_result (ApplyState f) = pure (core_result, f st)
runCore st _ (MachNext action)        = runCore st Next action
runCore st core_result action         = runCoreM st $ do
  case action of
    Init                     -> basedOn load_init fetch_init
    Route route_with         -> act_route route_with
    MergeUsers users         -> act_merge_users users
    MergeUserIds ids         -> act_merge_user_ids ids
    Save                     -> act_save
    SaveThreadPost           -> act_save_threadPost
    SaveThreadPostInPlace    -> act_save_threadPost_inPlace
    DoLike ent ent_id m_like -> act_do_like ent ent_id m_like

    -- Operations that should only run on a frontend.
    _ -> done

  where

  basedOn_ core_result_ start_ next_ done_ = case core_result_ of
    Start     -> start_
    Next      -> next_
    Done      -> done_
    Failure   -> failure
    Reroute r -> reroute r

  basedOn start_ next_ = basedOn_ core_result start_ next_ done



  fetch_init = do
    lr <- api getMe'
    liftIO $ putStrLn $ "WTF: me: " <> show lr
    rehtie lr (const cantLoad_init) $ \user_pack -> do
      let UserResponse{..} = user_pack
      modify (\st_->st_{_l_m_me = Loaded $ Just user_pack, _meId = userResponseId})
      done

  load_init = modify (\st'->st'{_l_m_me = Loading}) *> next

  cantLoad_init = modify (\st'->st'{_l_m_me = CantLoad}) *> done



  setRoute route_with = modify (\st'->st'{_route = route_with})

  act_route route_with = setRoute route_with *> case route_with of
    RouteWith Home _   -> basedOn load_boot fetch_boot
    RouteWith About _  -> start
    RouteWith Portal _ -> start

    RouteWith (Boards New) _                 -> basedOn load_boards_new (fetch_boards_new )
    RouteWith (Boards Index) _               -> basedOn load_boards_index (fetch_boards_index )
    RouteWith (Boards (ShowS board_sid)) _   -> basedOn load_boards_show (fetch_boards_show  board_sid)
    RouteWith (Boards (EditS board_sid)) _   -> basedOn load_boards (fetch_boards  board_sid)
    RouteWith (Boards (DeleteS board_sid)) _ -> basedOn load_boards (fetch_boards  board_sid)

    RouteWith (BoardsThreads board_sid New) _                  -> basedOn load_boards_threads_new (fetch_boards_threads_new  board_sid)
    RouteWith (BoardsThreads board_sid Index) _                -> basedOn load_boards_threads_index (fetch_boards_threads_index  board_sid)
    RouteWith (BoardsThreads board_sid (ShowS thread_sid)) _   -> basedOn load_boards_threads_show (fetch_boards_threads_show  board_sid thread_sid)
    RouteWith (BoardsThreads board_sid (EditS thread_sid)) _   -> basedOn load_boards_threads (fetch_boards_threads  board_sid thread_sid)
    RouteWith (BoardsThreads board_sid (DeleteS thread_sid)) _ -> basedOn load_boards_threads (fetch_boards_threads  board_sid thread_sid)

    RouteWith (BoardsThreadsPosts board_sid thread_sid New) _               -> basedOn load_boards_threads_posts_new (fetch_boards_threads_posts_new  board_sid thread_sid)
    RouteWith (BoardsThreadsPosts board_sid thread_sid Index) _             -> basedOn load_boards_threads_posts_index (fetch_boards_threads_posts_index  board_sid thread_sid)
    RouteWith (BoardsThreadsPosts board_sid thread_sid (ShowI post_id)) _   -> basedOn load_boards_threads_posts_show (fetch_boards_threads_posts_show  board_sid thread_sid post_id)
    RouteWith (BoardsThreadsPosts board_sid thread_sid (EditI post_id)) _   -> basedOn load_boards_threads_posts (fetch_boards_threads_posts  board_sid thread_sid post_id)
    RouteWith (BoardsThreadsPosts board_sid thread_sid (DeleteI post_id)) _ -> basedOn load_boards_threads_posts (fetch_boards_threads_posts  board_sid thread_sid post_id)

    RouteWith (Users Index) _                 -> basedOn load_users fetch_users
    RouteWith (Users (ShowS user_sid)) _      -> basedOn load_user (fetch_user user_sid)
    RouteWith (UsersProfile user_sid _) _     -> basedOn load_user (fetch_user user_sid)

    -- RouteWith (Users (ShowS user_sid)) _   -> start
    -- RouteWith (Users (EditS user_sid)) _   -> start
    -- RouteWith (Users (DeleteS user_sid)) _ -> start

    RouteWith (Experiments _) _ -> do_experiments

    RouteWith _ _               -> start

    where
    route               = case route_with of RouteWith route' _ -> route'
    params              = case route_with of RouteWith _ params' -> params'
    page_info           = pageInfoFromParams params
    params_list         = paramsFromPageInfo page_info
    params_list_dsc     = paramsFromPageInfo (page_info { sortOrder = SortOrderBy_Dsc, order = OrderBy_ActivityAt })

    new_page_info count = runPageInfo count page_info




    do_experiments :: MonadIO m => CoreM m CoreResult
    do_experiments = do
      -- Just load up some mock stuff that we may need for experiments
      modify (\st'->st'{
        _m_threadPostRequest = Just defaultThreadPostRequest
      })
      done


    load_users :: MonadIO m => CoreM m CoreResult
    load_users = modify (\st'->st'{_l_users = Loading}) *> next

    cantLoad_users :: MonadIO m => CoreM m CoreResult
    cantLoad_users = modify (\st'->st'{_l_users = CantLoad}) *> done

    fetch_users :: MonadIO m => CoreM m CoreResult
    fetch_users = do
      lr <- runEitherT $ do
        count <- mustPassT $ api $ getUsersCount'
        users <- mustPassT $ api $ getUserSanitizedPacks params_list
        pure (count, users)
      rehtie lr (const cantLoad_users) $ \(count, user_packs) -> do
        modify (\st'->st'{
            _l_users = Loaded $ idmapFrom userSanitizedPackResponseUserId (userSanitizedPackResponses user_packs)
          , _pageInfo = new_page_info count
          })
        done



    load_user :: MonadIO m => CoreM m CoreResult
    load_user = modify (\st'->st'{_l_m_user = Loading}) *> next

    cantLoad_user :: MonadIO m => CoreM m CoreResult
    cantLoad_user = modify (\st'->st'{_l_m_user = CantLoad}) *> done

    fetch_user :: MonadIO m => UserName -> CoreM m CoreResult
    fetch_user user_sid = do
      lr <- api $ ApiS.getUserSanitizedPack' user_sid
      rehtie lr (const cantLoad_user) $ \user@UserSanitizedPackResponse{..} -> do
        modify (\st'->st'{
            _l_m_user = Loaded $ Just user
          , _m_profileRequest = Just $ profileResponseToProfileRequest [] Nothing userSanitizedPackResponseProfile
        })
        done




    load_forum :: MonadIO m => CoreM m CoreResult
    load_forum = modify (\st'->st'{_l_m_forum = Loading}) *> next

    cantLoad_forum :: MonadIO m => CoreM m CoreResult
    cantLoad_forum = modify (\st'->st'{_l_m_forum = CantLoad}) *> done

    fetch_forum :: MonadIO m => ForumName -> CoreM m CoreResult
    fetch_forum forum_sid = do
      Store{..} <- get
      lr <- api $ getForumPack' 1
      rehtie lr (const cantLoad_forum) $ \forum_pack -> do
        let ForumPackResponse{..} = forum_pack
        modify (\st'->st'{
           _l_m_forum      = Loaded $ Just forum_pack
         , _m_forumRequest = Just $ forumResponseToForumRequest forumPackResponseForum
        })
        done




    load_boot :: MonadIO m => CoreM m CoreResult
    load_boot = modify (\st'->st'{_l_boards = Loading}) *> next

    cantLoad_boot :: MonadIO m => CoreM m CoreResult
    cantLoad_boot = modify (\st'->st'{_l_boards = CantLoad}) *> done

    fetch_boot :: MonadIO m => CoreM m CoreResult
    fetch_boot = do
      Store{..} <- get
      lr <- api $ getBoardPacks'
      liftIO $ putStrLn $ "WTF: board_packs: " <> show lr
      rehtie lr (const cantLoad_boot) $ \board_packs -> do
        let BoardPackResponses{..} = board_packs
        modify (\st'->st'{
           _l_boards      = Loaded $ idmapFrom boardPackResponseBoardId boardPackResponses
        })
        done



    load_boards_new :: MonadIO m => CoreM m CoreResult
    load_boards_new = modify (\st'->st'{_l_m_board = Loaded Nothing, _m_boardRequest = Just defaultBoardRequest}) *> next

    cantLoad_boards_new :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_new = modify (\st'->st'{_m_boardRequest = Nothing}) *> done



    load_boards :: MonadIO m => CoreM m CoreResult
    load_boards = modify (\st'->st'{_l_boards = Loading}) *> next

    cantLoad_boards :: MonadIO m => CoreM m CoreResult
    cantLoad_boards = modify (\st'->st'{_l_boards = CantLoad}) *> done




    load_board :: MonadIO m => CoreM m CoreResult
    load_board = modify (\st'->st'{_l_m_board = Loading}) *> next

    cantLoad_board :: MonadIO m => CoreM m CoreResult
    cantLoad_board = modify (\st'->st'{_l_m_board = CantLoad}) *> done

    fetch_board :: MonadIO m => BoardName -> CoreM m CoreResult
    fetch_board board_sid = do
      Store{..} <- get
      case _l_m_forum of
        Loaded (Just forum@ForumPackResponse{..}) -> do
          lr <- api $ ApiS.getBoardPack' board_sid -- forumPackResponseForumId
          rehtie lr (const cantLoad_board) $ \board_pack -> do
            let BoardPackResponse{..} = board_pack
            modify (\st'->st'{
                _l_m_board      = Loaded $ Just board_pack
              , _m_boardRequest = Just $ boardResponseToBoardRequest boardPackResponseBoard
            })
            done
        _ -> cantLoad_board






    load_threads_new :: MonadIO m => CoreM m CoreResult
    load_threads_new = modify (\st'->st'{_l_m_thread = Loaded Nothing, _m_threadRequest = Just defaultThreadRequest}) *> next

    cantLoad_threads_new :: MonadIO m => CoreM m CoreResult
    cantLoad_threads_new = modify (\st'->st'{_m_threadRequest = Nothing}) *> done



    load_threads :: MonadIO m => CoreM m CoreResult
    load_threads = modify (\st'->st'{_l_threads = Loading}) *> next

    cantLoad_threads :: MonadIO m => CoreM m CoreResult
    cantLoad_threads = modify (\st'->st'{_l_threads = CantLoad}) *> done

    fetch_threads :: MonadIO m => CoreM m CoreResult
    fetch_threads = do
      Store{..} <- get
      case (_l_m_forum, _l_m_board) of
        (Loaded (Just forum), Loaded (Just board)) -> do
          let
            ForumPackResponse{..} = forum
            BoardPackResponse{..} = board

          lr <- runEitherT $ do
            count   <- mustPassT $ api $ getThreadsCount_ByBoardId' boardPackResponseBoardId
            threads <- mustPassT $ api $ getThreadPacks_ByBoardId params_list_dsc boardPackResponseBoardId
            pure (count, threads)

          rehtie lr (const cantLoad_threads) $ \(count, threads) -> do
            let ThreadPackResponses{..} = threads
            modify (\st'->st'{
              _l_threads = Loaded $ idmapFrom threadPackResponseThreadId threadPackResponses
            , _pageInfo  = new_page_info count
            })
            -- | Merge users from threads
            --
            act_merge_users $ map threadPackResponseUser threadPackResponses
            done
        _ -> cantLoad_threads



    load_thread :: MonadIO m => CoreM m CoreResult
    load_thread = modify (\st'->st'{_l_m_thread = Loading}) *> next

    cantLoad_thread :: MonadIO m => CoreM m CoreResult
    cantLoad_thread = modify (\st'->st'{_l_m_thread = CantLoad}) *> done

    fetch_thread :: MonadIO m => ThreadName -> CoreM m CoreResult
    fetch_thread thread_sid = do
      Store{..} <- get
      case _l_m_board of
        Loaded (Just board@BoardPackResponse{..}) -> do
          lr <- api $ ApiS.getThreadPack' thread_sid -- TODO FIXME: boardPackResponseBoardId
          rehtie lr (const cantLoad_thread) $ \thread_pack -> do
            let ThreadPackResponse{..} = thread_pack
            modify (\st'->st'{
                _l_m_thread          = Loaded $ Just thread_pack
              , _m_threadRequest     = Just $ threadResponseToThreadRequest Nothing threadPackResponseThread
              , _m_threadPostRequest = Just defaultThreadPostRequest
            })
            -- | Merge user from thread
            --
            act_merge_users [threadPackResponseUser]
            done
        _ -> cantLoad_thread






    load_threadPosts_new :: MonadIO m => CoreM m CoreResult
    load_threadPosts_new = modify (\st'->st'{_l_m_threadPost = Loaded Nothing, _m_threadRequest = Just defaultThreadRequest}) *> next

    cantLoad_threadPosts_new :: MonadIO m => CoreM m CoreResult
    cantLoad_threadPosts_new = modify (\st'->st'{_m_threadPostRequest = Nothing}) *> done



    load_threadPosts :: MonadIO m => CoreM m CoreResult
    load_threadPosts = modify (\st'->st'{_l_threadPosts = Loading}) *> next

    cantLoad_threadPosts :: MonadIO m => CoreM m CoreResult
    cantLoad_threadPosts = modify (\st'->st'{_l_threadPosts = CantLoad}) *> done

    fetch_threadPosts :: MonadIO m => CoreM m CoreResult
    fetch_threadPosts = do
      Store{..} <- get
      case _l_m_thread of
        Loaded (Just thread@ThreadPackResponse{..}) -> do
          lr <- runEitherT $ do
            count <- mustPassT $ api $ getThreadPostsCount_ByThreadId' threadPackResponseThreadId
            posts <- mustPassT $ api $ getThreadPostPacks_ByThreadId params_list threadPackResponseThreadId
            pure (count, posts)
          rehtie lr (const cantLoad_threadPosts) $ \(count, posts) -> do
            let ThreadPostPackResponses{..} = posts
            modify (\st'->st'{
              _l_threadPosts = Loaded $ idmapFrom threadPostPackResponseThreadPostId threadPostPackResponses
            , _pageInfo = new_page_info count
            })
            -- | Merge users from thread posts
            --
            act_merge_users $ map threadPostPackResponseUser threadPostPackResponses
            done
        _ -> cantLoad_threadPosts



    fetch_threadPostsWith :: MonadIO m => ThreadPostId -> CoreM m CoreResult
    fetch_threadPostsWith post_id = do
      Store{..} <- get
      case _l_m_thread of
        Loaded (Just thread@ThreadPackResponse{..}) -> do
          lr <- runEitherT $ do
            count <- mustPassT $ api $ getThreadPostsCount_ByThreadId' threadPackResponseThreadId
            post  <- mustPassT $ api $ getThreadPostPack (WithThreadPosts True : params_list) post_id
            let ThreadPostPackResponse{..} = post
            posts <- (case threadPostPackResponseWithThreadPosts of
              Nothing   -> pure $ ThreadPostPackResponses []
              Just keys -> mustPassT $ api $ getThreadPostPacks_ByThreadPostsIds' (post_id : keys))
            pure (count, post, posts)
          rehtie lr (const cantLoad_threadPosts) $ \(count, post, posts) -> do
            let
              ThreadPostPackResponses{..} = posts
            modify (\st'->st'{
              _l_threadPosts = Loaded $ idmapFrom threadPostPackResponseThreadPostId threadPostPackResponses
            , _pageInfo = new_page_info count
            })
            -- | Merge users from thread posts
            --
            act_merge_users $ map threadPostPackResponseUser threadPostPackResponses
            done
        _ -> cantLoad_threadPosts


    load_threadPost :: MonadIO m => CoreM m CoreResult
    load_threadPost = modify (\st'->st'{_l_m_threadPost = Loading}) *> next

    cantLoad_threadPost :: MonadIO m => CoreM m CoreResult
    cantLoad_threadPost = modify (\st'->st'{_l_m_threadPost = CantLoad}) *> done

    fetch_threadPost :: MonadIO m => ThreadPostId -> CoreM m CoreResult
    fetch_threadPost post_id = do
      Store{..} <- get
      lr <- api $ getThreadPostPack' post_id
      rehtie lr (const cantLoad_threadPost) $ \post_pack -> do
        let ThreadPostPackResponse{..} = post_pack
        modify (\st'->st'{
           _l_m_threadPost = Loaded $ Just post_pack
         , _m_threadPostRequest = Just $ threadPostResponseToThreadPostRequest Nothing Nothing threadPostPackResponseThreadPost
        })
        -- | Merge user from thread post
        --
        act_merge_users [threadPostPackResponseUser]
        done



    load_recentThreadPosts :: MonadIO m => CoreM m CoreResult
    load_recentThreadPosts = modify (\st'->st'{_l_recentThreadPosts = Loading}) *> next

    cantLoad_recentThreadPosts :: MonadIO m => CoreM m CoreResult
    cantLoad_recentThreadPosts = modify (\st'->st'{_l_recentThreadPosts = CantLoad}) *> done

    fetch_recentThreadPosts :: MonadIO m => CoreM m CoreResult
    fetch_recentThreadPosts = do
      Store{..} <- get
      case _l_m_forum of
        Loaded (Just ForumPackResponse{..}) -> do
          let ForumResponse{..} = forumPackResponseForum
          lr <- api $ getRecentThreadPostPacks [ Limit $ fromIntegral forumResponseRecentPostsLimit
                                               , WithBoard True
                                               , WithThread True
                                               , SortOrder SortOrderBy_Dsc
                                               , Order OrderBy_CreatedAt
                                               ]
          rehtie lr (const cantLoad_recentThreadPosts) $ \ThreadPostPackResponses{..} -> do
            modify (\st'->st'{
              _l_recentThreadPosts = Loaded $ idmapFrom threadPostPackResponseThreadPostId threadPostPackResponses
            })
            done
        _ -> cantLoad_recentThreadPosts





    fetch_boards :: MonadIO m => BoardName -> CoreM m CoreResult
    fetch_boards board_sid = do
      Store{..} <- get
      case _l_m_board of
        Loading         -> fetch_board board_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        Loaded (Just _) -> done
        _               -> cantLoad_boards



    fetch_boards_new :: MonadIO m => CoreM m CoreResult
    fetch_boards_new = cantLoad_boards



    load_boards_index :: MonadIO m => CoreM m CoreResult
    load_boards_index = do
      load_boards
      next

    cantLoad_boards_index :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_index = do
      cantLoad_boards
      done

    fetch_boards_index :: MonadIO m => CoreM m CoreResult
    fetch_boards_index = do
      Store{..} <- get
      case _l_boards of
        Loading  -> fetch_boards_index >>= \core_result_ -> basedOn_ core_result_ start next next
        Loaded _ -> done
        _        -> cantLoad_boards_index



    load_boards_show :: MonadIO m => CoreM m CoreResult
    load_boards_show = do
      load_boards
      load_threads
      next

    cantLoad_boards_show :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_show = do
      cantLoad_boards
      cantLoad_threads
      done

    fetch_boards_show :: MonadIO m => BoardName -> CoreM m CoreResult
    fetch_boards_show board_sid = do
      result <- fetch_boards board_sid
      doneDo result $ do
        Store{..} <- get
        case _l_threads of
          Loading   -> fetch_threads >>= \core_result_ -> basedOn_ core_result_ start next next
          Loaded _  -> done
          _         -> cantLoad_boards_show







    load_boards_threads :: MonadIO m => CoreM m CoreResult
    load_boards_threads = do
      load_board
      load_thread
      modify (\st'->st'{_l_m_threadPost = Loaded Nothing})
      next

    cantLoad_boards_threads :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_threads = do
      cantLoad_board
      cantLoad_thread
      done

    fetch_boards_threads :: MonadIO m => BoardName -> ThreadName -> CoreM m CoreResult
    fetch_boards_threads board_sid thread_sid = do
      Store{..} <- get
      case (_l_m_board, _l_m_thread) of
        (Loading, Loading)                 -> fetch_board board_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loading)         -> fetch_thread thread_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _)) -> done
        _                                  -> cantLoad_boards_threads



    load_boards_threads_new :: MonadIO m => CoreM m CoreResult
    load_boards_threads_new = do
      load_board
      load_threads_new
      next

    cantLoad_boards_threads_new :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_threads_new = do
      cantLoad_board
      cantLoad_threads_new
      done

    fetch_boards_threads_new :: MonadIO m => BoardName -> CoreM m CoreResult
    fetch_boards_threads_new board_sid = do
      Store{..} <- get
      case _l_m_board of
        Loading         -> fetch_board board_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        Loaded (Just _) -> done
        _               -> cantLoad_boards_threads_new



    load_boards_threads_index :: MonadIO m => CoreM m CoreResult
    load_boards_threads_index = do
      load_board
      load_threads
      next

    cantLoad_boards_threads_index :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_threads_index = do
      cantLoad_board
      cantLoad_threads
      done

    fetch_boards_threads_index :: MonadIO m => BoardName -> CoreM m CoreResult
    fetch_boards_threads_index board_sid = do
      Store{..} <- get
      case (_l_m_board, _l_threads) of
        (Loading, Loading)          -> fetch_board board_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loading)  -> fetch_threads >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded _) -> done
        _                           -> cantLoad_boards_threads_index



    load_boards_threads_show :: MonadIO m => CoreM m CoreResult
    load_boards_threads_show = do
      load_boards_threads
      load_threadPosts
      next

    cantLoad_boards_threads_show :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_threads_show = do
      cantLoad_boards_threads
      cantLoad_threadPosts
      done

    fetch_boards_threads_show :: MonadIO m => BoardName -> ThreadName -> CoreM m CoreResult
    fetch_boards_threads_show board_sid thread_sid = do
      result <- fetch_boards_threads board_sid thread_sid
      doneDo result $ do
        Store{..} <- get
        case _l_threadPosts of
          Loading   -> fetch_threadPosts >>= \core_result_ -> basedOn_ core_result_ start next next
          Loaded _  -> done
          _         -> cantLoad_boards_threads_show





    load_boards_threads_posts :: MonadIO m => CoreM m CoreResult
    load_boards_threads_posts = do
      load_board
      load_thread
      load_threadPost
      next

    cantLoad_boards_threads_posts :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_threads_posts = do
      cantLoad_board
      cantLoad_thread
      cantLoad_threadPost
      done

    fetch_boards_threads_posts :: MonadIO m => BoardName -> ThreadName -> ThreadPostId -> CoreM m CoreResult
    fetch_boards_threads_posts board_sid thread_sid post_id = do
      Store{..} <- get
      case (_l_m_board, _l_m_thread, _l_m_threadPost) of
        (Loading, Loading, Loading)                         -> fetch_board board_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loading, Loading)                 -> fetch_thread thread_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loading)         -> fetch_threadPost post_id >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _), Loaded (Just _)) -> done
        _                                                   -> cantLoad_boards_threads_posts



    load_boards_threads_posts_new :: MonadIO m => CoreM m CoreResult
    load_boards_threads_posts_new = do
      load_board
      load_thread
      load_threadPosts_new
      next

    cantLoad_boards_threads_posts_new :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_threads_posts_new = do
      cantLoad_board
      cantLoad_thread
      cantLoad_threadPosts_new
      done

    fetch_boards_threads_posts_new :: MonadIO m => BoardName -> ThreadName -> CoreM m CoreResult
    fetch_boards_threads_posts_new board_sid thread_sid = do
      Store{..} <- get
      case (_l_m_board, _l_m_thread) of
        (Loading, Loading)                 -> fetch_board board_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loading)         -> fetch_thread thread_sid >>= \core_result_ -> basedOn_ core_result_ start next next
        (Loaded (Just _), Loaded (Just _)) -> done
        _                                  -> cantLoad_boards_threads_posts_new



    load_boards_threads_posts_index :: MonadIO m => CoreM m CoreResult
    load_boards_threads_posts_index = do
      load_board
      load_thread
      load_threadPosts
      next

    cantLoad_boards_threads_posts_index :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_threads_posts_index = do
      cantLoad_threadPosts
      done

    fetch_boards_threads_posts_index :: MonadIO m => BoardName -> ThreadName -> CoreM m CoreResult
    fetch_boards_threads_posts_index board_sid thread_sid = do
      result <- fetch_boards_threads_new board_sid
      doneDo result $ do
        Store{..} <- get
        case _l_m_thread of
          Loading  -> fetch_threadPosts >>= \core_result_ -> basedOn_ core_result_ start next next
          Loaded _ -> done
          _        -> cantLoad_boards_threads_index



    load_boards_threads_posts_show :: MonadIO m => CoreM m CoreResult
    load_boards_threads_posts_show = do
      load_boards_threads
      load_threadPosts
      next

    cantLoad_boards_threads_posts_show :: MonadIO m => CoreM m CoreResult
    cantLoad_boards_threads_posts_show = do
      cantLoad_boards_threads
      cantLoad_threadPosts
      done

    fetch_boards_threads_posts_show :: MonadIO m => BoardName -> ThreadName -> ThreadPostId -> CoreM m CoreResult
    fetch_boards_threads_posts_show board_sid thread_sid post_id = do
      result <- fetch_boards_threads board_sid thread_sid
      doneDo result $ do
        Store{..} <- get
        case _l_threadPosts of
          Loading  -> fetch_threadPostsWith post_id >>= \core_result_ -> basedOn_ core_result_ start next next
          Loaded _ -> done
          _        -> cantLoad_boards_threads_posts_show

      -- result <- fetch_boards_threads_posts board_sid thread_sid post_id
      -- doneDo result $ do
      --   done
        -- TODO FIXME
        -- Store{..} <- get
        -- case _l_threadPosts of
        --   Loading   -> fetch_threadPosts >>= \core_result_ -> basedOn_ core_result_ start next next
        --   Loaded _  -> done
        --   _         -> cantLoad_boards_threads_posts_show





    -- load_users_profile :: MonadIO m => CoreM m CoreResult
    -- load_users_profile = modify (\st'->st'{_l_m_profile = Loading}) *> next

    -- cantLoad_users_profile :: MonadIO m => CoreM m CoreResult
    -- cantLoad_users_profile = modify (\st'->st'{_l_m_profile = CantLoad}) *> done

    -- fetch_users_profile :: MonadIO m => UserName -> CoreM m CoreResult
    -- fetch_users_profile user_sid = do
    --   lr <- runEitherT $ do
    --     pure (count, profiles)
    --   rehtie lr (const $ cantLoad_users_profile) $ \(count, profile_packs) -> do
    --     modify (\st'->st'{
    --         _l_m_profile = Loaded $ idmapFrom profilePackResponseOrganizationId (profilePackResponses profile_packs)
    --       , _pageInfo = new_page_info count
    --       })
    --     done



    -- load_users_profile_index :: MonadIO m => CoreM m CoreResult
    -- load_users_profile_index = do
    --   load_user
    --   next

    -- fetch_users_profile_index :: MonadIO m => UserName -> CoreM m CoreResult
    -- fetch_users_profile_index user_sid = do






  act_save = do
    route_with <- gets _route
    case route_with of
      RouteWith (Boards _) _                 -> act_save_board
      RouteWith (BoardsThreads _ _) _        -> act_save_thread
      RouteWith (BoardsThreadsPosts _ _ _) _ -> act_save_threadPost
      RouteWith (UsersProfile _ _) _                                -> act_save_users_profile
      RouteWith (Users _) _                                         -> done
      _ -> done

    where

    act_save_board :: MonadIO m => CoreM m CoreResult
    act_save_board = do
      Store{..} <- get
      case _m_boardRequest of
        Just request -> do
          lr <- case _l_m_board of
                  Loaded (Just BoardPackResponse{..}) -> api $ putBoard' boardPackResponseBoardId request
                  _                                   -> api $ postBoard_ByForumId' 1 {- TODO FIXME -} request
          rehtie lr (const done) $ \BoardResponse{..} -> do
            reroute $ RouteWith (Boards (ShowS boardResponseName)) emptyParams
        _ -> done

    act_save_thread :: MonadIO m => CoreM m CoreResult
    act_save_thread = do
      Store{..} <- get
      case (_l_m_board, _m_threadRequest) of
        (Loaded (Just BoardPackResponse{..}), Just request) -> do
          lr <- case _l_m_thread of
                  Loaded (Just ThreadPackResponse{..}) -> api $ putThread' threadPackResponseThreadId request
                  _                                    -> api $ postThread_ByBoardId' boardPackResponseBoardId request
          rehtie lr (const done) $ \ThreadResponse{..} -> do
            reroute $ RouteWith (BoardsThreads (boardResponseName boardPackResponseBoard) (ShowS threadResponseName)) emptyParams
        _ -> done

    act_save_users_profile :: MonadIO m => CoreM m CoreResult
    act_save_users_profile = do
      Store{..} <- get
      case (_l_m_user, _m_profileRequest) of
        (Loaded (Just UserSanitizedPackResponse{..}), Just request) -> do
          let
            UserSanitizedResponse{..} = userSanitizedPackResponseUser
            ProfileResponse{..}       = userSanitizedPackResponseProfile

          lr <- api $ putUserProfile' profileResponseId request
          rehtie lr (const done) $ \_ -> do
            reroute $ RouteWith (UsersProfile userSanitizedResponseName Index) emptyParams

        _ ->  done

  -- Needs to be separated out
  -- because we can save thread posts from the Threads route
  --
  act_save_threadPost :: MonadIO m => CoreM m CoreResult
  act_save_threadPost = do
    Store{..} <- get
    case (_l_m_board, _l_m_thread, _m_threadPostRequest) of
      (Loaded (Just BoardPackResponse{..}), Loaded (Just ThreadPackResponse{..}), Just request) -> do
        lr <- case _l_m_threadPost of
                Loaded (Just ThreadPostPackResponse{..}) -> api $ putThreadPost' threadPostPackResponseThreadPostId request
                _                                        -> api $ postThreadPost_ByThreadId' threadPackResponseThreadId request
        rehtie lr (const done) $ \ThreadPostResponse{..} -> do
          reroute $ RouteWith (BoardsThreadsPosts (boardResponseName boardPackResponseBoard) (threadResponseName threadPackResponseThread) (ShowI threadPostResponseId)) emptyParams
      _ -> done

  -- Needs to be separated out
  -- because we can save thread posts from the Threads route
  --
  act_save_threadPost_inPlace :: MonadIO m => CoreM m CoreResult
  act_save_threadPost_inPlace = do
    Store{..} <- get
    case (_l_m_board, _l_m_thread, _m_threadPostRequest) of
      (Loaded (Just BoardPackResponse{..}), Loaded (Just ThreadPackResponse{..}), Just request) -> do
        lr <- case _l_m_threadPost of
                Loaded (Just ThreadPostPackResponse{..}) -> api $ putThreadPost' threadPostPackResponseThreadPostId request
                _                                        -> api $ postThreadPost_ByThreadId' threadPackResponseThreadId request
        rehtie lr (const done) $ \ThreadPostResponse{..} -> do
          lr' <- api $ getThreadPostPack' threadPostResponseId
          rehtie lr' (const done) $ \tpr -> do
            case _l_threadPosts of
              Loaded _threadPosts -> do
                                     modify (\st'->st'{_l_threadPosts = Loaded (Map.insert threadPostResponseId tpr _threadPosts), _m_threadPostRequest = Just defaultThreadPostRequest })
                                     done
              _                   -> done
        done
      _ -> done



  -- | Takes a list of sanitized users, and pulls down any of them that
  -- don't already exist in the current usersCache
  --
  act_merge_users users = act_merge_user_ids $ map userSanitizedResponseId users



  -- | Takes a list of user ids, and pulls down any of them that
  -- don't already exist in the current _usersCache
  --
  act_merge_user_ids user_ids = do
    Store{..} <- get
    let
      user_ids_not_in_map =
        filter (\user_id -> not $ Map.member user_id _usersCache)
        $ nub user_ids

    case user_ids_not_in_map of
      [] -> done
      xs -> do
        lr <- api $ getUserSanitizedPacks_ByUsersIds' user_ids_not_in_map
        rehtie lr (const done) $ \UserSanitizedPackResponses{..} -> do
          let new_users_map = idmapFrom userSanitizedPackResponseUserId userSanitizedPackResponses
          Store{..} <- get
          modify (\st'->st'{_usersCache = Map.union new_users_map _usersCache})
          done

  -- | Like, Neutral, Dislike, or Unlike something
  --
  act_do_like ent ent_id Nothing = do
    case ent of
      Ent_Like -> do
        api $ deleteLike' ent_id
        done
      _        -> done

  act_do_like ent ent_id (Just like_request) = do
    case ent of
      Ent_Like -> do
        api $ putLike' ent_id like_request
        done
      _        -> do
        api $ postLike ent ent_id like_request
        done



  -- unexpectedError = errorMessage "Unexpected error."

  apiError (DecodeError _)   = errorMessage "Unexpected JSON decode error."
  apiError (ServerError _ _) = errorMessage "Unexpected server error."

  errorMessage msg = setError msg

  setError msg = do
    modify (\st'@Store{..}->st'{ _errors = msg : _errors })
    done

--   clearErrors = do
--     modify (\st'->st'{ _errorText = [] })
--     done





-- TODO FIXME
-- move this somewhere else, ie, ln-api
--
postLike :: Ent -> Int64 -> LikeRequest -> ApiEff SpecificApiOptions (Either (ApiError ApplicationError) LikeResponse)
postLike ent ent_id like_request =
  case ent of
    Ent_ThreadPost -> postLikes_ByThreadPostId' ent_id like_request
    _              -> error "unsupported"
