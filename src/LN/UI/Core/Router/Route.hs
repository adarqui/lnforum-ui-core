{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module LN.UI.Core.Router.Route (
  RouteWith (..),
  routeWith,
  routeWith',
  fromRouteWith,
  fromRouteWithHash,
  toRouteWith,
  toRouteWithHash,
  Route (..),
  HasLinkName,
  linkName,
  HasCrumb,
  crumb
) where



import           Control.Applicative          ((*>), (<$), (<$>), (<*>), (<|>))
import           Control.DeepSeq              (NFData)
import           Data.ByteString.Char8        (ByteString)
import qualified Data.ByteString.Char8        as BSC
import           Data.Either                  (rights)
import           Data.Either                  (Either (..))
import           Data.List                    (scanl)
import qualified Data.Map                     as Map
import           Data.Maybe                   (Maybe (Just))
import           Data.Monoid                  (mempty, (<>))
import           Data.Text                    (Text)
import           Haskell.Api.Helpers.Shared   (qp)
import           Prelude                      (Eq, Int, Show, fmap, map, pure,
                                               ($), (.), (==), (>>=))
import           Text.Parsec.Prim             (try, (<?>))
import           Web.Routes

import           LN.T
import           LN.UI.Core.Helpers.DataList  (tailFriendly)
import           LN.UI.Core.Helpers.DataText  (tshow)
import           LN.UI.Core.Helpers.GHCJS     (JSString, textToJSString')
import           LN.UI.Core.Helpers.WebRoutes (notCRUD, notCRUDstr1, str1)
import           LN.UI.Core.Router.CRUD       (CRUD (..))
import           LN.UI.Core.Router.Crumb      (HasCrumb, crumb)
import           LN.UI.Core.Router.LinkName   (HasLinkName, linkName)
import           LN.UI.Core.Router.Param      (Params, buildParams,
                                               fromWebRoutesParams)



data RouteWith
  = RouteWith Route Params
  deriving (Eq, Show, Generic, NFData)



routeWith :: Route -> [(ParamTag, Param)] -> RouteWith
routeWith route params = RouteWith route (buildParams params)



routeWith' :: Route -> RouteWith
routeWith' route = routeWith route mempty



fromRouteWith :: RouteWith -> Text
fromRouteWith (RouteWith route params) =
  toPathInfoParams route params'
  where
  params' = map (fmap Just . qp) $ Map.elems params



fromRouteWithHash :: RouteWith -> JSString
fromRouteWithHash = textToJSString' . ("#" <>) <$> fromRouteWith



toRouteWith :: ByteString -> RouteWith
toRouteWith url =
  case (fromPathInfoParams url) of
    Left _               -> routeWith' NotFound
    Right (url_, params) -> routeWith url_ $ fromWebRoutesParams params



toRouteWithHash :: ByteString -> RouteWith
toRouteWithHash = toRouteWith . BSC.drop 1



data Route
  = Home
  | About
  | Me
  | Errors
  | Portal
  | Boards CRUD
  | BoardsThreads Text CRUD
  | BoardsThreadsPosts Text Text CRUD
  | Users CRUD
  | UsersProfile Text CRUD
  | UsersSettings Text
  | UsersThreads Text
  | UsersThreadPosts Text
  | UsersWorkouts Text
  | UsersLikes Text
  | Login
  | Logout
  | NotFound
  | Experiments Text
  | FixMe
  deriving (Eq, Show, Generic, NFData)



instance HasLinkName Route where
  linkName route = case route of
    Home                            -> "Home"
    About                           -> "About"
    Portal                          -> "Portal"
    Boards Index               -> "Boards"
    Boards New                 -> "New"
    Boards   (ShowS board_sid)   -> board_sid
    Boards   (EditS board_sid)   -> board_sid
    Boards   (DeleteS board_sid) -> board_sid
    BoardsThreads  _ Index               -> "Threads"
    BoardsThreads  _  New                 -> "New"
    BoardsThreads  _  (ShowS thread_sid)   -> thread_sid
    BoardsThreads  _  (EditS thread_sid)   -> thread_sid
    BoardsThreads  _  (DeleteS thread_sid) -> thread_sid
    BoardsThreadsPosts  _ _  Index             -> "Posts"
    BoardsThreadsPosts  _ _ New               -> "New"
    BoardsThreadsPosts  _ _ (ShowI post_id)   -> tshow post_id
    BoardsThreadsPosts  _ _ (EditI post_id)   -> tshow post_id
    BoardsThreadsPosts  _ _ (DeleteI post_id) -> tshow post_id
    Users Index                     -> "Users"
    Users (ShowS user_sid)          -> user_sid
    Users (EditS user_sid)          -> user_sid
    Users (DeleteS user_sid)        -> user_sid
    UsersProfile _ Index            -> "Profile"
    UsersProfile _ EditZ            -> "Edit Profile"
    Login                           -> "Login"
    Logout                          -> "Logout"
    Experiments experiment_sid      -> experiment_sid
    _                               -> "Unknown"



instance HasLinkName RouteWith where
  linkName (RouteWith route _) = linkName route



instance HasCrumb Route where

  crumb route = maybe_organizations_index <> routes
    where
    segments                   = toPathSegments route
    segment_buckets            = tailFriendly $ scanl (\acc x -> acc <> [x]) [] segments
    routes                     = rights $ map (parseSegments fromPathSegments) segment_buckets
    maybe_organizations_index =
      case routes of
        (r:_) -> case r of
          Boards _                 -> []
          BoardsThreads _ _        -> []
          BoardsThreadsPosts _ _ _ -> []
          _                                               -> []

        _     -> []



instance PathInfo Route where

  toPathSegments route = case route of
    Home                     -> pure ""
    About                    -> pure "about"
    Me                       -> pure "me"
    Errors                   -> pure "errors"
    Portal                   -> pure "portal"
    Boards  Index -> mempty
    Boards  crud -> toPathSegments crud
    BoardsThreads  board_sid Index -> pure board_sid
    BoardsThreads  board_sid crud -> pure board_sid <> toPathSegments crud
    BoardsThreadsPosts  board_sid thread_sid Index -> pure board_sid <> pure thread_sid
    BoardsThreadsPosts  board_sid thread_sid crud -> pure board_sid <> pure thread_sid <> toPathSegments crud
    Users Index                -> pure "users"
    Users crud                 -> (pure "users") <> toPathSegments crud
    UsersProfile user_sid Index -> (pure "users") <> (pure user_sid) <> (pure "profile")
    UsersProfile user_sid crud  -> (pure "users") <> (pure user_sid) <> (pure "profile") <> toPathSegments crud
    Experiments experiment_sid -> pure "experiments" <> pure experiment_sid
    _                          -> pure ""

  fromPathSegments =
        (About         <$ segment "about"
    <|> Me            <$ segment "me"
    <|> Errors        <$ segment "errors"
    <|> Portal        <$ segment "portal"
    <|> Experiments   <$ segment "experiments" <*> fromPathSegments
    <|> (try
            (UsersProfile  <$ segment "users" <*> fromPathSegments <*> (segment "profile" *> fromPathSegments))
            <|>
            (Users         <$ segment "users" <*> fromPathSegments))

    -- welcome to the inferno
    --
    -- This is what you call, the definition of a massive hackjob.
    --
    -- It's nearly 7 AM and I still can't figure this out..
    --
    <|> (do
           board_sid <- notCRUDstr1 -- Boards
           (do
            thread_sid <- notCRUDstr1 -- BoardsThreads
            fromPathSegments >>= \k ->
              if k == Index
                then (BoardsThreads <$> pure board_sid <*> pure (ShowS thread_sid))
                else (BoardsThreadsPosts <$> pure board_sid <*> pure thread_sid <*> pure k)))
                          -- <|> (fromPathSegments >>= \k -> if k == Index then (Boards <$> pure (ShowS board_sid)) else (BoardsThreads <$> pure board_sid <*> pure k)))
    <?> "Route: fromPathSegments failed"
    -- TODO FIXME: Can't do Home <$ segment "" because it fails to pattern match. Though, pure Index works because it's terminal.
    )
