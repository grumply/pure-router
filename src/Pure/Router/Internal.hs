{-# LANGUAGE ViewPatterns, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module Pure.Router.Internal
  ( Routing(..), RoutingState(..)
  , getOriginalUrl, getOriginalPath, getOriginalParams
  , getPath, getParams
  , tryParam, param, path, continue, dispatch
  , route, route'
  ) where

-- from pure-txt
import Pure.Data.Txt (Txt,ToTxt(..),FromTxt(..))
import qualified Pure.Data.Txt as Txt

-- from pure-uri
import Pure.Data.URI

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fail
import Data.String

import Control.Monad.State  as St
import Control.Monad.Except as E
import Control.Monad.IO.Class

import qualified Data.Map as Map


--------------------------------------------------------------------------------
-- Routing DSL Type
-- 
-- The continuation monad allows short-circuiting with a route result.
-- The state monad allows delimiting route blocks.
-- The reader monad allows global access to the root route and params.
--
-- Paths are not guaranteed to be valid URI path segments because percent 
-- decoding is applied.

data RoutingState rt = RoutingState
  { _url      :: Txt
  , _path     :: Txt
  , _params   :: Map.Map Txt Txt
  }

newtype Routing rt a = MkRouting 
  { unRouting :: ExceptT (Maybe rt) (StateT (RoutingState rt) IO) a 
  } deriving (Functor,Applicative)

instance FromTxt a => IsString (Routing rt a) where
  fromString = param . toTxt

instance MonadIO (Routing rt) where
  liftIO = MkRouting . liftIO

instance MonadFail (Routing rt) where
  fail _ = empty

instance Monad (Routing rt) where
  return = MkRouting . return
  (>>=) ma amb = MkRouting $ unRouting ma >>= unRouting . amb
  fail _ = MkRouting (throwError Nothing)

instance Alternative (Routing rt) where
  empty = MkRouting (throwError Nothing)
  (<|>) rl rr = do
    st@(RoutingState url path params) <- MkRouting St.get 
    lr <- liftIO $ evalStateT (runExceptT (unRouting rl)) st
    case lr of
      Left (Just rt) -> MkRouting $ throwError (Just rt)
      Left Nothing   -> rr
      Right a        -> return a

instance MonadPlus (Routing rt) where
  mzero = empty
  -- dubious
  mplus rl rr = do
    st@(RoutingState url path params) <- MkRouting St.get 
    lr <- liftIO $ evalStateT (runExceptT (unRouting rl)) st
    case lr of
      Left (Just rt) -> MkRouting $ throwError (Just rt)
      _ -> rr


--------------------------------------------------------------------------------
-- API

getOriginalUrl :: Routing rt Txt
getOriginalUrl = do
  RoutingState url _ _<- MkRouting St.get
  pure url

getOriginalPath :: Routing rt Txt
getOriginalPath = do
  RoutingState url _ _ <- MkRouting St.get
  let (p,_) = breakRoute url
  pure p

getOriginalParams :: Routing rt (Map.Map Txt Txt)
getOriginalParams = do
  RoutingState url _ _ <- MkRouting St.get
  let (_,ps) = breakRoute url
  pure (Map.fromList ps)

getPath :: Routing rt Txt
getPath = do
  RoutingState _ path _ <- MkRouting St.get
  pure path

getParams :: Routing rt (Map.Map Txt Txt)
getParams = do
  RoutingState _ _ params <- MkRouting St.get
  pure params

tryParam :: FromTxt a => Txt -> Routing rt (Maybe a)
tryParam p = do
  ps <- getParams
  pure ( fmap fromTxt $ Map.lookup p ps )

param :: FromTxt a => Txt -> Routing rt a
param p = do
  ps <- getParams
  case Map.lookup p ps of
    Nothing -> continue
    Just a  -> pure ( fromTxt a )

path :: Txt -> Routing rt a -> Routing rt (Maybe a)
path stncl rt = do
  st@(RoutingState url path params) <- MkRouting St.get 
  case stencil stncl path of
    Nothing -> return Nothing
    Just (sub,ps) -> do
      let newRS = RoutingState url sub (Map.union (Map.fromList ps) params)
      lr <- liftIO $ evalStateT (runExceptT (unRouting rt)) newRS
      case lr of
        Left (Just rt) -> MkRouting $ throwError (Just rt)
        Left Nothing   -> return Nothing
        Right a        -> return (Just a)

continue :: Routing rt a
continue = MkRouting (throwError Nothing)

dispatch :: rt -> Routing rt a
dispatch rt = MkRouting $ throwError (Just rt)


--------------------------------------------------------------------------------
-- DSL executor

route' :: rt -> Routing rt a -> Txt -> IO rt
route' def rt url = do
  let (path,params) = breakRoute url
  fmap (either (maybe def id) id) $ (`evalStateT` (RoutingState url path (Map.fromList params))) $ runExceptT $ 
    unRouting (rt >> return def)

route :: Routing rt a -> Txt -> IO (Maybe rt)
route rt url = do
  let (path,params) = breakRoute url
  fmap (either id id) $ (`evalStateT` (RoutingState url path (Map.fromList params))) $ runExceptT $ 
    unRouting (rt >> return Nothing)


--------------------------------------------------------------------------------
-- Utils

-- | Normalize path, decode as uri, and extract query parameters.
--
-- prop> breakRoute "/has%20space"
-- ("/has space",[])
--
-- prop> breakRoute "/a?p=b"
-- ("/a",[("p","b")])
--
-- prop> breakRoute "/a?p1=b&p2=c"
-- ("/a",[("p1","b"),("p2","c")])
--
breakRoute :: Txt -> (Txt,[(Txt,Txt)])
breakRoute (decodeURI . Txt.takeWhile (/= '#') -> uri) =
  let (path,params0) = Txt.span (/= '?') uri
      params =
        case Txt.uncons params0 of
          Just ('?',qps) ->
            fmap (second safeTail) $
            fmap (Txt.breakOn "=")
                     (Txt.splitOn "&" qps)
          _ -> []
      safeTail x =
        case Txt.uncons x of
          Just (_,rest) -> rest
          _ -> ""
  in (path,params)

-- | Our core matcher pairs the given stencil against a given path segment.
--
-- prop> stencil "/:param" "/test"
-- Just ("",[("param","test")])
--
-- prop> stencil "/:param/a" "/test/a"
-- Just ("",[("param","test")])
--
-- prop> stencil "/a/:param/b" "/a/test/b"
-- Just ("",[("param","test")])
--
-- prop> stencil "/a" "/a/b"
-- Just ("/b",[])
--
-- prop> stencil "/a/:param" "/a/b/c"
-- Just ("/c",[("param","b")])
--
-- prop> stencil "/a" "/b"
-- Nothing
--
-- prop> stencil "/a" "/a/"
-- Just ("",[])
--
stencil :: Txt -> Txt -> Maybe (Txt,[(Txt,Txt)])
stencil = withAcc []
  where
    withAcc acc = go
      where
        go x y =
          -- the DSL doesn't have a method of handling trailing slashes, so they are ignored
          if Txt.null x && (Txt.null y || Txt.null (Txt.dropWhileEnd (== '/') y)) then
            Just (x,acc)
          else
            case (Txt.uncons x,Txt.uncons y) of
              (Just ('/',ps),Just ('/',cs)) -> do
                let
                  (p, ps') = Txt.break (== '/') ps
                  (c_,cs') = Txt.break (== '/') cs
                  c  = decodeURIComponent c_
                case Txt.uncons p of
                  Just (':',pat) -> withAcc ((pat,c):acc) ps' cs'

                  _ -> if p == c
                       then go ps' cs'
                       else Nothing

              (Nothing,_) -> Just (y,acc)

              _ -> Nothing


