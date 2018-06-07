{-# LANGUAGE CPP, MultiParamTypeClasses, ScopedTypeVariables, OverloadedStrings #-}
module Pure.Router
  ( Route
  , subpath, path
  , getRawUrl, setPath, getPath, getParam, getParams, setParam
  , dispatch, keep, reroute
  , route
  , Router(..)
  , goto, lref
  , currentRoute
  , onRoute, onRoute'
  , CurrentRoute
  ) where

-- from pure-router (local)
import Pure.Router.Internal

-- from pure-core
import Pure.Data.View hiding (On)
import Pure.Data.View.Patterns

-- from pure-default
import Pure.Data.Default

-- from pure-events
import Pure.Data.Events

-- from pure-html
import Pure.Data.HTML.Properties

-- from pure-lifted
import Pure.Data.Lifted

-- from pure-txt
import Pure.Data.Txt
import qualified Pure.Data.Txt as Txt

-- from excelsior
import Excelsior

-- from base
import Control.Monad
import Data.IORef
import Data.Monoid
import Data.Typeable
import System.IO.Unsafe

data Router route = Router
  { initialRoute :: route
  , router :: Txt -> IO (Maybe route)
  }

data RouteCommand r
  = RouteChange Txt

instance Typeable r => Command (CurrentRoute r) (RouteCommand r)

data CurrentRoute route = CurrentRoute route

instance Typeable route => Pure (Router route) where
  view = ComponentIO $ \self ->
    let
        runRouter = do
            pn  <- getPathname
            qps <- getSearch
            command (RouteChange (pn <> qps) :: RouteCommand route)

    in
        def
            { construct = do
                -- (d,w) <- (,) <$> getDocument <*> getWindow
                -- onRaw (toNode d) "load"     def $ \stop _ -> do
                --   setPopped >> stop
                w <- getWindow
                onRaw (toNode w) "popstate" def $ \_    _ -> do
                  -- print =<< getPopped
                  -- getPopped >>= flip when runRouter
                  runRouter
            , executing = runRouter
            , unmount = join (getState self)
            , render = \rtr _ ->
                View $ Excelsior (CurrentRoute $ initialRoute rtr) [] [ middleware (mw rtr) ]
            }
    where
      mw :: Router route -> Middleware (CurrentRoute route) (RouteCommand route)
      mw rtr next (RouteChange path) rt = do
        mrt <- router rtr path
        case mrt of
          Just rt -> return (CurrentRoute rt)
          _       -> return rt

-- {-# NOINLINE popped #-}
-- popped = unsafePerformIO $ newIORef False
-- setPopped = writeIORef popped True
-- getPopped = readIORef popped

onRoute :: Typeable route => (route -> IO ()) -> IO (Maybe (Excelsior.Callback (CurrentRoute route)))
onRoute f = watch (\(CurrentRoute r) -> f r)

onRoute' :: Typeable route => (route -> IO ()) -> IO (Maybe (Excelsior.Callback (CurrentRoute route)))
onRoute' f = watch' (\(CurrentRoute r) -> f r)

lref :: HasFeatures a => Txt -> a -> a
lref t a = Listener (intercept (On "click" (\_ -> goto t))) (Href t a)

goto :: Txt -> IO ()
goto rt = do
  -- setPopped
  pushState rt
  popState

pushPath :: Txt -> IO ()
pushPath pth = do
  -- setPopped
  pushState pth
#ifndef __GHCJS__
  let (pathname,search) = Txt.span (/= '?') pth
  writeIORef pathname_ pathname
  writeIORef search_ search
#endif

currentRoute :: Typeable route => IO (Maybe route)
currentRoute = do
  mr <- lookupState
  case mr of
    Just (CurrentRoute rt) -> return (Just rt)
    _                      -> return Nothing

-- Not yet exposing these or accessors.

{-# NOINLINE pathname_ #-}
pathname_ :: IORef Txt
pathname_ = unsafePerformIO (newIORef mempty)

{-# NOINLINE search_ #-}
search_ :: IORef Txt
search_ = unsafePerformIO (newIORef mempty)
