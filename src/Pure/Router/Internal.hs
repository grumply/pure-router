{-# LANGUAGE CPP, GADTs, ScopedTypeVariables, ViewPatterns, TypeSynonymInstances, FlexibleInstances, OverloadedStrings #-}
module Pure.Router.Internal where

-- from ef
import Ef

-- from pure-txt
import Pure.Data.Txt (Txt,ToTxt(..),FromTxt(..))
import qualified Pure.Data.Txt as Txt

-- from pure-uri
import Pure.Data.URI

-- from base
import Data.Bifunctor
import Data.Function
import Data.List as List
import Data.Maybe
import Data.Proxy
import Data.String
import Unsafe.Coerce

data Route k where
    GetPath
      :: (Txt -> k)
      -> Route k

    SetPath
      :: Txt
      -> k
      -> Route k

    GetRawUrl
      :: (Txt -> k)
      -> Route k

    GetParams
      :: ([(Txt,Txt)] -> k)
      -> Route k

    GetParam
      :: Txt
      -> (Maybe Txt -> k)
      -> Route k

    SetParam
      :: Txt
      -> Txt
      -> k
      -> Route k

    Reroute
      :: Routing a
      -> Route k

    Subpath
      :: Txt
      -> Routing a
      -> k
      -> Route k

    Path
      :: Txt
      -> Routing a
      -> k
      -> Route k

    Route
      :: a
      -> Route k

    Keep
      :: Route k

type Routing = Narrative Route IO

instance Functor Route where
  fmap f (GetPath jk) = GetPath (fmap f jk)
  fmap f (SetPath j k) = SetPath j (f k)
  fmap f (GetRawUrl jk) = GetRawUrl (fmap f jk)
  fmap f (GetParams jjsk) = GetParams (fmap f jjsk)
  fmap f (GetParam j mjk) = GetParam j (fmap f mjk)
  fmap f (SetParam j j' k) = SetParam j j' (f k)
  fmap f (Reroute c) = Reroute c
  fmap f (Subpath j c k) = Subpath j c (f k)
  fmap f (Path j c k) = Path j c (f k)
  fmap f (Route a) = Route a
  fmap f Keep = Keep

instance FromTxt a => IsString (Routing a) where
  fromString = getParamOrKeep . fromString
    where
      getParamOrKeep :: (FromTxt a) => Txt -> Routing a
      getParamOrKeep p = do
        mp <- getParam p
        case mp of
          Nothing -> keep
          Just p -> return (fromTxt p)

getRawUrl :: Routing Txt
getRawUrl = send (GetRawUrl id)

setPath :: Txt -> Routing ()
setPath url = send (SetPath url ())

getPath :: Routing Txt
getPath = send (GetPath id)

getParams :: Routing [(Txt,Txt)]
getParams = send (GetParams id)

setParam :: Txt -> Txt -> Routing ()
setParam p v = send (SetParam p v ())

getParam :: Txt -> Routing (Maybe Txt)
getParam p = send (GetParam p id)

subpath :: Txt -> Routing a -> Routing ()
subpath match handler = send (Subpath match handler ())

path :: Txt -> Routing a -> Routing ()
path stencil handler = send (Path stencil handler ())

dispatch :: a -> Routing a
dispatch a = send (Route a)

keep :: Routing a
keep = send Keep

reroute :: Routing a -> Routing b
reroute rtr = send (Reroute rtr)

stripTrailingSlashes = Txt.dropWhileEnd (== '/')

breakRoute url =
  let (path,params0) = Txt.span (/= '?') (stripTrailingSlashes url)
      params =
        case Txt.uncons params0 of
          Just ('?',qps) ->
            List.map (second safeTail) $
            List.map (Txt.breakOn "=")
                     (Txt.splitOn "&" qps)
          _ -> []
      safeTail x =
        case Txt.uncons x of
          Just (_,rest) -> rest
          _ -> ""
  in (stripTrailingSlashes $ Txt.takeWhile (/= '#') path,params)

route :: Routing a -> Txt -> IO (Maybe a)
route rtr url0@(breakRoute -> (path,params)) =
  withUrl path params rtr
  where

      withUrl :: forall b. Txt -> [(Txt,Txt)] -> Routing b -> IO (Maybe b)
      withUrl url params = go
          where

              go :: forall x. Routing x -> IO (Maybe x)
              go (Return _) = return Nothing
              go (Lift sup) = sup >>= go
              go (Do msg) =
                case msg of
                  GetRawUrl sk -> go (sk url0)
                  GetPath sk -> go (sk url)
                  SetPath nr k -> withUrl nr params k
                  GetParams psk -> go $ psk params
                  GetParam p mvk -> go $ mvk (List.lookup p params)
                  SetParam p v k -> withUrl url (nubBy ((==) `on` fst) ((p,v):params)) k
                  Subpath section more k -> do
                    espv <- liftIO $ match section url
                    case espv of
                      Just (Left subpath) -> do
                        res <- withUrl subpath params $ unsafeCoerce more
                        case res of
                          Nothing -> go k
                          Just n -> return (Just n)
                      Just (Right ((p,v),subpath)) -> do
                        res <- withUrl subpath (nubBy ((==) `on` fst) ((p,v):params)) (unsafeCoerce more)
                        case res of
                          Nothing -> go k
                          Just n -> return (Just n)
                      Nothing ->
                        go k
                  Path pttrn more k -> do
                    mps <- liftIO $ stencil pttrn url
                    case mps of
                      Just ps -> do
                        res <- withUrl Txt.empty (nubBy ((==) `on` fst) (ps ++ params)) (unsafeCoerce more)
                        case res of
                          Nothing -> go k
                          Just n -> return (Just n)
                      Nothing -> go k
                  Reroute rtr' ->
                    route (unsafeCoerce rtr') url0
                  Route a ->
                    return (Just $ unsafeCoerce a)
                  Keep ->
                    return Nothing


      match x y  =
        case (Txt.uncons x,Txt.uncons y) of
          (Just (':',param),Just ('/',path)) -> do
            let (valueEnc,path') = Txt.break (== '/') path
                value = decodeURI valueEnc
            return $ Just $ Right ((param,value),path')

          (Just matchPath,Just ('/',path)) -> do
            let (subpathEnc,rest) = Txt.splitAt (Txt.length x) path
                subpath = decodeURI subpathEnc
            return $
              if subpath == x then
                if Txt.null rest then
                  Just $ Left rest
                else
                  case Txt.uncons rest of
                    Just ('/',_) -> Just $ Left rest
                    _            -> Nothing
              else
                Nothing

          _ -> return Nothing


      stencil = withAcc []
        where

          withAcc acc = go
            where

              go x y =
                if Txt.null x && Txt.null y then
                  return $ Just acc
                else
                  case (Txt.uncons x,Txt.uncons y) of
                    (Just ('/',ps),Just ('/',cs)) -> do
                      let
                        (p, ps') = Txt.break (== '/') ps
                        (c_,cs') = Txt.break (== '/') cs
                        c  = decodeURI c_
                      case Txt.uncons p of
                        Just (':',pat) -> withAcc ((pat,c):acc) ps' cs'

                        _ -> if p == c
                             then go ps' cs'
                             else return Nothing

                    (Nothing,Nothing) -> return $ Just acc

                    _ -> return Nothing

