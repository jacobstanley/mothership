{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Mothership.Application
    ( Application
    , applicationInitializer
    , MonadMothership(..)
    ) where

import Control.Monad.Reader
import System.FilePath ((</>))
import System.Directory (getCurrentDirectory, createDirectoryIfMissing)
import Snap.Auth
import Snap.Extension
import Snap.Extension.DB.MongoDB
import Snap.Extension.Heist.Impl
import Snap.Extension.Session.CookieSession
import Snap.Types (MonadSnap)

import Mothership.Types (uniqueIndexes)

------------------------------------------------------------------------------

type Application = SnapExtend ApplicationState

data ApplicationState = ApplicationState
    { mothershipState :: MothershipState
    , templateState   :: HeistState Application
    , databaseState   :: MongoDBState
    , sessionState    :: CookieSessionState
    }

------------------------------------------------------------------------------

instance HasMothershipState ApplicationState where
    getMothershipState = mothershipState

instance HasHeistState Application ApplicationState where
    getHeistState     = templateState
    setHeistState s a = a { templateState = s }

instance HasMongoDBState ApplicationState where
    getMongoDBState     = databaseState
    setMongoDBState s a = a { databaseState = s }

instance HasCookieSessionState ApplicationState where
    getCookieSessionState = sessionState

instance MonadAuth Application where
    authAuthenticationKeys = return ["username"]

------------------------------------------------------------------------------

applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    mship    <- mothershipInitializer
    heist    <- heistInitializer "resources/templates"
    session  <- cookieSessionStateInitializer $ defCookieSessionState
                { csKeyPath    = "config/site.key"
                , csCookieName = "mothership-session" }

    database <- mongoDBInitializer (host "127.0.0.1") 1 "mothership"
    ensureIndexes database

    return $ ApplicationState mship heist database session

------------------------------------------------------------------------------
-- MongoDB intialization

ensureIndexes :: MongoDBState -> Initializer ()
ensureIndexes db = initDB db $ mapM_ (ensureIndex . unique) uniqueIndexes
  where
    unique :: (Collection, Label) -> Index
    unique (c, l) = (index c [l =: (1 :: Int)])
                  { iUnique = True, iDropDups = True }

-- Would be nice for something like this to be in snap-extension-mongodb
initDB :: MongoDBState -> ReaderT Database (Action IO) a -> Initializer a
initDB (MongoDBState pool db) run = do
    x <- liftIO . access safe Master pool $ use db run
    either (error . show) return x

------------------------------------------------------------------------------
-- Mothership extension

class HasMothershipState s where
    getMothershipState :: s -> MothershipState

class MonadSnap m => MonadMothership m where
    getRepositoriesDir :: m FilePath

data MothershipState = MothershipState
    { _repositoriesDir :: FilePath
    }

instance HasMothershipState s => MonadMothership (SnapExtend s) where
    getRepositoriesDir = fmap _repositoriesDir $ asks getMothershipState

instance (MonadSnap m, HasMothershipState s) => MonadMothership (ReaderT s m) where
    getRepositoriesDir = fmap _repositoriesDir $ asks getMothershipState

instance InitializerState MothershipState where
    extensionId = const "Mothership"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

mothershipInitializer :: Initializer MothershipState
mothershipInitializer = do
    dir <- liftIO getCurrentDirectory
    let repos  = dir </> "repositories"
        config = dir </> "config"
    liftIO $ createDirectoryIfMissing False repos
    liftIO $ createDirectoryIfMissing False config
    mkInitializer $ MothershipState repos
