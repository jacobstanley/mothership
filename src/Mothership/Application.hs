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
import System.Directory (getCurrentDirectory)
import Snap.Extension
import Snap.Extension.Heist.Impl
import Snap.Types (MonadSnap)

------------------------------------------------------------------------------

type Application = SnapExtend ApplicationState

data ApplicationState = ApplicationState
    { templateState :: HeistState Application
    , mothershipState :: MothershipState
    }

------------------------------------------------------------------------------

instance HasHeistState Application ApplicationState where
    getHeistState     = templateState
    setHeistState s a = a { templateState = s }

------------------------------------------------------------------------------

instance HasMothershipState ApplicationState where
    getMothershipState = mothershipState

------------------------------------------------------------------------------

applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    heist <- heistInitializer "resources/templates" 
    mship <- mothershipInitializer
    return $ ApplicationState heist mship

------------------------------------------------------------------------------
-- Mothership extension

class HasMothershipState s where
    getMothershipState :: s -> MothershipState

class MonadSnap m => MonadMothership m where
    getRepoDir :: m FilePath

data MothershipState = MothershipState
    { _repoDir :: FilePath
    }

instance HasMothershipState s => MonadMothership (SnapExtend s) where
    getRepoDir = fmap _repoDir $ asks getMothershipState

instance (MonadSnap m, HasMothershipState s) => MonadMothership (ReaderT s m) where
    getRepoDir = fmap _repoDir $ asks getMothershipState

instance InitializerState MothershipState where
    extensionId = const "Mothership"
    mkCleanup   = const $ return ()
    mkReload    = const $ return ()

mothershipInitializer :: Initializer MothershipState
mothershipInitializer = do
    dir <- liftIO getCurrentDirectory
    mkInitializer $ MothershipState $ dir </> "repositories"
