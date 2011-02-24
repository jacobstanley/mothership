{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module GitSnap.Application
    ( Application
    , applicationInitializer
    ) where

import Snap.Extension
import Snap.Extension.Heist.Impl

------------------------------------------------------------------------------

type Application = SnapExtend ApplicationState

data ApplicationState = ApplicationState
    { templateState :: HeistState Application
    }

------------------------------------------------------------------------------

instance HasHeistState Application ApplicationState where
    getHeistState     = templateState
    setHeistState s a = a { templateState = s }

------------------------------------------------------------------------------

applicationInitializer :: Initializer ApplicationState
applicationInitializer = do
    heist <- heistInitializer "resources/templates" 
    return $ ApplicationState heist
