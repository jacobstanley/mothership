{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Mothership.Types
    ( Bson (..)

    , Entity
    , insert
    , findAll

    , User (..)
    , Repository (..)

    , uniqueIndexes
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Maybe (mapMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (span, lookup)
import           Snap.Extension.DB.MongoDB hiding (insert)
import qualified Snap.Extension.DB.MongoDB as DB

------------------------------------------------------------------------

class Bson a where
    toDoc   :: a -> Document
    fromDoc :: Document -> Maybe a

class Bson a => Entity a where
    collection :: a -> Collection

instance Val Text where
    val = val . T.unpack
    cast' = fmap T.pack . cast'

insert :: (MonadMongoDB m, Entity a) => a -> m ()
insert x = withDB' $ DB.insert_ (collection x) (toDoc x)

findAll :: forall m a. (MonadMongoDB m, Entity a) => m [a]
findAll = withDB' $ do
    docs <- find (select [] col) >>= rest
    return $ mapMaybe fromDoc docs
  where
    col = collection (undefined :: a)

------------------------------------------------------------------------

data User = User
   { userUsername :: Text
   , userFullName :: Text
   } deriving (Show)

instance Bson User where
    toDoc x = [ "username"  =: userUsername x
              , "full_name" =: userFullName x ]
    fromDoc x = User <$> lookup "username" x
                     <*> lookup "full_name" x

------------------------------------------------------------------------

data Repository = Repository
   { repoName        :: Text
   , repoDescription :: Text
   } deriving (Show)

instance Bson Repository where
    toDoc x = [ "name"        =: repoName x
              , "description" =: repoDescription x]
    fromDoc x = Repository <$> lookup "name" x
                           <*> lookup "description" x

instance Entity Repository where
    collection = const "repositories"

------------------------------------------------------------------------

uniqueIndexes :: [(Collection, Label)]
uniqueIndexes = [ ("users", "username")
                , ("repositories", "name") ]
