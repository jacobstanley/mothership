{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}

module Mothership.Types
    ( Bson (..)
    , User (..)
    , Repository (..)
    , uniqueIndexes
    ) where

import           Control.Applicative ((<$>), (<*>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Prelude hiding (span, lookup)
import           Snap.Extension.DB.MongoDB

------------------------------------------------------------------------

class Bson a where
    toDoc   :: a -> Document
    fromDoc :: Document -> Maybe a

instance Val Text where
    val = val . T.unpack
    cast' = fmap T.pack . cast'

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

------------------------------------------------------------------------

uniqueIndexes :: [(Collection, Label)]
uniqueIndexes = [ ("users", "username")
                , ("repositories", "name") ]
