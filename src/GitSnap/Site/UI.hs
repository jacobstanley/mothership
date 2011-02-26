{-# LANGUAGE OverloadedStrings #-}

module GitSnap.Site.UI
    ( serveUI
    ) where

import           Control.Monad (forM_)
import           Control.Monad.Trans (MonadIO, liftIO)
import qualified Data.Text as T
import           Data.Text (Text)
import           Snap.Extension.Heist
import           Snap.Types hiding (path, dir)
import           System.FilePath
import           System.Directory
import           Text.Blaze.Html5 hiding (map, head)
import           Text.Blaze.Html5.Attributes hiding (title, hidden, dir, span)
import           Text.Blaze.Renderer.XmlHtml (renderHtml)
import           Text.Templating.Heist
import qualified Text.XmlHtml as X
import           Prelude hiding (span)

import           GitSnap.Application

------------------------------------------------------------------------

serveUI :: FilePath -> Application ()
serveUI repoDir = route
    [ ("/", index repoDir)
    ]

------------------------------------------------------------------------

index :: FilePath -> Application ()
index repoDir = do
    users <- getUsers repoDir
    ifTop $ heistLocal (bindSplices $ splices users) $ render "index"
  where
    splices rs = [ ("repositories", repoSplice rs) ]

repoSplice :: [User] -> Splice Application
repoSplice = htmlSplice . users
  where
    users = ul . mapM_ user
    user (User n rs) = forM_ rs $ \r -> li $ do
        a ! href (textValue $ T.concat [n,"/",r]) $ do
            text n
            text "/"
            span ! class_ "repo-name" $ text r

htmlSplice :: Html -> Splice Application
htmlSplice = return . docContent . renderHtml

docContent :: X.Document -> [X.Node]
docContent (X.HtmlDocument _ _ ns) = ns
docContent (X.XmlDocument  _ _ ns) = ns

------------------------------------------------------------------------

data User = User
    { _name  :: Text
    , _repos :: [Text]
    }

getUsers :: MonadIO m => FilePath -> m [User]
getUsers repoDir = do
    names <- getDirectoryContents' repoDir
    repos <- mapM getRepoNames $ map (repoDir </>) names
    let namesT = map T.pack names
        reposT = map (map T.pack) repos
    return $ zipWith User namesT reposT
  where
    getRepoNames :: MonadIO m => FilePath -> m [String]
    getRepoNames dir = do
        xs <- getDirectoryContents' dir
        return $ map takeBaseName $ filter dotGit xs

    dotGit :: FilePath -> Bool
    dotGit = (== ".git") . takeExtension

getDirectoryContents' :: MonadIO m => FilePath -> m [FilePath]
getDirectoryContents' dir = do
    contents <- liftIO $ getDirectoryContents dir
    return $ filter (not . hidden) contents

hidden :: FilePath -> Bool
hidden ('.':_) = True
hidden _       = False

