{-# LANGUAGE OverloadedStrings #-}

module Mothership.Site.UI
    ( serveUI
    ) where

import           Control.Monad (forM_)
import           Control.Monad.Trans (MonadIO, liftIO, lift)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map as M
import qualified Data.Text as T
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Snap.Auth
import           Snap.Auth.Handlers
import           Snap.Extension.DB.MongoDB
import           Snap.Extension.Heist
import           Snap.Extension.Session.CookieSession
import           Snap.Types hiding (path, dir)
import           System.FilePath
import           System.Directory
import           Text.Blaze.Html5 hiding (map, head, time)
import           Text.Blaze.Html5.Attributes (href, class_)
import           Text.Blaze.Renderer.XmlHtml (renderHtml)
import           Text.Templating.Heist
import qualified Text.XmlHtml as X
import           Prelude hiding (span)

import           Mothership.Application

------------------------------------------------------------------------

serveUI :: Application ()
serveUI = route
    [ ("/", home)

    , ("/login",  method GET  newLogin)
    , ("/login",  method POST login)
    , ("/logout", method GET  logout)

    , ("/signup", method GET  newSignup)
    , ("/signup", method POST signup)
    ]

------------------------------------------------------------------------

home :: Application ()
home = do
    users <- getRepoUsers =<< getRepoDir
    ifTop $ renderWithSplices "home"
          [ ("repositories", repoSplice users)
          , ("user", userSplice) ]

------------------------------------------------------------------------

newLogin :: Application ()
newLogin = render "login"

login :: Application ()
login = loginHandler "password" Nothing newLogin (redirect "/")

logout :: Application ()
logout = logoutHandler (redirect "/")

------------------------------------------------------------------------

newSignup :: Application ()
newSignup = render "signup"

signup :: Application ()
signup = do
    ps <- getParams
    let usr = createUser ps
    au <- saveAuthUser (authUser usr, ["full_name" =: userFullName usr])
    case au of
        Nothing  -> newSignup
        Just au' -> do
            setSessionUserId (userId au')
            redirect "/"

data User = User
   { authUser     :: AuthUser
   , userFullName :: ByteString
   } deriving (Show)

createUser :: Params -> User
createUser ps = User
    { authUser = emptyAuthUser
        { userEmail    = Just $ lookup' "email"
        , userPassword = Just $ ClearText $ lookup' "password" }
    , userFullName = lookup' "full_name" }
  where
    lookup' :: ByteString -> ByteString
    lookup' k = case M.lookup k ps of
        Just (x:_) -> x
        _          -> error $ "createUser: cannot create without "
                           ++ "parameter '" ++ B.unpack k ++ "'"

------------------------------------------------------------------------

userSplice :: Splice Application
userSplice = do
    au <- lift currentAuthUser
    let user = case au of
          Nothing -> ("" :: ByteString)
          Just (_, usr) -> "full_name" `at` usr
    htmlSplice (span $ text $ decodeUtf8 user)

------------------------------------------------------------------------

repoSplice :: [RepoUser] -> Splice Application
repoSplice = htmlSplice . users
  where
    users = ul . mapM_ user
    user (RepoUser n rs) = forM_ rs $ \r -> li $ do
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

data RepoUser = RepoUser
    { _name  :: Text
    , _repos :: [Text]
    }

getRepoUsers :: MonadIO m => FilePath -> m [RepoUser]
getRepoUsers repoDir = do
    names <- getDirectoryContents' repoDir
    repos <- mapM getRepoNames $ map (repoDir </>) names
    let namesT = map T.pack names
        reposT = map (map T.pack) repos
    return $ zipWith RepoUser namesT reposT
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
