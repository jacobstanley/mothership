{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mothership.Site
    ( site
    ) where

import           Control.Applicative ((<|>), (<$>))
import           Control.Monad (guard, liftM)
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
import           Snap.Util.FileServe
import           System.FilePath
import           System.Directory
import           Text.Blaze.Html5 hiding (map, head, time)
import           Text.Blaze.Html5.Attributes (href, class_)
import           Text.Blaze.Renderer.XmlHtml (renderHtml)
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Ignore
import qualified Text.XmlHtml as X
import           Prelude hiding (span, lookup)

import           Mothership.Application
import           Mothership.Types
import           Snap.Util
import           Snap.Util.BasicAuth
import           Snap.Util.Git

------------------------------------------------------------------------

site :: Application ()
site = routes <|> serveDirectory "resources/static"
  where
    routes = withSplices $ route
      [ ("/", home)

      , ("/login",  method GET  newLogin)
      , ("/login",  method POST login)
      , ("/logout", method GET  logout)

      , ("/signup", method GET  newSignup)
      , ("/signup", method POST signup)

      , ("/repositories/new", method GET newRepository)

      , ("/:repo", git)
      ]

------------------------------------------------------------------------

git :: Application ()
git = do
    repo <- getParamStr "repo"
    guard (isRepo repo)

    basicAuth "Mothership" attemptLogin

    dir <- getRepoDir
    serveRepo (dir </> repo)
  where
    attemptLogin username password = do
        user <- performLogin euid password False
        return (user /= Nothing)
      where
        euid = EUId $ M.fromList [("username", [username])]

------------------------------------------------------------------------

home :: Application ()
home = do
    repos <- getRepoNames =<< getRepoDir
    ifTop $ renderWithSplices "home"
          [ ("repositories", repoSplice repos) ]

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
    (authUser, user) <- createUser <$> getParams
    au <- saveAuthUser (authUser, toDoc user)
    case au of
        Nothing  -> newSignup
        Just au' -> do
            setSessionUserId (userId au')
            redirect "/"

createUser :: Params -> (AuthUser, User)
createUser ps =
    ( emptyAuthUser
        { userEmail    = Just $ lookupBS "email"
        , userPassword = Just $ ClearText $ lookupBS "password" }
    , User
        { userUsername = lookupT "username"
        , userFullName = lookupT "full_name" }
    )
  where
    lookupBS :: ByteString -> ByteString
    lookupBS k = case M.lookup k ps of
        Just (x:_) -> x
        _          -> error $ "createUser: cannot create without "
                           ++ "parameter '" ++ B.unpack k ++ "'"

    lookupT :: ByteString -> Text
    lookupT = decodeUtf8 . lookupBS

------------------------------------------------------------------------

newRepository :: Application ()
newRepository = render "newrepo"

------------------------------------------------------------------------

withSplices :: Application a -> Application a
withSplices = heistLocal (bindSplices splices)

splices :: [(Text, Splice Application)]
splices =
    [ ("ifLoggedIn",   ifLoggedInSplice)
    , ("ifGuest",      ifGuestSplice)
    , ("userFullName", userFullNameSplice) ]

ifLoggedInSplice :: (MonadAuth m, MonadMongoDB m) => Splice m
ifLoggedInSplice = ifLoggedIn childNodes ignoreImpl

ifGuestSplice :: (MonadAuth m, MonadMongoDB m) => Splice m
ifGuestSplice = ifLoggedIn ignoreImpl childNodes

ifLoggedIn :: (MonadAuth m, MonadMongoDB m) => Splice m -> Splice m -> Splice m
ifLoggedIn yes no = lift authenticatedUserId >>= maybe no (const yes)

childNodes :: Monad m => Splice m
childNodes = liftM X.childNodes getParamNode

userFullNameSplice :: (MonadAuth m, MonadMongoDB m) => Splice m
userFullNameSplice = lift currentAuthUser >>= return . maybe [] name
  where
    name = return . X.TextNode . decodeUtf8 . at "full_name" . snd

------------------------------------------------------------------------

repoSplice :: [Text] -> Splice Application
repoSplice = htmlSplice . repos
  where
    repos = ul . mapM_ repo
    repo name = li $ do
        a ! href (textValue name)
          ! class_ "repo-name"
          $ text name

htmlSplice :: Monad m => Html -> Splice m
htmlSplice = return . docContent . renderHtml

docContent :: X.Document -> [X.Node]
docContent (X.HtmlDocument _ _ ns) = ns
docContent (X.XmlDocument  _ _ ns) = ns

------------------------------------------------------------------------

getRepoNames :: MonadIO m => FilePath -> m [Text]
getRepoNames repoDir = do
    xs <- getDirectoryContents' repoDir
    return $ map T.pack
           $ map takeBaseName
           $ filter isRepo xs

isRepo :: FilePath -> Bool
isRepo = (== ".git") . takeExtension

getDirectoryContents' :: MonadIO m => FilePath -> m [FilePath]
getDirectoryContents' dir = do
    contents <- liftIO $ getDirectoryContents dir
    return $ filter (not . hidden) contents

hidden :: FilePath -> Bool
hidden ('.':_) = True
hidden _       = False
