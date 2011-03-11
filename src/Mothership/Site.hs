{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Mothership.Site
    ( site
    ) where

import           Control.Applicative ((<|>), (<$>))
import           Control.Monad (guard)
import           Control.Monad.Trans (lift)
import qualified Data.ByteString.Char8 as B
import           Data.ByteString.Char8 (ByteString)
import qualified Data.Map as M
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Snap.Auth
import           Snap.Auth.Handlers
import           Snap.Extension.DB.MongoDB (MonadMongoDB)
import           Snap.Extension.Heist
import           Snap.Extension.Session.CookieSession
import           Snap.Types hiding (path, dir)
import           Snap.Util.FileServe
import           System.FilePath
import           Text.Blaze.Html5 hiding (map, head, time)
import           Text.Blaze.Html5.Attributes (href, class_)
import           Text.Blaze.Renderer.XmlHtml (renderHtml)
import           Text.Templating.Heist
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

      , ("/repositories/new", method GET  newRepo)
      , ("/repositories",     method POST createRepo)

      , ("/:repo", git)
      ]

------------------------------------------------------------------------

home :: Application ()
home = do
    ifTop $ renderWithSplices "home"
          [ ("repositories", repoSplice) ]

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
        { userEmail    = Just $ lookupBS "email" ps
        , userPassword = Just $ ClearText $ lookupBS "password" ps }
    , User
        { userUsername = lookupT "username" ps
        , userFullName = lookupT "full_name" ps }
    )

currentUser :: (MonadAuth m, MonadMongoDB m) => m (Maybe User)
currentUser = toUser <$> currentAuthUser
  where
    toUser x = fmap snd x >>= fromDoc

lookupBS :: ByteString -> Params -> ByteString
lookupBS k ps = case M.lookup k ps of
    Just (x:_) -> x
    _          -> error $ "lookupBS: missing required parameter '"
                          ++ B.unpack k ++ "'"

lookupT :: ByteString -> Params -> Text
lookupT k = decodeUtf8 . lookupBS k

------------------------------------------------------------------------

newRepo :: Application ()
newRepo = render "newrepo"

createRepo :: Application ()
createRepo = do
    repo <- mkRepo <$> getParams
    insert repo
    redirect "/"
  where
    mkRepo ps = Repository
        { repoName = lookupT "name" ps
        , repoDescription = lookupT "description" ps }

------------------------------------------------------------------------

withSplices :: Application a -> Application a
withSplices = heistLocal (bindSplices splices)

splices :: [(Text, Splice Application)]
splices =
    [ ("ifLoggedIn",   ifLoggedIn)
    , ("ifGuest",      ifGuest)
    , ("requireAuth",  requireAuth)
    , ("userFullName", userFullNameSplice)
    ]

ifLoggedIn :: Splice Application
ifLoggedIn = do
    node <- getParamNode
    lift $ requireUser (return []) (return $ X.childNodes node)

ifGuest :: Splice Application
ifGuest = do
    node <- getParamNode
    lift $ requireUser (return $ X.childNodes node) (return [])

requireAuth :: Splice Application
requireAuth = lift $ requireUser pass (return [])

userFullNameSplice :: Splice Application
userFullNameSplice = lift currentUser >>= return . maybe [] name
  where
    name = return . X.TextNode . userFullName

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

isRepo :: FilePath -> Bool
isRepo = (== ".git") . takeExtension

------------------------------------------------------------------------

repoSplice :: Splice Application
repoSplice = lift findAll >>= htmlSplice . repos
  where
    repos = ul . mapM_ repo
    repo x = li $ do
        a ! href (textValue $ repoName x)
          ! class_ "repo-name"
          $ text (repoName x)

htmlSplice :: Monad m => Html -> Splice m
htmlSplice = return . docContent . renderHtml

docContent :: X.Document -> [X.Node]
docContent (X.HtmlDocument _ _ ns) = ns
docContent (X.XmlDocument  _ _ ns) = ns
