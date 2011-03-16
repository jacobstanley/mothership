-- | Heist 0.5.x
module Heist.Future
    ( viewWith
    , viewWithTemplates
    , viewWithText
    , mapSplices
    ) where

import           Control.Arrow (second)
import           Control.Monad (liftM)
import           Data.Text (Text)
import           Text.Templating.Heist
import qualified Text.XmlHtml as X

------------------------------------------------------------------------------
-- | Runs the parameter node's children and returns the resulting node list.
-- By itself this function is a simple passthrough splice that makes the
-- spliced node disappear.  In combination with locally bound splices, this
-- function makes it easier to pass the desired view into your splices.
runChildren :: Monad m => Splice m
runChildren = runNodeList . X.childNodes =<< getParamNode


------------------------------------------------------------------------------
-- | Binds a list of splices before using the children of the spliced node as
-- a view.
viewWith :: (Monad m)
         => [(Text, Splice m)]
         -- ^ List of splices to bind before running the param nodes.
         -> Splice m
         -- ^ Returns the passed in view.
viewWith splices = localTS (bindSplices splices) runChildren


------------------------------------------------------------------------------
-- | Wrapper around viewWith that applies a transformation function to the
-- second item in each of the tuples before calling viewWith.
viewTrans :: (Monad m)
          => (b -> Splice m)
          -- ^ Splice generating function
          -> [(Text, b)]
          -- ^ List of tuples to be bound
          -> Splice m
viewTrans f = viewWith . map (second f)


------------------------------------------------------------------------------
-- | Like viewWith but using constant templates rather than dynamic splices.
viewWithTemplates :: (Monad m) => [(Text, Template)] -> Splice m
viewWithTemplates = viewTrans return


------------------------------------------------------------------------------
-- | Like viewWith but using literal text rather than dynamic splices.
viewWithText :: (Monad m) => [(Text, Text)] -> Splice m
viewWithText = viewTrans (return . (:[]) . X.TextNode)


------------------------------------------------------------------------------
-- | Maps a splice generating function over a list and concatenates the
-- results.
mapSplices :: (Monad m)
        => (a -> Splice m)
        -- ^ Function applied to each element of the list to generate splices
        -> [a]
        -- ^ List of items to generate splices for
        -> Splice m
        -- ^ The result of all splices concatenated together.
mapSplices f vs = liftM concat $ mapM f vs


------------------------------------------------------------------------------

localTS :: Monad m
        => (TemplateState m -> TemplateState m)
        -> TemplateMonad m a
        -> TemplateMonad m a
localTS f k = do
    ts <- getTS
    putTS $ f ts
    res <- k
    restoreTS ts
    return res
