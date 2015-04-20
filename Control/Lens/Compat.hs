-- | This module can assist in transition from the data-lens package
-- to the lens package.  However, it is not a drop-in replacement.  In
-- particular, to get things to build you will first have to:
--
-- * Reverse order of composition using @(.)@.  Remove imports of
--   @Control.Category ((.))@ and import @(.)@ from Prelude instead.
--
-- * PartialLens is currently a wrapper, so @(.)@ won't work on it,
--   use composePL.
--
-- * Need to add @{-# LANGUAGE RankNTypes #-}@ to some modules
-- * If you use fancy name functions with the template haskell lens
--   generator nameMakeLens you need to convert your types to simple
--   "starts with _" format
--
-- * You need to call makeLenses and makePrisms for each type name
--   individually.

{-# LANGUAGE CPP, RankNTypes, GADTs #-}
module Control.Lens.Compat
    ( Lens
    , lens
    , Control.Lens.Compat.iso
    , getL, (^$)
    , setL, (^=)
    , modL
    , access
    , focus
    , (Control.Lens.Compat.%=)
    , (Control.Lens.Compat.~=)
    , PartialLens
    , getPL, setPL, totalLens, justLens
    , leftLens, rightLens, headLens, tailLens, Control.Lens.Compat.null
    ) where

import Control.Applicative (Applicative)
import Control.Monad.State (StateT, get, put)
import qualified Control.Lens
import Control.Lens hiding (Lens, lens, (^=))
import Control.Lens.Internal.Zoom (Zoomed)

type Lens a b = Lens' a b

lens :: (s -> a) -> (a -> s -> s) -> Lens s a
lens getter setter = Control.Lens.lens getter (flip setter)

iso :: (s -> a) -> (a -> s) -> Lens s a
iso = Control.Lens.iso

getL :: Lens s a -> s -> a
getL lns x = x ^. lns -- view lns x

setL :: Lens s a -> a -> s -> s
setL lns a s = lns .~ a $ s -- set lns a s

modL :: Lens s a -> (a -> a) -> s -> s
modL lns f s = lns %~ f $ s -- over lns f s

access :: Monad m => Lens s a -> StateT s m a
access lns = use lns

(^$) :: Lens s a -> s -> a
(^$) = getL

(^=) :: Lens s a -> a -> s -> s
(^=) = setL

(~=) :: Monad m => Lens s a -> a -> StateT s m ()
lns ~= a = lns .= a

(%=) :: Monad m => Lens s a -> (a -> a) -> StateT s m ()
lns %= f = get >>= \s -> put (set lns (f (getL lns s)) s)

focus :: forall m n s t c. (Zoom m n s t, Zoomed n ~ Zoomed m) => LensLike' (Zoomed m c) t s -> m c -> n c
focus lns st = zoom lns st

-- | The PartialLens in data-lens is a Traversal'.
type PartialLens s a = Traversal' s a

getPL :: PartialLens s a -> s -> Maybe a
getPL lns x = x ^? lns

setPL :: PartialLens s a -> a -> s -> s
setPL lns a s = set lns a s

totalLens :: Lens s a -> PartialLens s a
totalLens lns = lns

-- | Stock partial lenses.
justLens :: PartialLens (Maybe a) a
justLens = _Just

leftLens :: PartialLens (Either a b) a
leftLens = _Left

rightLens :: PartialLens (Either a b) b
rightLens = _Right

headLens :: PartialLens [a] a
headLens = prism (: []) $ (\ xs -> case xs of [] -> Left xs; (x : _) -> Right x)

tailLens :: PartialLens [a] [a]
tailLens = prism (undefined :) $ (\ xs -> case xs of [] -> Left xs; (_ : xs') -> Right xs')

null :: PartialLens (Maybe a) ()
null = _Nothing
