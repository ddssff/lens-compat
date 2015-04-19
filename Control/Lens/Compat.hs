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
#if 0
    , PL(PL, unPL), composePL, getPL', setPL', totalLens', justLens'
#endif
    ) where

import Control.Applicative (Applicative)
import Control.Monad.State (StateT, get, put)
import qualified Control.Lens
import Control.Lens hiding (Lens, lens, (^=))
import Control.Lens.Internal.Zoom (Zoomed)

type Lens a b = Lens' a b
-- type Lens' a b = Control.Lens.Lens a a b b

-- lens :: Functor f => (s -> a) -> (b -> s -> s) -> (a -> f a) -> s -> f s

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

-- focus :: Monad m => Lens a b -> StateT b m c -> StateT a m c
--focus (Lens f) (StateT g) = StateT $ \a -> case f a of
--  StoreT (Identity h) b -> liftM (second h) (g b)
focus :: forall m n s t c. (Zoom m n s t, Zoomed n ~ Zoomed m) => LensLike' (Zoomed m c) t s -> m c -> n c
focus lns st = zoom lns st

{-
Couldn't match type
 ‘(b -> Zoomed (StateT b m) c b) -> a -> Zoomed (StateT b m) c a’
                  with
 ‘forall (f :: * -> *). Functor f => (b -> f b) -> a -> f a’
    Expected type: Lens a b -> StateT b m c -> StateT a m c
      Actual type: LensLike' (Zoomed (StateT b m) c) a b -> StateT b m c -> StateT a m c
-}

-- | The PartialLens in data-lens is a combination of a Prism' and a Lens'
-- type PartialLens s a = Prism' s a
type PartialLens s a = (Applicative f) => (a -> f a) -> s -> (f s)
-- type PartialLens s a = (Choice p, Applicative f) => p a (f a) -> p s (f s)

getPL :: PartialLens s a -> s -> Maybe a
getPL lns x = x ^? lns

setPL :: PartialLens s a -> a -> s -> s
setPL lns a s = set lns a s

-- type Fold s a = forall f. (Contravariant f, Applicative f) => (a -> f a) -> s -> f s
-- prism' :: (a -> s) -> (s -> Maybe a) -> Prism' s a

-- | Turn a total lens into a partial lens that never returns nothing.
-- Almost certain this doesn't do anything and all occurrences of
-- totalLens can be removed.
totalLens :: Lens s a -> PartialLens s a
totalLens lns = prism' id Just . lns

-- | Stock partial lenses.
justLens :: PartialLens (Maybe a) a
justLens = _Just

leftLens :: PartialLens (Either a b) a
leftLens = _Left
-- _Left = prism Left $ either Right (Left . Right)

rightLens :: PartialLens (Either a b) b
rightLens = _Right
-- _Right = prism Right $ either (Left . Left) Right

headLens :: PartialLens [a] a
headLens = prism (: []) $ (\ xs -> case xs of [] -> Left xs; (x : _) -> Right x)

tailLens :: PartialLens [a] [a]
tailLens = prism (undefined :) $ (\ xs -> case xs of [] -> Left xs; (_ : xs') -> Right xs')

null :: PartialLens (Maybe a) ()
null = _Nothing

-- I don't fully understand why this wrapper is sometimes needed
#if 0
newtype PL s a = PL {unPL :: Prism' s a}

composePL :: PL s a -> PL a b -> PL s b
composePL (PL a) (PL b) = PL (a . b)

getPL' :: PL s a -> s -> Maybe a
getPL' (PL lns) x = x ^? lns

setPL' :: PL s a -> a -> s -> s
setPL' (PL lns) y x = set lns y x

totalLens' :: Lens s a -> PL s a
totalLens' lns = PL $ totalLens lns

justLens' :: PL (Maybe a) a
justLens' = PL justLens

#endif
