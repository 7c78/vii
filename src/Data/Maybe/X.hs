module Data.Maybe.X where

import Control.Applicative ((<|>))
import Data.Maybe

altM :: forall m a. (Monad m) => m (Maybe a) -> m (Maybe a) -> m (Maybe a)
altM mx my = pure (<|>) <*> mx <*> my

orElseM :: forall m a. (Monad m) => m (Maybe a) -> m a -> m a
orElseM mx y = mx >>= maybe y pure

orElse :: forall a. Maybe a -> a -> a
orElse = flip fromMaybe
