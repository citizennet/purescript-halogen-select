-- | Copied over from 
-- | https://github.com/krisajenkins/purescript-remotedata
-- |
-- | due to dependency conflicts
module Docs.Internal.RemoteData where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Foldable (class Foldable, foldrDefault, foldlDefault)

-- | A datatype representing fetched data.
data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a

derive instance eqRemoteData :: (Eq e, Eq a) => Eq (RemoteData e a)
derive instance functorRemoteData :: Functor (RemoteData e)

instance foldableRemoteData :: Foldable (RemoteData e) where
  foldMap f (Success a) = f a
  foldMap _ (Failure e) = mempty
  foldMap _ NotAsked = mempty
  foldMap _ Loading = mempty
  foldr f = foldrDefault f
  foldl f = foldlDefault f

-- | Convert a `RemoteData` to a `Maybe`.
toMaybe :: forall e a. RemoteData e a -> Maybe a
toMaybe (Success value) = Just value
toMaybe _ = Nothing

-- | Convert a `Maybe` to `RemoteData`.
fromMaybe :: forall e a. Maybe a -> RemoteData e a
fromMaybe Nothing = NotAsked
fromMaybe (Just value) = Success value

-- | Convert an `Either` to `RemoteData`
fromEither :: forall e a. Either e a -> RemoteData e a
fromEither (Left err) = Failure err
fromEither (Right value) = Success value

