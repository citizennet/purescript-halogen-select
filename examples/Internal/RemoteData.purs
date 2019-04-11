-- | Copied over from 
-- | https://github.com/krisajenkins/purescript-remotedata
-- |
-- | due to dependency conflicts
module Docs.Internal.RemoteData where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))

-- | A datatype representing fetched data.
-- |
-- | If you find yourself continually using `Maybe (Either e a)` to
-- | represent data loaded from an external source, or you have a
-- | habit of shuffling errors away to where they can be quietly
-- | ignored, consider using this. It makes it easier to represent the
-- | real state of a remote data fetch and handle it properly.
data RemoteData e a
  = NotAsked
  | Loading
  | Failure e
  | Success a

derive instance eqRemoteData :: (Eq e, Eq a) => Eq (RemoteData e a)

derive instance functorRemoteData :: Functor (RemoteData e)

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

