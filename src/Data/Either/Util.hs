module Data.Either.Util
    (
      maybeToEither
    ) where

maybeToEither :: e -> Maybe a -> Either e a
maybeToEither e Nothing = Left e
maybeToEither _ (Just a) = Right a
