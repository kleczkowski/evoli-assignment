{-# LANGUAGE TemplateHaskell #-}
module Evoli.Assignment.Interfaces.Storage where

import Polysemy

-- | An effect that allows to store and manage values @v@ under key @k@.
data Storage k v m a where
  StorageGet    :: Proxy v -> k -> Storage k v m (Maybe v)
  StoragePut    :: k -> v -> Storage k v m ()
  StorageDelete :: Proxy v -> k -> Storage k v m ()

makeSem ''Storage
