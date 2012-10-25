-- |
-- Module:      Network.Riak.MontageClient
-- Maintainer:  Erin Dahlgren <edahlgren@bu.mp>
-- Stability:   experimental
-- Portability: portable
--
-- Exposes functions and types for making requests to the Montage riak resolution proxy

module Network.Riak.MontageClient
       (
       -- * Client functions
         module Network.Riak.MontageClient.MontageClient
       -- * Types
       , MontageObject(..)
       )
       where

import Network.Riak.MontageClient.MontageClient
import Network.Riak.MontageClient.Proto.MontageClient.MontageObject
