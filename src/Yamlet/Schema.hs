-- | The core schema of YAML 1.2.2: the rules that give a scalar its value.
--
-- A decoder applies these rules to every scalar. A program that writes YAML
-- can use them to check how a plain scalar reads back, e.g. @9.10@ is a
-- number, not a string.
module Yamlet.Schema
  ( resolvePlain
  , resolveTagged
  , isPlainString
  , isPlainSafe
  ) where

import Yamlet.Internal.Schema
