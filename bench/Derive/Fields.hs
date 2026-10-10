-- | The values of the benchmark types, shared by "Derive.Generic" and
-- "Derive.Manual".
module Derive.Fields
  ( fields
  ) where

import Data.Text qualified as T

-- | The fields of a record of the benchmarks for the number, given to its
-- constructor. The records have the same field types.
fields
  :: ( T.Text
       -> Maybe Int
       -> Int
       -> T.Text
       -> Maybe Int
       -> Int
       -> T.Text
       -> Maybe Int
       -> Int
       -> T.Text
       -> r
     )
  -> Int
  -> r
fields con i =
  con
    (T.pack (show i))
    (if even i then Nothing else Just 2)
    (i + 3)
    (T.pack (show (i * 4)))
    (if even i then Nothing else Just 5)
    (i + 6)
    (T.pack (show (i * 7)))
    (if even i then Nothing else Just 8)
    (i + 9)
    (T.pack (show (i * 10)))
