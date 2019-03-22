module Data.ULID.Crockford
  ( encode
  , decode
  )
where

import qualified Codec.Crockford               as CR
import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import           Text.Read

-- source: https://stackoverflow.com/a/29153602
-- I removed the safety for m > length because that should never happen
-- and if it does, I want it to crash!
leftpad :: Int -> Text -> Text
leftpad m xs = T.justifyRight l '0' xs--replicate (m - length xs) '0' ++ xs
  where l = m - T.length xs

encode :: Int -> Integer -> T.Text
encode width = leftpad width . T.pack . CR.encode

decode :: Int -> ReadS Integer
decode width str
  | length str >= width
  = let (crock, remainder) = splitAt width str
    in  case CR.decode crock of
          Nothing -> []
          Just c  -> [(c, remainder)]
  | otherwise
  = []
