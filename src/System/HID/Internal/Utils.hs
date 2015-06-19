-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
----------------------------------------------------------------------------

module System.HID.Internal.Utils where

import Foreign.C.Types

-- |Convert returned int by hidapi into booleans.
--
--   - **0** means success; 'True'
--   - **-1** means failure; 'False'
--
-- For the sake of safety, anything that is not **0** is considered a failure.
fromHIDRet :: CInt -> Bool
fromHIDRet r
  | r == 0 = True
  | otherwise = False
