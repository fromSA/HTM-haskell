-- |
-- Module      : HTM
-- Description : Data type container
-- Copyright   : (c) Fromsa Hera, 2020
-- License     : AGPL-3.0-or-later
-- Maintainer  : fromsahera28@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module has contains the data types used by the other modules in HTM.
module HTM.CommonDataTypes where

import GHC.Natural (Natural)

-- | An index is a integer value >= 0, i.e. a 'Natural' number
type Index' = Natural

-- | The index of a element
type BitIndex = Index'