module HTM.CommonDataTypes where

import GHC.Natural

-- | An index is a integer value >= 0, i.e. a 'Natural' number
type Index' = Natural

-- | The index of a element
type BitIndex = Index'