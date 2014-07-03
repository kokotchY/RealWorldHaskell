{-# LINE 1 "Regex-hsc.hs" #-}
{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# LINE 2 "Regex-hsc.hs" #-}
module Regex where

import Foreign
import Foreign.C.Types


{-# LINE 8 "Regex-hsc.hs" #-}

newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq, Show)

caseless  :: PCREOption
caseless  = PCREOption 1
dollar_endonly  :: PCREOption
dollar_endonly  = PCREOption 32
dotall  :: PCREOption
dotall  = PCREOption 4

{-# LINE 17 "Regex-hsc.hs" #-}

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
