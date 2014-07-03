{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module Regex where

import Foreign
import Foreign.C.Types

#include <pcre.h>

newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq, Show)

#{enum PCREOption, PCREOption
, caseless = PCRE_CASELESS
, dollar_endonly = PCRE_DOLLAR_ENDONLY
, dotall = PCRE_DOTALL
}

combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
