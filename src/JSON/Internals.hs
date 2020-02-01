{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module JSON.Internals(
    NoPrefix(..)
  ) where

import GHC.Generics
import Data.Aeson as A

newtype NoPrefix a = NoPrefix a

instance ( Generic a
         , A.GToJSON A.Zero (Rep a)
         )
    => ToJSON (NoPrefix a) where
  toJSON (NoPrefix x) = genericToJSON noPrefixOptions x

instance ( Generic a
         , A.GFromJSON A.Zero (Rep a)
         )
    => FromJSON (NoPrefix a) where
  parseJSON = fmap NoPrefix . genericParseJSON noPrefixOptions

noPrefixOptions :: Options
noPrefixOptions = defaultOptions{ fieldLabelModifier = drop 1 }
