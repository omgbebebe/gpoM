{-# LANGUAGE DuplicateRecordFields #-}
module GpoM.Types.CSE.Common where

import Data.List.NonEmpty
import Data.Text (Text)
import Data.Word (Word8)

class FromXml a where
  fromXml :: Text -> a

data Outer = Outer {clsid :: !Text
                   ,disabled :: !(Maybe Bool)
                   }
  deriving (Eq, Show)

mkOuter :: Text -> Outer
mkOuter clsid = Outer clsid Nothing

data Inner = Inner {clsid :: !Text
                   ,image :: !(Maybe Word8)
                   ,bypassErrors :: !(Maybe Bool)
                   ,userContext :: !(Maybe Bool)
                   ,removePolicy :: !(Maybe Bool)
                   ,desc :: !(Maybe Text)
                   ,changed :: !(Maybe Text)
                   ,uid :: !Text
                   ,name :: !Text
                   ,status :: !(Maybe Text)
                   }
  deriving (Eq, Show)

mkInner :: Text -> Text -> Text -> Inner
mkInner clsid uid name = Inner clsid Nothing Nothing Nothing Nothing Nothing Nothing uid name Nothing

data CseXml = CseXml {outer :: Outer
                     ,inner :: NonEmpty Inner}
  deriving Show

instance FromXml CseXml where
  fromXml _ = CseXml {outer = o, inner = i :| [i2]}
    where o = mkOuter "outerClsid"
          i = mkInner "innerClsid" "innerUid" "innerName"
          i2 = mkInner "inner2Clsid" "inner2Uid" "inner2Name"


data EnvironmentVariableProperties =
  EnvironmentVariableProperties{action :: !(Maybe String)
                               ,name :: !Text
                               ,value :: !Text
                               ,user :: !(Maybe Bool)
                               ,partial :: !(Maybe Bool)
                               ,disabled :: !(Maybe Bool)
                               } deriving (Eq, Show)
data EnvironmentVariable =
  EnvironmentVariable{clsid :: !Text
                     ,name :: !Text
                     ,image :: !(Maybe Word8)
                     ,changed :: !(Maybe Text)
                     ,uid :: !Text
                     ,desc :: !(Maybe Text)
                     ,bypassErrors :: !(Maybe Bool)
                     ,userContext :: !(Maybe Bool)
                     ,removePolicy :: !(Maybe Bool)
                     ,properties :: !EnvironmentVariableProperties
                     } deriving (Eq, Show)

data EnvironmentVariables =
  EnvironmentVariables{clsid :: !Text
                      ,disabled :: !(Maybe Bool)
                      ,environmentVariables :: [EnvironmentVariable]
                      } deriving (Eq, Show)
