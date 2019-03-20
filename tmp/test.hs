{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Main where

import GHC.Generics
import Data.Maybe (listToMaybe)
import Generics.XmlPickler (gxpickle)
import Text.XML.HXT.Arrow.Pickle --(XmlPickler (..), showPickled, unpickleDoc)
import Text.XML.HXT.Parser.XmlParsec (xread)
import Data.Word (Word8)
import Data.Text (Text)
import Data.Aeson

data Properties = Properties { name :: String
                             , value :: String
                             , disabled :: Maybe Bool
                             , partial :: Maybe Bool
                             , user :: Maybe Bool
                             , action :: Maybe String
                             }
  deriving (Eq, Show, Generic)

instance FromJSON Properties
instance ToJSON Properties

instance XmlPickler Properties where
  xpickle = xpProperties

data EnvironmentVariable = EnvironmentVariable { name :: String
                                               , clsid :: String
                                               , uid :: String
                                               , removePolicy :: Maybe Bool
                                               , userContext :: Maybe Bool
                                               , bypassErrors :: Maybe Bool
                                               , desc :: Maybe String
                                               , changed :: Maybe String
                                               , image :: Maybe Word8
                                               , status :: Maybe String
                                               , properties :: Properties
                                               }
  deriving (Eq, Show, Generic)

instance FromJSON EnvironmentVariable
instance ToJSON EnvironmentVariable

instance XmlPickler EnvironmentVariable where
  xpickle = xpEnvironmentVariable


data EnvironmentVariables = EnvironmentVariables { clsid :: String
                                                 , environmentVariables :: [EnvironmentVariable]
                                                 , disabled :: Maybe Bool
                                                 }
  deriving (Eq, Show, Generic)
instance FromJSON EnvironmentVariables
instance ToJSON EnvironmentVariables


instance XmlPickler EnvironmentVariables where
  xpickle = xpEnvironmentVariables

xpEnvironmentVariables :: PU EnvironmentVariables
xpEnvironmentVariables = xpElem "EnvironmentVariables" $
  xpWrap (\(a,b,c) -> EnvironmentVariables a b c
         ,\t -> (clsid (t :: EnvironmentVariables), environmentVariables t, disabled (t :: EnvironmentVariables))
         ) $
  xpTriple
  (xpAttr "clsid" xpText)
  (xpList xpickle)
  (xpOption (xpAttr "disabled" xpBool))

xpEnvironmentVariable :: PU EnvironmentVariable
xpEnvironmentVariable = xpElem "EnvironmentVariable" $
  xpWrap (\(a,b,c,d,e,f,g,h,i,j,k) -> EnvironmentVariable a b c d e f g h i j k
         ,\t -> ( name (t :: EnvironmentVariable)
                , clsid (t :: EnvironmentVariable)
                , uid t
                , removePolicy t
                , userContext t
                , bypassErrors t
                , desc t
                , changed t
                , image t
                , status t
                , properties t
                )

         ) $
  xp11Tuple
  (xpAttr "name" xpText)
  (xpAttr "clsid" xpText)
  (xpAttr "uid" xpText)
  (xpOption (xpAttr "removePolicy" xpBool))
  (xpOption (xpAttr "userContext" xpBool))
  (xpOption (xpAttr "bypassErrors" xpBool))
  (xpOption (xpAttr "desc" xpText))
  (xpOption (xpAttr "changed" xpText))
  (xpOption (xpAttr "image" xpPrim))
  (xpOption (xpAttr "status" xpText))
  (xpickle)



fromBool :: Bool -> Int
fromBool True = 1
fromBool _ = 0

toBool :: Int -> Bool
toBool 1 = True
toBool _ = False

--xpBool :: PU Bool
xpBool = xpWrap (toBool, fromBool) xpickle

xpProperties :: PU Properties
xpProperties = xpElem "Properties" $
  xpWrap (\(a, b, c, d, e, f) -> Properties a b c d e f
         ,\t -> (name (t :: Properties), value t, disabled (t :: Properties), partial t, user t, action t)
         ) $
  xp6Tuple
  (xpAttr "name" xpText)
  (xpAttr "value" xpText)
  (xpOption (xpAttr "disabled" xpBool))
  (xpOption (xpAttr "partial" xpBool))
  (xpOption (xpAttr "user" xpBool))
  (xpOption (xpAttr "action" xpText))


{--
data User = User
  { name  :: String
  , admin :: Bool
  } deriving (Show, Generic)

instance XmlPickler User where
  xpickle = gxpickle


userString :: String
userString = showPickled [] (User "Simon" True)
-- = "<user><name>Simon</name><admin>true</admin></user>"

user :: Maybe User
user = unpickleDoc xpickle =<< listToMaybe (xread "<user><name>Simon</name><admin>true</admin></user>")
-- = Just (User {name = "Simon", admin = True})
--}
main :: IO ()
main = undefined

