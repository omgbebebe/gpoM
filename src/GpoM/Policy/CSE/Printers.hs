module GpoM.Policy.CSE.Printers where

import GpoM.Types.Policy
import Data.Default
import GHC.Generics
import Text.XML.HXT.Arrow.Pickle
import GpoM.Policy.CSE.Parser.Common (xpBool, xpText')
import Toml (ParseException (..), TomlCodec, pretty, (.=), (<!>))
import qualified Toml
import Data.Text (Text)
import qualified Data.Text as T

data Printers = Printers { _clsid          :: !Text
                         , _disabled       :: !(Maybe Bool)
                         , _sharedPrinters :: ![SharedPrinter]
                         , _portPrinters   :: ![PortPrinter]
                         , _localPrinters  :: ![LocalPrinter]
                         }
  deriving (Eq, Show, Generic)

printersCodec :: TomlCodec Printers
printersCodec = Printers
  <$> Toml.text "clsid" .= (_clsid :: Printers -> Text)
  <*> Toml.dioptional (Toml.bool "disabled") .= (_disabled :: Printers -> Maybe Bool)
  <*> Toml.list sharedPrinterCodec "sharedPrinters" .= _sharedPrinters
  <*> Toml.list portPrinterCodec "portPrinters" .= _portPrinters
  <*> Toml.list localPrinterCodec "localPrinters" .= _localPrinters

instance XmlPickler Printers where
  xpickle = xpPrinters

data SharedPrinter = SharedPrinter { _clsid        :: !Text
                                   , _name         :: !Text
                                   , _uid          :: !Text
                                   , _image        :: !(Maybe Int)
                                   , _changed      :: !(Maybe Text)
                                   , _desc         :: !(Maybe Text)
                                   , _bypassError  :: !(Maybe Bool)
                                   , _userContext  :: !(Maybe Bool)
                                   , _removePolicy :: !(Maybe Bool)
                                   , _status       :: !(Maybe Text)
                                   , _properties   :: !SharedPrinterProperties
                                   }
  deriving (Eq, Show, Generic)

sharedPrinterCodec :: TomlCodec SharedPrinter
sharedPrinterCodec = SharedPrinter
  <$> Toml.text "clsid" .= (_clsid :: SharedPrinter -> Text)
  <*> Toml.text "name" .= (_name :: SharedPrinter -> Text)
  <*> Toml.text "uid" .= (_uid :: SharedPrinter -> Text)
  <*> Toml.dioptional (Toml.int "image") .= (_image :: SharedPrinter -> Maybe Int)
  <*> Toml.dioptional (Toml.text "changed") .= (_changed :: SharedPrinter -> Maybe Text)
  <*> Toml.dioptional (Toml.text "desc") .= (_desc :: SharedPrinter -> Maybe Text)
  <*> Toml.dioptional (Toml.bool "bypassError") .= (_bypassError :: SharedPrinter -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "userContext") .= (_userContext :: SharedPrinter -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "removePolicy") .= (_removePolicy :: SharedPrinter -> Maybe Bool)
  <*> Toml.dioptional (Toml.text "status") .= (_status :: SharedPrinter -> Maybe Text)
  <*> Toml.table sharedPrinterPropertiesCodec "properties" .= (_properties :: SharedPrinter -> SharedPrinterProperties)

instance XmlPickler SharedPrinter where
  xpickle = xpSharedPrinter


data SharedPrinterProperties = SharedPrinterProperties { _path       :: !Text
                                                       , _port       :: !Text
                                                       , _action     :: !(Maybe Text)
                                                       , _comment    :: !(Maybe Text)
                                                       , _location   :: !(Maybe Text)
                                                       , _default    :: !(Maybe Bool)
                                                       , _skipLocal  :: !(Maybe Bool)
                                                       , _deleteAll  :: !(Maybe Bool)
                                                       , _persistent :: !(Maybe Bool)
                                                       , _deleteMaps :: !(Maybe Bool)
                                                       , _username   :: !(Maybe Text)
                                                       , _cpassword  :: !(Maybe Text)
                                                       , _disabled   :: !(Maybe Bool)
                                                       }
  deriving (Eq, Show, Generic)

sharedPrinterPropertiesCodec :: TomlCodec SharedPrinterProperties
sharedPrinterPropertiesCodec = SharedPrinterProperties
  <$> Toml.text "path" .= (_path :: SharedPrinterProperties -> Text)
  <*> Toml.text "port" .= (_port :: SharedPrinterProperties -> Text)
  <*> Toml.dioptional (Toml.text "action") .= (_action :: SharedPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.text "comment") .= (_comment :: SharedPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.text "location") .= (_location :: SharedPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.bool "default") .= (_default :: SharedPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "skipLocal") .= (_skipLocal :: SharedPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "deleteAll") .= (_deleteAll :: SharedPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "persistent") .= (_persistent :: SharedPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "deleteMaps") .= (_deleteMaps :: SharedPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.text "username") .= (_username :: SharedPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.text "cpassword") .= (_cpassword :: SharedPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.bool "disabled") .= (_disabled :: SharedPrinterProperties -> Maybe Bool)

instance XmlPickler SharedPrinterProperties where
  xpickle = xpSharedPrinterProperties


data PortPrinter = PortPrinter { _clsid        :: !Text
                               , _name         :: !Text
                               , _uid          :: !Text
                               , _image        :: !(Maybe Int)
                               , _changed      :: !(Maybe Text)
                               , _desc         :: !(Maybe Text)
                               , _bypassError  :: !(Maybe Bool)
                               , _userContext  :: !(Maybe Bool)
                               , _removePolicy :: !(Maybe Bool)
                               , _status       :: !(Maybe Text)
                               , _properties   :: !PortPrinterProperties
                               }
  deriving (Eq, Show, Generic)

portPrinterCodec :: TomlCodec PortPrinter
portPrinterCodec = PortPrinter
  <$> Toml.text "clsid" .= (_clsid :: PortPrinter -> Text)
  <*> Toml.text "name" .= (_name :: PortPrinter -> Text)
  <*> Toml.text "uid" .= (_uid :: PortPrinter -> Text)
  <*> Toml.dioptional (Toml.int "image") .= (_image :: PortPrinter -> Maybe Int)
  <*> Toml.dioptional (Toml.text "changed") .= (_changed :: PortPrinter -> Maybe Text)
  <*> Toml.dioptional (Toml.text "desc") .= (_desc :: PortPrinter -> Maybe Text)
  <*> Toml.dioptional (Toml.bool "bypassError") .= (_bypassError :: PortPrinter -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "userContext") .= (_userContext :: PortPrinter -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "removePolicy") .= (_removePolicy :: PortPrinter -> Maybe Bool)
  <*> Toml.dioptional (Toml.text "status") .= (_status :: PortPrinter -> Maybe Text)
  <*> Toml.table portPrinterPropertiesCodec "properties" .= (_properties :: PortPrinter -> PortPrinterProperties)

instance XmlPickler PortPrinter where
  xpickle = xpPortPrinter

data PortPrinterProperties = PortPrinterProperties { _ipAddress     :: !Text
                                                   , _path          :: !Text
                                                   , _action        :: !(Maybe Text)
                                                   , _location      :: !(Maybe Text)
                                                   , _localName     :: !(Maybe Text)
                                                   , _comment       :: !(Maybe Text)
                                                   , _default       :: !(Maybe Bool)
                                                   , _skipLocal     :: !(Maybe Bool)
                                                   , _useDNS        :: !(Maybe Bool)
                                                   , _deleteAll     :: !(Maybe Bool)
                                                   , _lprQueue      :: !(Maybe Text)
                                                   , _snmpCommunity :: !(Maybe Text)
                                                   , _protocol      :: !(Maybe Text)
                                                   , _portNumber    :: !(Maybe Int)
                                                   , _doubleSpool   :: !(Maybe Bool)
                                                   , _snmpEnabled   :: !(Maybe Bool)
                                                   , _snmpDevIndex  :: !(Maybe Int)
                                                   , _disabled      :: !(Maybe Bool)
                                                   }
  deriving (Eq, Show, Generic)

portPrinterPropertiesCodec :: TomlCodec PortPrinterProperties
portPrinterPropertiesCodec = PortPrinterProperties
  <$> Toml.text "ipAddress" .= (_ipAddress :: PortPrinterProperties -> Text)
  <*> Toml.text "path" .= (_path :: PortPrinterProperties -> Text)
  <*> Toml.dioptional (Toml.text "action") .= (_action :: PortPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.text "location") .= (_location :: PortPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.text "localName") .= (_localName :: PortPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.text "comment") .= (_comment :: PortPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.bool "default") .= (_default :: PortPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "skipLocal") .= (_skipLocal :: PortPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "useDNS") .= (_useDNS :: PortPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "deleteAll") .= (_deleteAll :: PortPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.text "lprQueue") .= (_lprQueue :: PortPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.text "snmpCommunity") .= (_snmpCommunity :: PortPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.text "protocol") .= (_protocol :: PortPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.int "portNumber") .= (_portNumber :: PortPrinterProperties -> Maybe Int)
  <*> Toml.dioptional (Toml.bool "doubleSpool") .= (_doubleSpool :: PortPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "snmpEnabled") .= (_snmpEnabled :: PortPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.int "snmpDevIndex") .= (_snmpDevIndex :: PortPrinterProperties -> Maybe Int)
  <*> Toml.dioptional (Toml.bool "disabled") .= (_disabled :: PortPrinterProperties -> Maybe Bool)

instance XmlPickler PortPrinterProperties where
  xpickle = xpPortPrinterProperties

data LocalPrinter = LocalPrinter { _clsid        :: !Text
                                 , _name         :: !Text
                                 , _uid          :: !Text
                                 , _image        :: !(Maybe Int)
                                 , _changed      :: !(Maybe Text)
                                 , _desc         :: !(Maybe Text)
                                 , _bypassError  :: !(Maybe Bool)
                                 , _userContext  :: !(Maybe Bool)
                                 , _removePolicy :: !(Maybe Bool)
                                 , _status       :: !(Maybe Text)
                                 , _properties   :: !LocalPrinterProperties
                                 }
  deriving (Eq, Show, Generic)

localPrinterCodec :: TomlCodec LocalPrinter
localPrinterCodec = LocalPrinter
  <$> Toml.text "clsid" .= (_clsid :: LocalPrinter -> Text)
  <*> Toml.text "name" .= (_name :: LocalPrinter -> Text)
  <*> Toml.text "uid" .= (_uid :: LocalPrinter -> Text)
  <*> Toml.dioptional (Toml.int "image") .= (_image :: LocalPrinter -> Maybe Int)
  <*> Toml.dioptional (Toml.text "changed") .= (_changed :: LocalPrinter -> Maybe Text)
  <*> Toml.dioptional (Toml.text "desc") .= (_desc :: LocalPrinter -> Maybe Text)
  <*> Toml.dioptional (Toml.bool "bypassError") .= (_bypassError :: LocalPrinter -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "userContext") .= (_userContext :: LocalPrinter -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "removePolicy") .= (_removePolicy :: LocalPrinter -> Maybe Bool)
  <*> Toml.dioptional (Toml.text "status") .= (_status :: LocalPrinter -> Maybe Text)
  <*> Toml.table localPrinterPropertiesCodec "properties" .= (_properties :: LocalPrinter -> LocalPrinterProperties)

instance XmlPickler LocalPrinter where
  xpickle = xpLocalPrinter

data LocalPrinterProperties = LocalPrinterProperties { _name      :: !Text
                                                     , _port      :: !Text
                                                     , _path      :: !Text
                                                     , _action    :: !(Maybe Text)
                                                     , _default   :: !(Maybe Bool)
                                                     , _deleteAll :: !(Maybe Bool)
                                                     , _location  :: !(Maybe Text)
                                                     , _comment   :: !(Maybe Text)
                                                     , _disabled  :: !(Maybe Bool)
                                                     }
  deriving (Eq, Show, Generic)

localPrinterPropertiesCodec :: TomlCodec LocalPrinterProperties
localPrinterPropertiesCodec = LocalPrinterProperties
  <$> Toml.text "name" .= (_name :: LocalPrinterProperties -> Text)
  <*> Toml.text "port" .= (_port :: LocalPrinterProperties -> Text)
  <*> Toml.text "path" .= (_path :: LocalPrinterProperties -> Text)
  <*> Toml.dioptional (Toml.text "action") .= (_action :: LocalPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.bool "default") .= (_default :: LocalPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.bool "deleteAll") .= (_deleteAll :: LocalPrinterProperties -> Maybe Bool)
  <*> Toml.dioptional (Toml.text "location") .= (_location :: LocalPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.text "comment") .= (_comment :: LocalPrinterProperties -> Maybe Text)
  <*> Toml.dioptional (Toml.bool "disabled") .= (_disabled :: LocalPrinterProperties -> Maybe Bool)

instance XmlPickler LocalPrinterProperties where
  xpickle = xpLocalPrinterProperties

instance Policy Printers where
  path _ = "/CSE/Printers"

instance Default Printers where
  def = Printers "{1F577D12-3D1B-471e-A1B7-060317597B9C}" Nothing [] [] []

xpPrinters :: PU Printers
xpPrinters = xpElem "Printers" $
  xpWrap (\(a,b,c,d,e) -> Printers a b c d e
         ,\(Printers{..}) -> (_clsid, _disabled, _sharedPrinters, _portPrinters, _localPrinters)
         ) $
  xp5Tuple
  (xpAttr "clsid" xpText')
  (xpOption (xpAttr "disabled" xpBool))
  (xpList xpickle)
  (xpList xpickle)
  (xpList xpickle)

xpSharedPrinter :: PU SharedPrinter
xpSharedPrinter = xpElem "SharedPrinter" $
  xpWrap (\(a,b,c,d,e,f,g,h,i,j,k) -> SharedPrinter a b c d e f g h i j k
         ,\SharedPrinter{..} -> (_clsid,_name,_uid,_image,_changed,_desc,_bypassError,_userContext,_removePolicy,_status,_properties)
         ) $
  innerElem xpickle

xpPortPrinter :: PU PortPrinter
xpPortPrinter = xpElem "PortPrinter" $
  xpWrap (\(a,b,c,d,e,f,g,h,i,j,k) -> PortPrinter a b c d e f g h i j k
         ,\PortPrinter{..} -> (_clsid,_name,_uid,_image,_changed,_desc,_bypassError,_userContext,_removePolicy,_status,_properties)
         ) $
  innerElem xpickle

xpLocalPrinter :: PU LocalPrinter
xpLocalPrinter = xpElem "LocalPrinter" $
  xpWrap (\(a,b,c,d,e,f,g,h,i,j,k) -> LocalPrinter a b c d e f g h i j k
         ,\LocalPrinter{..} -> (_clsid,_name,_uid,_image,_changed,_desc,_bypassError,_userContext,_removePolicy,_status,_properties)
         ) $
  innerElem xpickle


xpSharedPrinterProperties :: PU SharedPrinterProperties
xpSharedPrinterProperties = xpElem "Properties" $
  xpWrap (\(a,b,c,d,e,f,g,h,i,j,k,l,m) -> SharedPrinterProperties a b c d e f g h i j k l m
         ,\SharedPrinterProperties{..} -> (_path,_port,_action,_comment,_location,_default,_skipLocal,_deleteAll,_persistent,_deleteMaps,_username,_cpassword,_disabled)
         ) $
  xp13Tuple
  (xpAttr "path" xpText')
  (xpAttr "port" xpText')
  (xpOption (xpAttr "action" xpText'))
  (xpOption (xpAttr "comment" xpText'))
  (xpOption (xpAttr "location" xpText'))
  (xpOption (xpAttr "default" xpBool))
  (xpOption (xpAttr "skipLocal" xpBool))
  (xpOption (xpAttr "deleteAll" xpBool))
  (xpOption (xpAttr "persistent" xpBool))
  (xpOption (xpAttr "deleteMaps" xpBool))
  (xpOption (xpAttr "username" xpText'))
  (xpOption (xpAttr "cpassword" xpText'))
  (xpOption (xpAttr "disabled" xpBool))

xpPortPrinterProperties :: PU PortPrinterProperties
xpPortPrinterProperties = xpElem "Properties" $
  xpWrap (\(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) -> PortPrinterProperties a b c d e f g h i j k l m n o p q r
         ,\PortPrinterProperties{..} -> (_ipAddress,_path,_action,_location,_localName,_comment,_default,_skipLocal,_useDNS,_deleteAll,_lprQueue,_snmpCommunity,_protocol,_portNumber,_doubleSpool,_snmpEnabled,_snmpDevIndex,_disabled)
         ) $
  xp18Tuple
  (xpAttr "ipAddress" xpText')
  (xpAttr "path" xpText')
  (xpOption (xpAttr "action" xpText'))
  (xpOption (xpAttr "location" xpText'))
  (xpOption (xpAttr "localName" xpText'))
  (xpOption (xpAttr "comment" xpText'))
  (xpOption (xpAttr "default" xpBool))
  (xpOption (xpAttr "skipLocal" xpBool))
  (xpOption (xpAttr "useDNS" xpBool))
  (xpOption (xpAttr "deleteAll" xpBool))
  (xpOption (xpAttr "lprQueue" xpText'))
  (xpOption (xpAttr "snmpCommunity" xpText'))
  (xpOption (xpAttr "protocol" xpText'))
  (xpOption (xpAttr "portNumber" xpPrim))
  (xpOption (xpAttr "doubleSpool" xpBool))
  (xpOption (xpAttr "snmpEnabled" xpBool))
  (xpOption (xpAttr "snmpDevIndex" xpPrim))
  (xpOption (xpAttr "disabled" xpBool))

xpLocalPrinterProperties :: PU LocalPrinterProperties
xpLocalPrinterProperties = xpElem "Properties" $
  xpWrap (\(a,b,c,d,e,f,g,h,i) -> LocalPrinterProperties a b c d e f g h i
         ,\LocalPrinterProperties{..} -> (_name,_port,_path,_action,_default,_deleteAll,_location,_comment,_disabled)
         ) $
  xp9Tuple
  (xpAttr "name" xpText')
  (xpAttr "port" xpText')
  (xpAttr "path" xpText')
  (xpOption (xpAttr "action" xpText'))
  (xpOption (xpAttr "default" xpBool))
  (xpOption (xpAttr "deleteAll" xpBool))
  (xpOption (xpAttr "location" xpText'))
  (xpOption (xpAttr "comment" xpText'))
  (xpOption (xpAttr "disabled" xpBool))

innerElem x =
  xp11Tuple
  (xpAttr "clsid" xpText')
  (xpAttr "name" xpText')
  (xpAttr "uid" xpText')
  (xpOption (xpAttr "image" xpPrim))
  (xpOption (xpAttr "changed" xpText'))
  (xpOption (xpAttr "desc" xpText'))
  (xpOption (xpAttr "bypassError" xpBool))
  (xpOption (xpAttr "userContext" xpBool))
  (xpOption (xpAttr "removePolicy" xpBool))
  (xpOption (xpAttr "status" xpText'))
  x
