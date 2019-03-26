module GpoM.Policy.CSE.Printers where

import GpoM.Types.Policy
import Data.Default
import Data.Word (Word8)
import GHC.Generics
import Text.XML.HXT.Arrow.Pickle
import GpoM.Policy.CSE.Parser.Common (xpBool, xpUByte)

data Printers = Printers { _clsid          :: String
                         , _disabled       :: Maybe Bool
                         , _sharedPrinters :: [SharedPrinter]
                         , _portPrinters   :: [PortPrinter]
                         , _localPrinters  :: [LocalPrinter]
                         }
  deriving (Eq, Show, Generic)

instance XmlPickler Printers where
  xpickle = xpPrinters

data SharedPrinter = SharedPrinter { _clsid        :: String
                                   , _name         :: String
                                   , _uid          :: String
                                   , _image        :: Maybe Word8
                                   , _changed      :: Maybe String
                                   , _desc         :: Maybe String
                                   , _bypassError  :: Maybe Bool
                                   , _userContext  :: Maybe Bool
                                   , _removePolicy :: Maybe Bool
                                   , _status       :: Maybe String
                                   , _properties   :: SharedPrinterProperties
                                   }
  deriving (Eq, Show, Generic)

instance XmlPickler SharedPrinter where
  xpickle = xpSharedPrinter


data SharedPrinterProperties = SharedPrinterProperties { _path :: String
                                                       , _port :: String
                                                       , _action :: Maybe String
                                                       , _comment :: Maybe String
                                                       , _location :: Maybe String
                                                       , _default :: Maybe Bool
                                                       , _skipLocal :: Maybe Bool
                                                       , _deleteAll :: Maybe Bool
                                                       , _persistent :: Maybe Bool
                                                       , _deleteMaps :: Maybe Bool
                                                       , _username :: Maybe String
                                                       , _cpassword :: Maybe String
                                                       , _disabled :: Maybe Bool
                                                       }
  deriving (Eq, Show, Generic)

instance XmlPickler SharedPrinterProperties where
  xpickle = xpSharedPrinterProperties


data PortPrinter = PortPrinter { _clsid :: String
                               , _name         :: String
                               , _uid          :: String
                               , _image        :: Maybe Word8
                               , _changed      :: Maybe String
                               , _desc         :: Maybe String
                               , _bypassError  :: Maybe Bool
                               , _userContext  :: Maybe Bool
                               , _removePolicy :: Maybe Bool
                               , _status       :: Maybe String
                               , _properties   :: PortPrinterProperties
                               }
  deriving (Eq, Show, Generic)

instance XmlPickler PortPrinter where
  xpickle = xpPortPrinter

data PortPrinterProperties = PortPrinterProperties { _ipAddress :: String
                                                   , _path :: String
                                                   , _action :: Maybe String
                                                   , _location :: Maybe String
                                                   , _localName :: Maybe String
                                                   , _comment :: Maybe String
                                                   , _default :: Maybe Bool
                                                   , _skipLocal :: Maybe Bool
                                                   , _useDNS :: Maybe Bool
                                                   , _deleteAll :: Maybe Bool
                                                   , _lprQueue :: Maybe String
                                                   , _snmpCommunity :: Maybe String
                                                   , _protocol :: Maybe String
                                                   , _portNumber :: Maybe Int
                                                   , _doubleSpool :: Maybe Bool
                                                   , _snmpEnabled :: Maybe Bool
                                                   , _snmpDevIndex :: Maybe Word8
                                                   , _disabled :: Maybe Bool
                                                   }
  deriving (Eq, Show, Generic)

instance XmlPickler PortPrinterProperties where
  xpickle = xpPortPrinterProperties

data LocalPrinter = LocalPrinter { _clsid        :: String
                                 , _name         :: String
                                 , _uid          :: String
                                 , _image        :: Maybe Word8
                                 , _changed      :: Maybe String
                                 , _desc         :: Maybe String
                                 , _bypassError  :: Maybe Bool
                                 , _userContext  :: Maybe Bool
                                 , _removePolicy :: Maybe Bool
                                 , _status       :: Maybe String
                                 , _properties   :: LocalPrinterProperties
                                 }
  deriving (Eq, Show, Generic)

instance XmlPickler LocalPrinter where
  xpickle = xpLocalPrinter

data LocalPrinterProperties = LocalPrinterProperties { _name :: String
                                                     , _port :: String
                                                     , _path :: String
                                                     , _action :: Maybe String
                                                     , _default :: Maybe Bool
                                                     , _deleteAll :: Maybe Bool
                                                     , _location :: Maybe String
                                                     , _comment :: Maybe String
                                                     , _disabled :: Maybe Bool
                                                     }
  deriving (Eq, Show, Generic)

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
  (xpAttr "clsid" xpText)
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
  (xpAttr "path" xpText)
  (xpAttr "port" xpText)
  (xpOption (xpAttr "action" xpText))
  (xpOption (xpAttr "comment" xpText))
  (xpOption (xpAttr "location" xpText))
  (xpOption (xpAttr "default" xpBool))
  (xpOption (xpAttr "skipLocal" xpBool))
  (xpOption (xpAttr "deleteAll" xpBool))
  (xpOption (xpAttr "persistent" xpBool))
  (xpOption (xpAttr "deleteMaps" xpBool))
  (xpOption (xpAttr "username" xpText))
  (xpOption (xpAttr "cpassword" xpText))
  (xpOption (xpAttr "disabled" xpBool))

xpPortPrinterProperties :: PU PortPrinterProperties
xpPortPrinterProperties = xpElem "Properties" $
  xpWrap (\(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r) -> PortPrinterProperties a b c d e f g h i j k l m n o p q r
         ,\PortPrinterProperties{..} -> (_ipAddress,_path,_action,_location,_localName,_comment,_default,_skipLocal,_useDNS,_deleteAll,_lprQueue,_snmpCommunity,_protocol,_portNumber,_doubleSpool,_snmpEnabled,_snmpDevIndex,_disabled)
         ) $
  xp18Tuple
  (xpAttr "ipAddress" xpText)
  (xpAttr "path" xpText)
  (xpOption (xpAttr "action" xpText))
  (xpOption (xpAttr "location" xpText))
  (xpOption (xpAttr "localName" xpText))
  (xpOption (xpAttr "comment" xpText))
  (xpOption (xpAttr "default" xpBool))
  (xpOption (xpAttr "skipLocal" xpBool))
  (xpOption (xpAttr "useDNS" xpBool))
  (xpOption (xpAttr "deleteAll" xpBool))
  (xpOption (xpAttr "lprQueue" xpText))
  (xpOption (xpAttr "snmpCommunity" xpText))
  (xpOption (xpAttr "protocol" xpText))
  (xpOption (xpAttr "portNumber" xpPrim))
  (xpOption (xpAttr "doubleSpool" xpBool))
  (xpOption (xpAttr "snmpEnabled" xpBool))
  (xpOption (xpAttr "snmpDevIndex" xpUByte))
  (xpOption (xpAttr "disabled" xpBool))

xpLocalPrinterProperties :: PU LocalPrinterProperties
xpLocalPrinterProperties = xpElem "Properties" $
  xpWrap (\(a,b,c,d,e,f,g,h,i) -> LocalPrinterProperties a b c d e f g h i
         ,\LocalPrinterProperties{..} -> (_name,_port,_path,_action,_default,_deleteAll,_location,_comment,_disabled)
         ) $
  xp9Tuple
  (xpAttr "name" xpText)
  (xpAttr "port" xpText)
  (xpAttr "path" xpText)
  (xpOption (xpAttr "action" xpText))
  (xpOption (xpAttr "default" xpBool))
  (xpOption (xpAttr "deleteAll" xpBool))
  (xpOption (xpAttr "location" xpText))
  (xpOption (xpAttr "comment" xpText))
  (xpOption (xpAttr "disabled" xpBool))

innerElem x =
  xp11Tuple
  (xpAttr "clsid" xpText)
  (xpAttr "name" xpText)
  (xpAttr "uid" xpText)
  (xpOption (xpAttr "image" xpUByte))
  (xpOption (xpAttr "changed" xpText))
  (xpOption (xpAttr "desc" xpText))
  (xpOption (xpAttr "bypassError" xpBool))
  (xpOption (xpAttr "userContext" xpBool))
  (xpOption (xpAttr "removePolicy" xpBool))
  (xpOption (xpAttr "status" xpText))
  x
