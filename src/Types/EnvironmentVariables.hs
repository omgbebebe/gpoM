{-# LANGUAGE DuplicateRecordFields #-}
module Types.EnvironmentVariables where

import Data.Time.LocalTime(ZonedTime)
import Data.ByteString.Char8 as BS
import Data.Int(Int64)
--import Data.Time.ISO8601.Duration
--import FromXML
import Data.Time.Calendar(Day)
import Data.Time.Clock
import Xeno.DOM as Xeno

type XMLString = String
type XsUnsignedByte = String

data Properties =
    Properties {
             disabled :: Maybe Bool
           , partial :: Maybe Bool
           , user :: Maybe Bool
           , value :: XMLString
           , name :: XMLString
           , action :: Maybe XMLString
           }

data EnvironmentVariable =
    EnvironmentVariable {
                      removePolicy :: Maybe Bool
                    , userContext :: Maybe Bool
                    , bypassErrors :: Maybe Bool
                    , desc :: Maybe XMLString
                    , uid :: XMLString
                    , changed :: Maybe XMLString
                    , image :: Maybe XsUnsignedByte
                    , name :: XMLString
                    , clsid :: XMLString
                    , properties :: Properties
                    }

data EnvironmentVariables =
    EnvironmentVariables {
                       disabled :: Maybe Bool
                     , clsid :: XMLString
                     , environmentVariable :: [EnvironmentVariable]
                     }
type TopLevel = EnvironmentVariables
