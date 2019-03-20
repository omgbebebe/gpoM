module Types where

import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Set (Set)
import qualified Data.Set as S


type GUID = Text
type PolicyFiles = Set (FilePath, ByteString)

class Policy a where
  polName :: a -> Text
  polGuid :: a -> GUID
  polFiles :: a -> [FilePath]
  polRead :: PolicyFiles -> a
  polWrite :: a -> PolicyFiles

data EnvVars = EnvVars{ name :: Text
                      , guid :: GUID
                      , files :: PolicyFiles
                      }
  deriving (Eq, Show)

instance Policy EnvVars where
  polName = name
  polGuid = guid
  polFiles = S.foldr ((:) . fst) [] . files
  polRead = undefined
  polWrite = undefined

evPol = EnvVars {name = "Preference CSE GUID Environment Variables"
                ,guid = "0E28E245-9368-4853-AD84-6DA3BA35BB75"
                ,files = S.fromList [("/User/Preferences/EnvironmentVariables/EnvironmentVariables.xml", BS.empty)]
                }
