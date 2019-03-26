module GpoM.Policy.CSE where

import GpoM.Types.Policy

data CSE = Applications
         | ControlPanel
         | DataSources
         | Devices
         | Drives
         | EnvironmentVariables
         | Files
         | FolderOptions
         | Folders
         | IniFiles
         | InternerSettings
         | LocalUsersAndGroups
         | NetworkOptions
         | NetworkShares
         | PowerOptions
         | Printers
{-
instance Policy CSE where
  path Applications = "/Applications"
  path Printers = "/Printers"
  path _ = undefined
-}
