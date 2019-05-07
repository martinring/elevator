module Codec where 

import Data.Aeson
import Messages

-----------------------------------------------------------------------------
-- Der folgende Code dient der JSON (De)serialisierung und                 --
-- ist nicht wichtig für das lösen der Aufgabe.                            --
  

instance ToJSON Direction
instance FromJSON Direction
instance FromJSON Event
instance ToJSON Command

