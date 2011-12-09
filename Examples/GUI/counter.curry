------------------------------------------------------------------------------
-- A simple counter demo for the GUI library
------------------------------------------------------------------------------

import GUI
import Read

counterGUI =
 col [
   Label [Text "A simple counter:"],
   Entry [WRef val, Text "0", Background "yellow"],
   row [Button (updateValue incrText val) [Text "Increment"],
        Button (setValue val "0")         [Text "Reset"],
        Button exitGUI                    [Text "Stop"]]]
 where
   val free

   incrText s = show (readInt s + 1)

main = runGUI "Counter Demo" counterGUI

