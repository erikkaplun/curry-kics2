-- a simple mail widget to write and send an email

import GUI
import Mail

mailWidget =
 col [Label [Text "A simple mail composer:"],
      Matrix [LeftAlign] [[Label [Text "From:"],
                           Entry [WRef rfrom, FillX]],
                          [Label [Text "To:"],
                           Entry [WRef rto, FillX]],
                          [Label [Text "Subject:"],
                           Entry [WRef rsubject, FillX]]],
      TextEdit [WRef rtxt, Background "yellow", Fill],
      row [Button sendmail [Text "Send"],
           Button exitGUI  [Text "Cancel"]]
   ]
 where
   rfrom,rto,rsubject,rtxt free

   sendmail gport =
    do from <- getValue rfrom gport
       to <- getValue rto gport
       subject <- getValue rsubject gport
       body <- getValue rtxt gport
       sendMail from to subject body
       exitGUI gport

main = runGUI "Mail Demo" mailWidget


