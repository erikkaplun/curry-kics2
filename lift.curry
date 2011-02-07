import LiftCase
import System
import FlatCurry

main = do 
  [a] <- getArgs 
  prog <- readFlatCurry a
  writeFile (a++"Lifted.fcy") (show $ liftCases True prog)
