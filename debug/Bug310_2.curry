module Bug310_2 where

import qualified Bug310 as B (nub)

main = B.nub [()] == [()]