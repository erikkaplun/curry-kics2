module Base where

data NDClass
  = ND  -- non-deterministic function
  | DHO -- deterministic higher-order function
  | DFO -- deterministic first-order function
