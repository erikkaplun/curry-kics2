module Shadowing where

data Name = Name1

Name :: (Name -> Name) -> Name -> Name
Name f Name1 = f Name1
