module Pattern.FourLayer.Core where

 -- Layer 4 (Core)
 -- Three Layer Cake では Layer 3 に相当する
newtype Name = Name String
 
getName :: Name -> String
getName (Name s) = s