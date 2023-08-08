module Pattern.ThreeLayer.Layer3 where

newtype Name = Name String
 
getName :: Name -> String
getName (Name s) = s