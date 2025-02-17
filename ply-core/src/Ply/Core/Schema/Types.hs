module Ply.Core.Schema.Types (PlySchema (..), PlyDataSchema (..)) where

data PlySchema
  = PlyInt
  | PlyByteStr
  | PlyStr
  | PlyUnit
  | PlyBool
  | PlyListOf PlySchema
  | PlyPairOf PlySchema PlySchema
  | PlyD PlyDataSchema

data PlyDataSchema
  = PlyDS [[PlyDataSchema]]
  | PlyDM PlyDataSchema PlyDataSchema
  | PlyDL PlyDataSchema
  | PlyDI
  | PlyDB
