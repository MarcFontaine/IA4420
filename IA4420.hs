----------------------------------------------------------------------------
-- |
-- Module      :  IA442
-- Copyright   :  (c) Marc Fontaine 2016-2018
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- 

module IA4420
(
    module IA4420
  , module IA4420_Register
)
where

import IA4420_Register

import Data.Word
import Data.Bits
import Data.Map (Map)
import qualified Data.Map as Map

newtype Config = Config {_unConfig ::Map Command Word16}
  deriving (Show,Read)

porConfig :: Config
porConfig
  = Config $ Map.fromList
      $ map (\c -> (c, commandPOR c)) [minBound..maxBound]

class ConfigValue a where
  toField :: a -> ConfigField
  toBits  :: a -> Word16

instance ConfigValue ConfigConst where
  toField = configConstField
  toBits  = configConstValue

setBitField :: Int -> Int -> Word16 -> Word16 -> Word16
setBitField offset width w n = (w .&. mask) .|. n'
  where
    mask = complement ((0xffff `shiftR` (16-width)) `shiftL` offset)
    n' = n `shiftL` offset
    
setConfigValue :: ConfigValue a => a -> Config -> (Config, Word16)
setConfigValue f (Config conf) = (Config conf', commandWord)
  where
    field = toField f
    cmd = fieldCommand field
    commandWord = setBitField
      (fieldOffset field)
      (fieldWidth field)
      ((Map.!) conf cmd)
      (toBits f)
    conf' = Map.insert cmd commandWord conf

newtype OutputPower = OutputPower (Word16)
  deriving (Show,Read,Eq)

instance ConfigValue OutputPower where
  toField _ = RelativeOutputPower
  toBits (OutputPower w) = w


newtype FrequencyValue = FrequencyValue (Word16)
  deriving (Show,Read,Eq)

instance ConfigValue FrequencyValue where
  toField _ = IA4420_Register.Frequency
  toBits (FrequencyValue w) = w

frequencyMHz :: Double -> FrequencyValue
frequencyMHz d = FrequencyValue $ round $ ((d-430)*1000/2.5)

data FieldSetter = FieldSetter ConfigField Word16
  deriving (Show,Read,Eq)

instance ConfigValue FieldSetter where
  toField (FieldSetter f _) = f
  toBits (FieldSetter _ w) = w
           
