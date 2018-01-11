----------------------------------------------------------------------------
-- |
-- Module      :  IA4420_descr
-- Copyright   :  (c) Marc Fontaine 2016-2018
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Generate a Haskell module from a hardware description of the IA4420 IC.
-- (The description is also included and uses a proprietary format).

{-# LANGUAGE BinaryLiterals #-}
module IA4420_descr
where

import Data.Maybe
import Data.Word
import Text.PrettyPrint
import Numeric

data Command = Command {
   _prefix   :: Word16
  ,_por      :: Word16
  ,_cmdName  :: String
  ,_cmdDescr :: String
  ,_cmdFields :: [Field]
  } deriving Show

data Field = Field {
   _fieldOffset :: Int
  ,_fieldWidth :: Int
  ,_fieldName :: String
  ,_fieldType :: String
  ,_fieldConsts :: [(String,Word16)]
  } deriving Show

-- | Generate the Haskell module.
gen :: String
gen = render hsModule
  where
     hsModule = vcat [
        text "{-#LANGUAGE GADTs #-}"
       ,text "-- auto-generated"
       ,text "-- do not edit"
       ,text "module IA4420_Register"
       ,text "where"
       ,text "import Data.Word"
       ,blankLine
       ,dataTable "Command" ""
          $ map (\c -> (text $ _cmdName c, text "Command"))
            commands             
       ,nest 4 $ text "deriving (Show,Read,Eq,Ord,Enum,Bounded)"
       ,blankLine
       ,text "commandPrefix :: Command -> Word16"
       ,funTable "commandPrefix"
          $ map ( \x ->(text $ _cmdName x
                       ,text "0x" <> (text $ showHex (_prefix x) "")))
          commands
       ,blankLine
       ,text "commandPOR :: Command -> Word16"
       ,funTable "commandPOR"
          $ map ( \x ->(text $ _cmdName x
                       ,text "0x" <> (text $ showHex (_por x) "")))
          commands
       ,blankLine
       ,dataTable "ConfigField" ""
          $ map (\f -> (text $ _fieldName f, text "ConfigField" ))
            fields
       ,nest 4 $ text "deriving (Show,Read,Eq,Ord)"
       ,blankLine
       ,text "fieldCommand :: ConfigField -> Command"
       ,funTable "fieldCommand"
          $ map ( \(c,f) ->(text $ _fieldName f
                           ,text $ _cmdName c))
          commandFields
       ,blankLine
          
       ,blankLine
       ,text "fieldOffset :: ConfigField -> Int"
       ,funTable "fieldOffset"
          $ map ( \(_,f) ->(text $ _fieldName f
                           ,int $ _fieldOffset f))
          commandFields
       ,blankLine
       ,text "fieldWidth :: ConfigField -> Int"
       ,funTable "fieldWidth"
          $ map ( \(_,f) ->(text $ _fieldName f
                           ,int $ _fieldWidth f))
          commandFields
       ,blankLine
       ,dataTable "ConfigConst" ""
          $ map (\(_,(c,_)) -> (text c, text "ConfigConst" ))
            fieldConstants
       ,nest 4 $ text "deriving (Show,Read,Eq,Ord)"
       ,blankLine
       ,text "configConstField :: ConfigConst -> ConfigField"
       ,funTable "configConstField"
          $ map ( \(f,(c,_)) ->(text c,text $ _fieldName f)) fieldConstants
       ,blankLine
       ,text "configConstValue :: ConfigConst -> Word16"
       ,funTable "configConstValue"
          $ map ( \(_,(c,v)) ->(text c,int $ fromIntegral v)) fieldConstants
       ,blankLine        
       ]                

     blankLine :: Doc
     blankLine = text ""
     

dataTable :: String -> String -> [(Doc,Doc)] -> Doc
dataTable dataName kind constrs
    = vcat [
       text "data" <+> text dataName <+> text kind <+> text "where"
      ,vcat $ map mkConstr constrs
      ]
  where
    mkConstr (c, t)
      =nest 4 $ (c <+> colon <> colon <+> t)

funTable :: String -> [(Doc,Doc)] -> Doc
funTable funName assocs 
    = vcat $ map mkCase assocs
  where
    mkCase (argument, value) = hsep [
       text funName
      ,argument
      ,equals
      ,value
      ]



cmd :: Int -> (Int,Int-> Maybe Field)
cmd x = (x, const Nothing)

tf :: Int -> String -> (Int,Int -> Maybe Field)
tf size name = cf size name []
  
cf :: Int -> String -> [(String,Word16)] -> (Int,Int -> Maybe Field)
cf size name consts= (size,f)
  where
    f x = Just $ Field {
       _fieldOffset = x 
      ,_fieldWidth = size
      ,_fieldName  = name
      ,_fieldType  = name
      ,_fieldConsts  = consts
    }


binaryField :: String -> String -> String -> (Int,Int -> Maybe Field)
binaryField name high low = (1,f)
  where
    f x = Just $ Field {
       _fieldOffset = x 
      ,_fieldWidth = 1
      ,_fieldName  = name
      ,_fieldType  = "__undef__"
      ,_fieldConsts = [(high,1),(low,0)]
      }

highLow :: String -> String -> (Int,Int -> Maybe Field)
highLow high low
  = binaryField (high ++"Field") high low

enableDisable :: String -> (Int,Int -> Maybe Field)
enableDisable n
  = binaryField n ("Enable" ++ n) ("Disable" ++ n)

ed :: String -> (Int,Int -> Maybe Field)
ed = enableDisable

bf :: String -> (Int,Int -> Maybe Field)
bf = enableDisable
     
asmFields :: [(Int,Int-> Maybe Field)] -> [Field]
asmFields = catMaybes . worker 16
  where
    worker 0 [] = []
    worker _ [] = error "more or less than 16 bits defined"
    worker offset ((cnt,f):rest)
       = (f $ offset -cnt) : worker (offset -cnt) rest

commands :: [Command]
commands
  = [configurationSetting, powerManagement, frequencySetting, dataRate
    ,receiverControl, dataFilter, fifoAndReset, receiverFifoRead, afc
    ,txConfiguration, transmitterRegisterWrite, wakeUpTimer, lowDutyCycle
    ,lowBatteryDetector, statusRead]

fields :: [Field]
fields = concatMap _cmdFields commands

fieldConstants :: [(Field,(String,Word16))]
fieldConstants = concatMap (\f -> map (\c -> (f,c)) $ _fieldConsts f) fields

commandFields :: [(Command,Field)]
commandFields = concatMap (\c -> map (\f -> (c,f)) $ _cmdFields c) commands


configurationSetting :: Command
configurationSetting
  = Command {
   _prefix   =0x8000
  ,_por      =0x8008
  ,_cmdName  ="ConfigurationSetting"
  ,_cmdDescr ="configure fifos,fequency band, and crystal load capacitor"  
  ,_cmdFields = asmFields [
     cmd 8
    ,ed   "InternalDataRegister"
    ,bf   "FifoMode"
    ,cf 2 "FrequencyBand" [
             ("FrequencyBand315", 0b00)
            ,("FrequencyBand433", 0b01)
            ,("FrequencyBand868", 0b10)
            ,("FrequencyBand915", 0b11)]
    ,tf 4 "CrystalLoadCapacitor"
    ]
  }
    
powerManagement :: Command
powerManagement
  = Command {
   _prefix   =0x8200
  ,_por      =0x8208
  ,_cmdName  ="PowerManagement"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 8
    ,ed "ReceiverChain"
    ,ed "BaseBand"
    ,ed "Transmisson"
    ,ed "Synthesizer"
    ,ed "CrystalOscillator"
    ,ed "LowBatteryDetector"
    ,ed "WakeUpTimer"
    ,highLow "DisableClockOutput" "EnableClockOutput"
    ]
  }

frequencySetting :: Command
frequencySetting
  = Command {
   _prefix   =0xA000
  ,_por      =0xA680
  ,_cmdName  ="FrequencySetting"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 4
    ,tf 12 "Frequency"
    ]
  }

dataRate :: Command
dataRate
  = Command {
   _prefix   =0xC600
  ,_por      =0xC623
  ,_cmdName  ="DataRate"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 8
    ,bf "BitRatePrescalar"
    ,tf 7 "BitRate"
    ]
  }

receiverControl :: Command
receiverControl
  = Command {
   _prefix   =0x9000
  ,_por      =0x9080
  ,_cmdName  ="ReceiverControl"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 5
    ,bf   "Pin16Control"
    ,cf 2 "ValidDataIndicatorResponseTime" [
             ("VDIFast"    ,0b00)
            ,("VDIMedium"  ,0b01)
            ,("VDISlow"    ,0b10)
            ,("VDIAlwaysOn",0b11)
            ]
    ,cf 3 "ReceiverBandWidth" [
             ("RBW_400",0b001)
            ,("RBW_340",0b010)
            ,("RBW_270",0b011)
            ,("RBW_200",0b100)
            ,("RBW_134",0b101)
            ,("RBW_67",0b0110)
            ]
    ,cf 2 "LNAGain" [
             ("LNA_0dB" ,0b00)
            ,("LNA_6dB" ,0b01)
            ,("LNA_14dB",0b10)
            ,("LNA_20dB",0b11)
            ]
    ,cf 3 "RSSIThreshold" [
             ("RSSI_103dBm",0b000)
            ,("RSSI_97dBm" ,0b001)
            ,("RSSI_91dBm" ,0b010)
            ,("RSSI_85dBm" ,0b011)
            ,("RSSI_79dBm" ,0b100)
            ,("RSSI_73dBm" ,0b101)             
            ]
    ]
  }

dataFilter :: Command
dataFilter
  = Command {
   _prefix   =0xC200
  ,_por      =0xC22C
  ,_cmdName  ="DataFilter"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 8
    ,bf "ClockRecoveryAutoLockControl"
    ,bf "ClockRecoveryLockControl"
    ,cmd 1
    ,bf "FilterMode"
    ,cmd 1
    ,tf 3 "DataFilterCommandUnknown"
    ]
  }

fifoAndReset :: Command
fifoAndReset
  = Command {
   _prefix   =0xCA00
  ,_por      =0xCA80
  ,_cmdName  ="FifoAndReset"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 8
    ,tf 4 "FifoITLevel"
    ,cmd 1
    ,bf "FifoStartCondition"
    ,bf "FifoFill"
    ,highLow "DisableSensitiveReset" "EnableSensitiveReset"
    ]
  }

receiverFifoRead :: Command
receiverFifoRead
  = Command {
   _prefix   =0xB000
  ,_por      =0xB000
  ,_cmdName  ="ReceiverFifoRead"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [cmd 16]
  }

afc :: Command
afc
  = Command {
   _prefix   =0xC400
  ,_por      =0xC4F7
  ,_cmdName  ="AFC"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 8
    ,cf 2 "AFCOperationMode" [
            ("AFCAutoOff"    , 0b00)
           ,("AFCRunOnce"    , 0b01)
           ,("AFCKeepOffsetOnlyReceiving", 0b10)
           ,("AFCKeepOffset"             , 0b11)
           ]
    ,cf 2 "AFCMaxDevitation" [
            ("AFCDeviationUnrestricted", 0b00)
           ,("AFCDeviation_15", 0b01)
           ,("AFCDeviation_7", 0b10)
           ,("AFCDeviation_3", 0b11)
           ]
    ,cf 1 "AFCStrobeField" [("AFCStrobe",1)]
    ,bf   "AFCFineMode"
    ,ed   "FrequencyOffsetRegister"
    ,ed   "AFCOffsetCalculation"
    ]
  }

txConfiguration :: Command
txConfiguration
  = Command {
   _prefix   =0x9800
  ,_por      =0x9800
  ,_cmdName  ="TxConfiguration"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 7
    ,binaryField  "FSKPolarity" "FSKPolarityNormal" "FSKPolarityReverse"
    ,tf 4 "FSKShift"
    ,tf 4 "RelativeOutputPower"
    ]
  }

transmitterRegisterWrite :: Command
transmitterRegisterWrite
  = Command {
   _prefix   =0xB800
  ,_por      =0xB8AA
  ,_cmdName  ="TransmitterRegisterWrite"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 8
    ,tf 8 "TxData"
    ]
  }

wakeUpTimer :: Command
wakeUpTimer
  = Command {
   _prefix   =0xE000
  ,_por      =0xE196
  ,_cmdName  ="WakeUpTimerCommand"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 3
    ,tf 5 "WakeUpTimeExponent"
    ,tf 8 "WakeUpTimeBase"]
  }

lowDutyCycle :: Command
lowDutyCycle
  = Command {
   _prefix   =0xC800
  ,_por      =0xC80E
  ,_cmdName  ="LowDutyCycle"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 8
    ,tf 7 "DutyCycle"
    ,ed   "LowDutyCyleMode"
    ]
  }

lowBatteryDetector :: Command
lowBatteryDetector
  = Command {
   _prefix   =0xC000
  ,_por      =0xC000
  ,_cmdName  ="LowBatteryDetectorCommand"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [
     cmd 8
    ,cf 3 "ClockOutputFrequency" [
            ("ClockOut_1000kHz",  0b000)
           ,("ClockOut_1250kHz",  0b001)
           ,("ClockOut_1660kHz",  0b010)
           ,("ClockOut_2000kHz",  0b011)
           ,("ClockOut_2500kHz",  0b100)
           ,("ClockOut_3330kHz",  0b101)
           ,("ClockOut_5000kHz",  0b110)
           ,("ClockOut_10000kHz",  0b111)
           ]
    ,tf 5 "LowBatteryThresholdVoltage"]
  }

statusRead :: Command
statusRead
  = Command {
   _prefix   =0x0000
  ,_por      =0x0000
  ,_cmdName  ="StatusRead"
  ,_cmdDescr =""
  ,_cmdFields = asmFields [cmd 16]
  }
