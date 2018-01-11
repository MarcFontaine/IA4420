----------------------------------------------------------------------------
-- |
-- Module      :  OOK
-- Copyright   :  (c) Marc Fontaine 2016-2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- On-Off keying for remote controlled garage doors, etc
--
module OOK
where
import TRX
import IA4420
import IA4420_Register
import STM32.API as API
import STM32.GPIO as GPIO
import STM32.Timer as Timer
import STM32.DMA as DMA

import Control.Monad
import Control.Monad.IO.Class
import qualified Data.ByteString as BS

ookFrequency :: FrequencyValue
ookFrequency = frequencyMHz $
   433.9075  -- soll frequenz
 + 0.015     -- FSK-hub 15KHz (senden immer 0)
 + 0.017     -- korrektor für diesen chip
 + 0.0025
 
sendCommandsDMA :: [Word16] -> MI ()
sendCommandsDMA cmds = do
  let len = length cmds
      dmaBuffer = 0x20001000 
      dmaConfig = DMA.Config {
        _BufferSize         = fromIntegral $ len
       ,_Direction          = PeripheralDST
       ,_MemoryBaseAddr     = dmaBuffer
       ,_MemoryDataSize     = HalfWord
       ,_MemoryInc          = True
       ,DMA._Mode           = Normal
       ,_PeripheralBaseAddr = regToAddr SPI2 DR
       ,_PeripheralDataSize = HalfWord
       ,_PeripheralInc      = False
       ,_Priority           = DMA.Low
      }
  writeMem8 dmaBuffer $ packCmds cmds

  peripheralClockOn DMA1
  peripheralClockOn TIM4
  DMA.deInit DMA1_Channel7
  
  DMA.disable DMA1_Channel7
  DMA.init DMA1_Channel7 dmaConfig
  DMA.enable DMA1_Channel7

  let timeBase = TimeBase {
     _Prescaler   = 18  -- 72 Mhz /4 weil 4 cmds pro bit
    ,_CounterMode = Down 
    ,_Period      = 1394
    ,_ClockDevision = CKD_DIV1
    ,_RepetitionCounter  =0
    }

  Timer.deInit TIM4
  Timer.timeBaseInit TIM4 timeBase 
  bitReset TIM4 CR1_URS
  bitSet TIM4 DIER_UDE
  bitSet TIM4 CR1_CEN

packCmds :: [Word16] -> BS.ByteString
packCmds = BS.pack . concatMap lowHigh
  where
    lowHigh x = [fromIntegral (x .&. 0xff),fromIntegral $ x `shiftR` 8]  


sendCode :: [Char] -> TRX ()
sendCode code = do
  set DisableTransmisson
  set FrequencyBand433
  set AFCAutoOff
  set AFCDeviation_3
  set DisableFrequencyOffsetRegister
  set DisableAFCOffsetCalculation
  set FSKPolarityNormal
  set $ FieldSetter FSKShift 0
  set $ FieldSetter TxData 0
  set $ FieldSetter TxData 0
  set $ OutputPower 7
--  set $ OutputPower 0 --max power
  {-
    soll 433.908
    we send with 15kHz fsk-shift
  -}
  set ookFrequency 
  replicateM_ 100 $ do
     liftIO $ print "ping"
     liftSTL $ do
       pinLow spi_nss 
       sendCommandsDMA $ codeLine code
       delay 500000
       pinHigh spi_nss 
  set DisableTransmisson


codeLine :: [Char] -> [Word16]
codeLine code = cmds
  where
    cmds = concatMap mkBit code
    mkBit b =[if b=='1' then 33320 else 33288,0,0,0]


-- | Don't worry this will probably not open YOUR garage door ! 
tuerCode :: [Char]
tuerCode = code ++ pause ++ code ++ "00000000"
  where
    code = concat [
      "10101010","10101010","10101010"  -- AA AA AA
     ,"00111100","01001010","10101010"  -- 3C 4A AA                          
     ,"10110100","10110100"             -- B4 B4               
     ,"10110010","10101011"             -- B2 AB
     ,"01001011","00110101"             -- 4B 35
     ]
    pause = replicate 72 '0'
   
