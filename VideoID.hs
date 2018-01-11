----------------------------------------------------------------------------
-- |
-- Module      :  VideoID
-- Copyright   :  (c) Marc Fontaine 2016-2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Paint nice smileys in a waterfall diagram

module VideoID
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

showImg :: [[Char]] -> TRX ()
showImg img = do
  set FrequencyBand433

--  set FrequencyBand868
  set $ OutputPower 7
--  set $ OutputPower 0
  let
    frequencies = [0xa000 .|. x | x <- [1560,1563..2000]]
    on = 33320
    off = 33288
  forM_ img $ \l -> do
     liftIO $ putStrLn l
     set DisableTransmisson
     liftSTL $ do
       pinLow spi_nss 
       sendCommandsDMA $ codeLine frequencies on off l
       delay 500000
       pinHigh spi_nss 
  

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
       ,DMA._Mode           = Circular
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
     _Prescaler   = 720
    ,_CounterMode = Down 
    ,_Period      = 20
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

codeLine :: [Word16] -> Word16 -> Word16 -> [Char] -> [Word16]
codeLine freqs transOn transOff l = cmds
  where
    cmds = concat $ zipWith mkPoint freqs l 
    mkPoint f c =[transOff,f,if c==' ' then transOff else transOn,0,0,0,0]

smiley :: [[Char]]
smiley = reverse $ [
  "    #####    "
 ,"  ##     ##  "
 ," #         # "
 ," #  #   #  # "
 ,"#  ### ###  #"
 ,"#  # # # #  #"
 ,"#           #"
 ,"#           #"
 ,"#  #     #  #"
 ," #  #####  # "
 ," #   ###   # "  
 ,"  ##     ##  "
 ,"    #####    "
  ]
