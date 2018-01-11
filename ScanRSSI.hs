----------------------------------------------------------------------------
-- |
-- Module      :  ScanRSSI
-- Copyright   :  (c) Marc Fontaine 2016-2018
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--
-- Do some stuff and paint a TGA image

module ScanRSSI
where
import TRX
import IA4420
import IA4420_Register
import STM32.API as API
import STM32.GPIO as GPIO
import STM32.DMA as DMA
import STM32.SPI as SPI

import Data.Word
import Data.Bits
import Control.Monad
import Control.Monad.IO.Class
import System.IO
import qualified Data.ByteString as BS

approxRSSI :: TRX ()
approxRSSI = do
  set DisableTransmisson
  set FrequencyBand433
  set AFCAutoOff
  set AFCDeviation_3
  set DisableFrequencyOffsetRegister
  set DisableAFCOffsetCalculation
--  set $ frequencyMHz 438.817450
--  set LNA_0dB
  set EnableReceiverChain
  go RSSI_103dBm
 where 
  go sens = do
    liftIO $ putStr "\r"
    liftIO $ putStr $ show sens
    when ( sens == RSSI_85dBm || sens == RSSI_79dBm || sens == RSSI_73dBm) $ do
      liftIO $ putStrLn $ show  sens
      liftIO $ putStrLn $ show  sens
    set sens
    liftSTL $ delay 300000
    status <- readStatus
    if isRSSI status
      then go $ sensitivityDown sens
      else go $ sensitivityUp sens           

-- [3975..4021]
-- [772 ..856]
defaultScanLevel :: [ConfigConst]
defaultScanLevel = [RSSI_79dBm,RSSI_85dBm,RSSI_91dBm,RSSI_97dBm,RSSI_103dBm]
scanRSSI :: [ConfigConst] -> IO ()
scanRSSI scanLevel = withBinaryFile "/mnt/tmp/t/t.tga" WriteMode $ \handle -> do
 runReset $ do
  liftIO $ BS.hPut handle tgaHeader  
  set DisableTransmisson
  set FrequencyBand433
--  set $ frequencyMHz 438.817450
  set RBW_67
  set RSSI_85dBm
  set LNA_0dB
  set AFCAutoOff
  set AFCDeviation_3
  set DisableFrequencyOffsetRegister
  set DisableAFCOffsetCalculation
  set EnableReceiverChain
                                                       
  forM_ [0..100] $ \iter -> do
    liftIO $ print (iter :: Int)
    line <- forM [0..1023] $ \f -> do
      set $ FrequencyValue f
      approxLevel scanLevel
    
    liftIO $ do
      BS.hPut handle $ BS.concat $ map toColor line
      hFlush handle
      
toColor :: Int -> BS.ByteString
toColor i = case i of
  0 -> BS.pack [0,0,0]
  1 -> BS.pack [0,20,0]
  2 -> BS.pack [0,100,0]
  3 -> BS.pack [0,200,0]
  4 -> BS.pack [0,100,100]
  5 -> BS.pack [0,0,200]
  _ -> BS.pack [255,255,255]
  
approxLevel :: [ConfigConst] -> TRX Int
approxLevel scanLevel = worker 0 scanLevel
  where
    worker i [] = return i
    worker i (l:rest) = do
      set l
      status <- readStatus
      if isRSSI status
         then return i
         else worker (succ i) rest
  

isRSSI :: Word16 -> Bool
isRSSI w = testBit w 8

sensitivityUp :: ConfigConst -> ConfigConst
sensitivityUp r = case r of
   RSSI_103dBm ->  RSSI_103dBm
   RSSI_97dBm  ->  RSSI_103dBm
   RSSI_91dBm  ->  RSSI_97dBm
   RSSI_85dBm  ->  RSSI_91dBm
   RSSI_79dBm  ->  RSSI_85dBm
   RSSI_73dBm  ->  RSSI_79dBm
   x -> x

sensitivityDown :: ConfigConst -> ConfigConst   
sensitivityDown r = case r of
   RSSI_103dBm ->  RSSI_97dBm
   RSSI_97dBm  ->  RSSI_91dBm
   RSSI_91dBm  ->  RSSI_85dBm
   RSSI_85dBm  ->  RSSI_79dBm
   RSSI_79dBm  ->  RSSI_73dBm
   RSSI_73dBm  ->  RSSI_73dBm
   x -> x


tgaHeader :: BS.ByteString
tgaHeader = BS.pack [
  0,0,2
 ,0,0,0,0,0
 ,0,0,0,0 -- x0 y0
 ,0,4  --width
 ,0,4  -- hight
 ,24  --Bits-per-pixel
 ,16
 ]
            
sendCommandsDMA :: [Word16] -> MI ()
sendCommandsDMA cmds = do
  let
      dmaBuffer = 0x20001000 
      dmaConfig = DMA.Config {
        _BufferSize         = fromIntegral $ length cmds
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

  peripheralClockOn DMA1
  DMA.deInit DMA1_Channel5
  writeMem8 dmaBuffer $ packCmds cmds
  bitSet SPI2 CR2_TXDMAEN
  
  DMA.disable DMA1_Channel5
  DMA.init DMA1_Channel5 dmaConfig
  pinLow spi_nss 
  DMA.enable DMA1_Channel5

packCmds :: [Word16] -> BS.ByteString
packCmds = BS.pack . concatMap lowHigh
  where
    lowHigh x = [fromIntegral (x .&. 0xff),fromIntegral $ x `shiftR` 8]  

sweepCircular :: [Word16]
sweepCircular = map (\x -> 0xa000 .|. x) $ concat [
     l
    ,reverse l
  ]
  where l = [1000..2000]
