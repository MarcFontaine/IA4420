----------------------------------------------------------------------------
-- |
-- Module      :  Peil
-- Copyright   :  (c) Marc Fontaine 2016-2017
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--

module Peil
where
import Control.Monad

import API
import GPIO as GPIO
import SPI as SPI


led :: Wire
led = (GPIOC,Pin_13)
spi_nss :: Wire
spi_nss =(GPIOB,Pin_12)
spi_sck :: Wire
spi_sck =(GPIOB,Pin_13)
spi_miso :: Wire       
spi_miso=(GPIOB,Pin_14)
spi_mosi :: Wire       
spi_mosi=(GPIOB,Pin_15)

spiConfig :: SPI.Config
spiConfig = SPI.Config {
    _direction   = Two_Lines_FullDuplex
  , _mode        = Master
  , _dataSize    = Sixteen
  , _CPOL        = Low
  , _CPHA        = OneEdge
  , _NSS         = Soft
  , _baudRatePrescaler = Prescaler_256
  , _firstBit          = MSB
  , _CRCPolynomial     = 7
  }



main :: IO ()       
main = runMI $ do
  initMI
  resetHalt  
  setDefaultClocks
  SPI.deInit SPI2
  peripheralClockOn GPIOB
  peripheralClockOn GPIOC
  peripheralClockOn SPI2
  pinMode led $ GPOutPushPull Mhz_2
  pinMode spi_nss  $ GPIO.GPOutPushPull Mhz_2
  pinMode spi_sck  $ GPIO.AlternateOutPushPull Mhz_2
  pinMode spi_miso $ GPIO.InputPullDown
  pinMode spi_mosi $ GPIO.AlternateOutPushPull Mhz_2
  SPI.init SPI2 spiConfig
  SPI.enable SPI2
  initRF12B
  forM_ (cycle [0,1,2,3,4,5,6,7,8,9,10,8,16,32,64,128]) $ \w -> do
     delay 500000
     pinHigh led
     forM_ [1..1000] $ \c-> sendRFM12B w
     delay 10000
     pinLow led

sendCommand :: Word16 -> MI ()
sendCommand c = do
  pinLow spi_nss 
  sendData SPI2 c
  pinHigh spi_nss 

initRF12B :: MI ()
initRF12B = do
  mapM_ sendCommand [
    0x80D8
   ,0x8239
   ,0xA640
   ,0xC647
   ,0x94A0
   ,0xC2AC
   ,0xCA81
   ,0xCED4
   ,0xC483
   ,0x9850
   ,0xCC77
   ,0xE000
   ,0xC800
   ,0xC040
   ]

sendRFM12B :: Word16 -> MI ()
sendRFM12B b = sendCommand (0xb800+b)
