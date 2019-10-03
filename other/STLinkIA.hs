module STLinkIA
where

import STM32.STLinkUSB
import IA4420
import IA4420_Register
import STM32.API as API
import STM32.GPIO as GPIO
import STM32.SPI as SPI
import STM32.STLinkUSB.Env (runSTLink', defaultDebugLogger)

import Control.Monad.IO.Class
import Control.Monad.Trans.RWS as RWS
import Control.Monad.Trans.Reader

type TRX a = RWST STLinkEnv () IA4420.Config IO a

stateFile :: FilePath
stateFile = "/mnt/tmp/t/ia4420.state"
runReset :: TRX a -> IO a
runReset action = runTRX $ do
  liftSTL $ initSPI
  resetTRX stateFile
  action

runWarm :: TRX a -> IO a
runWarm action = runTRX $ do
  cfg <- liftIO $ readFile stateFile
  liftIO $ print cfg -- need to force
  put $ read cfg
  res <- action
  conf <- get
  liftIO $ writeFile stateFile $ show conf
  return res
  
runTRX :: TRX a -> IO a
runTRX a = runSTLink' defaultDebugLogger $ runTRX' a

runTRX' :: TRX a -> (STLinkEnv -> IO a)
runTRX' action stLinkEnv = do
  (a,_,_) <- runRWST action stLinkEnv (error "no IA4420 State available")
  return a

liftSTL :: ReaderT STLinkEnv IO a -> TRX a
liftSTL a = do
  env <- RWS.ask
  liftIO $ runReaderT a env
  
resetTRX :: FilePath -> TRX ()
resetTRX fp = do
  -- todo hw reset
  put porConfig
  liftIO $ writeFile fp $ show porConfig

set :: ConfigValue a => a -> TRX ()
set a = do
  conf <- get
  let (conf',cmd) = setConfigValue a conf
  liftSTL $ sendSPI cmd
  put conf'

sendSPI :: Word16 -> MI ()
sendSPI c = do
  pinLow spi_nss 
  sendData SPI2 c
  pinHigh spi_nss 

readStatus :: TRX Word16
readStatus = liftSTL $ do
  pinLow spi_nss
  _<-receiveData SPI2 -- clear buffer
  sendData SPI2 0
  x <- receiveData SPI2
  pinHigh spi_nss 
  return x
           
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
  , _baudRatePrescaler = Prescaler_8
  , _firstBit          = MSB
  , _CRCPolynomial     = 7
  }


initSPI :: MI ()
initSPI = do
  initMI
  API.resetHalt  
  setDefaultClocks
  SPI.deInit SPI2
  peripheralClockOn GPIOB
  peripheralClockOn GPIOC
  peripheralClockOn SPI2
  pinMode led $ GPOutPushPull MHz_2
  pinMode spi_nss  $ GPIO.GPOutPushPull MHz_2
  pinMode spi_sck  $ GPIO.AlternateOutPushPull MHz_2
--  pinMode spi_miso $ GPIO.InputFloating
  pinMode spi_miso $ GPIO.InputPullDown
  pinMode spi_mosi $ GPIO.AlternateOutPushPull MHz_2
  SPI.init SPI2 spiConfig
  SPI.enable SPI2

{-
test :: IO ()
test = runReset $ do
  initIA4420

initIA4420 :: TRX ()
initIA4420 = do
  set FrequencyBand433
  set $ frequencyMHz 433.400
  set $ OutputPower 7
  set EnableTransmisson
-}


initRFM12B :: TRX()
initRFM12B = liftSTL $ do
  mapM_ sendSPI [
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
