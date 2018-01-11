----------------------------------------------------------------------------
-- |
-- Module      :  Test
-- Copyright   :  (c) Marc Fontaine 2016-2018
-- License     :  BSD3
-- 
-- Maintainer  :  Marc.Fontaine@gmx.de
-- Stability   :  experimental
-- Portability :  GHC-only
--

module Test
where
import TRX
import IA4420

-- | Reset and turn on the transmitter.
test :: IO ()
test = runReset $ do
  initIA4420

initIA4420 :: TRX ()
initIA4420 = do
  set FrequencyBand433
  set $ frequencyMHz 433.400
  set $ OutputPower 7
  set EnableTransmisson


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
