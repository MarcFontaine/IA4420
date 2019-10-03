module UDPSink
where
import Network.Socket
import Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Builder as DBB
import Control.Monad
import Control.Concurrent(threadDelay)

main :: IO ()
main = withSocketsDo $ do
  s <- socket AF_INET Datagram defaultProtocol
  connect s $ SockAddrInet 20000 0X0100007f
  forM_ (chunks 1000 [0,0.01..]) $ \l ->do
    let sig = map (\x -> (11*sin (x*7) + 13* sin (x*9))) l
    void $ NSB.send s $ encodeList sig
    threadDelay 100000

encodeList :: [Float] -> BS.ByteString
encodeList = BSL.toStrict . DBB.toLazyByteString . mconcat . map DBB.floatLE

chunks :: Int -> [a] -> [[a]]
chunks size list = go list
  where
    go :: [a] -> [[a]]
    go [] = []
    go l = let (h,r) = splitAt size l in h : go r
    
