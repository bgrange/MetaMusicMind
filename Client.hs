module Client

where

import Network
import System.IO
import Control.Monad

pORT_NUMBER=1025

main = do h <- connectTo "localhost" (PortNumber pORT_NUMBER)
	  putStrLn $ "connected to server"
          forever $ do l <- hGetLine h
	               putStrLn l
                       hPutStrLn h "ABCD"

