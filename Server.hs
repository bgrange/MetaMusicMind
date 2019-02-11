{-# LANGUAGE GADTs, StandaloneDeriving, Rank2Types #-}

module Server

where

import Network
import Data.Monoid
import Control.Monad
import System.IO
import Data.CircularList

pORT_NUMBER=1025

data Player = Player { host :: HostName,
                       port :: PortNumber,
                       handle :: Handle
                    }
		    deriving Show
              
data Game a = Game { players :: CList Player,
                     composition :: a }
			deriving Show

emptyGame :: Monoid a => Game a
emptyGame = Game empty mempty

addPlayer :: Game a -> Player -> Game a
addPlayer g p = Game (p `insertL` (players g)) (composition g)

oneRound :: (Monoid a,Read a,Show a) => Game a -> IO (Game a)
oneRound g = let peeps = players g in
  case focus $ peeps of
   Nothing -> undefined
   Just peep -> let h = handle peep in
      do hPutStrLn h "it's your turn"
         newMaterial <- hGetLine h
         return $ Game (rotR $ peeps) ((composition g) <> ((read newMaterial)::Read a => a))

allRounds :: (Monoid a,Read a,Show a) => Game a -> IO (Game a)
allRounds g = do print $ composition g
		 g' <- oneRound g
                 allRounds g'

main = do x <- listenOn (PortNumber pORT_NUMBER)
	  (handle,host,port) <- accept x
	  putStrLn $ "connected to: " ++ host ++ ":" ++ (show port)
          (allRounds::Game String -> IO (Game String)) $ addPlayer emptyGame (Player host port handle)
            

