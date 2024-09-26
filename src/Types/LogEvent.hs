module Types.LogEvent (
    LogEvent (..),
    debugEvent,
) where

import Lens.Micro

import Types.Card (Card)
import Types.Clearing (Clearing)
import Types.Faction (Faction)
import Types.Game (Game)
import qualified Types.Game as Game
import Types.IxTable (Index)
import Types.Phase

data LogEvent
    = -- General events
      CardCrafted (Index Card) Faction
    | GainedVictoryPoints Int
    | -- Cat events
      WoodPlaced (Index Clearing)
    | -- Debug events
      PhasePushed Phase
    | PhasePopped Phase

----------------------------------
-- Debugging
----------------------------------

debugEvent :: LogEvent -> IO ()
debugEvent (PhasePushed phase) = do
    putStrLn $ "The phase " <> show phase <> " was pushed on the stack."
debugEvent (PhasePopped phase) = do
    putStrLn $ "The phase " <> show phase <> " was popped from the stack."
debugEvent _ = return ()

debugShowPhaseStack :: Game -> IO ()
debugShowPhaseStack game = putStrLn $ "Updated phase stack: " <> show (game ^. Game.phaseStack)
    
