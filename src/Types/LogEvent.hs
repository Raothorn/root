module Types.LogEvent (
    LogEvent (..),
    debugEvent,
) where

import Types.Card (Card)
import Types.Clearing (Clearing)
import Types.Faction (Faction)
import Types.Index (Index)
import Types.Phase

data LogEvent
    = -- General events
      CardGained (Index Card) Faction
    | CardCrafted (Index Card) Faction
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
