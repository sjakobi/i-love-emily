{-----------------------------------------------------------------------------
    SARA
------------------------------------------------------------------------------}
module SARA.Compose



-- | Randomly choose a pickup.
--
-- This function seems to be defunct.
-- None of the pieces in the database have an "incipient gesture".
chooseIncipientGesture :: Name -> [([String], [AnalysisLabel])]
chooseIncipientGesture _ = [(["incipience"], [])]