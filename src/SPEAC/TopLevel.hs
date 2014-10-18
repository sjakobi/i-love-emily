module SPEAC.TopLevel where

import Types

runTheProgram = undefined
-- breakIntoPhrases
-- doSpeacOnPhrases
-- getSpeacMiddleGround
-- groupForm
-- getSpeacBackGround

breakIntoPhrases :: Notes -> [Time] -> [Notes]
breakIntoPhrases events []          = [events]
breakIntoPhrases events (t:timings) = firstEvents : laterEvents
  where
    -- TODO: * use partition
    --       * maybe make assumptions about order of events and use
    --         span/break instead
    firstEvents = getEventsTo t events
    laterEvents = breakIntoPhrases (getEventsFrom t events) timings

getEventsTo :: Time -> Notes -> Notes
getEventsTo time = filter (\x -> start x < time)

getEventsFrom :: Time -> Notes -> Notes
getEventsFrom time = filter (\x -> start x >= time)
