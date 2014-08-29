import Criterion.Main

import SPEAC.Tensions

main = defaultMain [
    bench "removeOctaves" $ nf removeOctaves [60, 67, 64, 72]
  , bench "runSPEAC" $ nf (runTheSPEACWeightings bookExample 4 8) 4
  ]
