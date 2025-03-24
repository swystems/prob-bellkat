{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}

import BellKAT.Prelude
import BellKAT.ProbabilisticPrelude

import qualified Data.ByteString.Lazy as BS
import qualified Data.Aeson as A

e :: BellKATPolicy
e = create "C" <> trans "C" ("A", "C")

f :: BellKATPolicy
f = create "C" <> trans "C" ("B", "C")

p :: BellKATPolicy
p = (e <.> f) <> (e <.> f)

-- networkCapacity :: NetworkCapacity BellKATTag
-- networkCapacity = ["C" ~ "C", "C" ~ "C", "A" ~ "C", "B" ~ "C"]

actionConfig :: ProbabilisticActionConfiguration
actionConfig = PAC 
    { pacTransmitProbability = [(("C", "B"), 1 / 2),(("C", "A"), 4 / 5)]
    , pacCreateProbability = [("C", 9/10)]
    , pacUCreateProbability = []
    , pacSwapProbability = []
    }

main :: IO ()
main =
    let cdbps = applyStarPolicy' actionConfig p []
     in BS.putStr $ A.encode cdbps
