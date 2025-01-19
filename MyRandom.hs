module MyRandom where

type Rng = Int

rngA :: Int
rngA = 1103515245

rngC :: Int
rngC = 12345

rngM :: Int
rngM = 2^31

randomInt :: Rng -> (Int, Rng)
randomInt g = let x = (rngA * g + rngC) `mod` rngM in
              (x, x)

data Random a = Random (Rng -> (a, Rng))

pickRandom :: [a] -> Random a
pickRandom xs = Random $ \g -> let (a, g') = randomInt g in
                               let a' = a `mod` length xs in
                               (xs !! a', g')
