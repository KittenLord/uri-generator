module MyRandom where

type Rng = Int

rngA :: Int
rngA = 1103515245

rngC :: Int
rngC = 12345

rngM :: Int
rngM = 2^31

data Random a = Random (Rng -> (a, Rng))

randomInt :: Random Int
randomInt = Random $ \g -> let x = (rngA * g + rngC) `mod` rngM in
                           (x, x)

instance Functor Random where
    fmap f (Random t) = Random $ \g -> let (a, g') = t g in (f a, g')



pickRandom :: [a] -> Random a
pickRandom xs = (\a -> xs !! (a `mod` length xs)) <$> randomInt
