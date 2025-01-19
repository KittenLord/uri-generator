module MyRandom where

type Rng = Int

rngA :: Int
rngA = 1103515245

rngC :: Int
rngC = 12345

rngM :: Int
rngM = 2^31

data Random a = Random (Rng -> (a, Rng))

random :: Random Int
random = Random $ \g -> let x = (rngA * g + rngC) `mod` rngM in
                        (x, x)

instance Functor Random where
    fmap f (Random t) = Random $ \g -> let (a, g') = t g in (f a, g')

-- NOTE: ideally all of these should be g, not g g' and g'', but for some reason ghosting didn't work as I expected
instance Applicative Random where
    pure a = Random $ \g -> (a, g)
    (Random fc) <*> (Random ac) = Random $
        \g -> let (f, g') = fc g in
              let (a, g'') = ac g' in
              (f a, g'')

instance Monad Random where
    (Random ac) >>= f = Random $ (\(a, g) -> let (Random bc) = f a in bc g) . ac

randomRange :: (Int, Int) -> Random Int
randomRange (lo, hi) = (\r -> (r `mod` (hi - lo) + lo)) <$> random

seed :: Random a -> Rng -> a
seed (Random ac) s = let (a, g) = ac s in a

randomElem :: [a] -> Random a
randomElem xs = do
    a <- random
    let i = a `mod` length xs
    return $ xs !! i

-- TODO: for some reason this doesn't work
randomBound :: (Enum a, Bounded a) => Random a
randomBound = randomElem [ minBound..maxBound ]

randomPercent :: Int -> Random Bool
randomPercent x
    | x >= 100 = pure True
    | x <= 0 = pure False
    | otherwise = (< x) <$> randomRange (0, 100)
