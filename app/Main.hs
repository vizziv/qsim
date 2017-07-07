{-# LANGUAGE
    DefaultSignatures
  , FlexibleInstances
  , RankNTypes
  , TypeFamilies
#-}

module Main where

import Control.Lens hiding ( argument )
import Data.List ( groupBy, sortBy )
import Data.Monoid ( (<>), Product(..) )
import Data.Foldable ( for_ )
import Data.Function ( on )
import Options.Applicative
import System.IO

import Dmrl ( JobDmrl )
import Arrival
import Job hiding ( ageStart )
import qualified Job ( ageStart )
import Simulate
import Stream ( Stream )
import qualified Stream

main :: IO ()
main = do
  opts <- execParser (info parserAc mempty)
  let configs = expandAc opts
      n = length configs
  for_ (zip [0..] configs) $ \(i, config) -> do
    hPutStrLn stderr $ "Progress: " ++ show i ++ " / " ++ show n
    run config
  hPutStrLn stderr $ "Progress: " ++ show n ++ " / " ++ show n
  hPutStrLn stderr $ "Done!"
  where
    expandAc =
      expand & mapped . mapped . _2 %~ \(seed, num, size, factor) ->
        Ac seed num size (fromIntegral num * size * factor)

parserAc =
  (,,)
  <$> ((:[]) <$> option auto (short 'n'))
  <*> ( (,,,)
        <$> option auto (short 's')
        <*> argument auto (metavar "MULTI-NUM-TASKS")
        <*> (map Time <$> argument auto (metavar "MULTI-TASK-SIZE"))
        <*> (map Time <$> argument auto (metavar "DMRL-SIZE-FACTOR"))
      )
  <*> ( (,)
        <$> argument auto (metavar "LOAD")
        <*> argument auto (metavar "DMRL-FRACTION")
      )

run :: (Int, ArrivalConfig, (Double, Double)) -> IO ()
run (numEvents, ac, (load, fracDmrl)) =
  print ((numEvents, loads, ac), (statsJo, statsJsf, statsJsp))
  where
    loads = (load * (1 - fracDmrl), load * fracDmrl)
    (jos, jsfs, jsps) = streams ac loads
    statsJo = stats numEvents $ simulate jos
    statsJsf = stats numEvents $ simulate jsfs
    statsJsp = stats numEvents $ simulate jsps
    zipDivBy (a1, a2, a3) (b1, b2, b3) = (b1/a1, b2/a2, b3/a3)

streams ::
  ArrivalConfig ->
  (Double, Double) ->
  ( Stream (Delayed (Either JobDmrl JobOptimal))
  , Stream (Delayed (Either JobDmrl JobSerptFirst))
  , Stream (Delayed (Either JobDmrl JobSerptParallel))
  )
streams ac = \loads ->
  ( jbs `withLoads` loads
  , jbs `withLoads` loads
  , jbs `withLoads` loads
  )
  where
    jbs = poisson ac

statsWithErrs ::
  Int ->
  Stream (Either t (Delayed Event)) ->
  -- Mean number in system, discrepancies for number and work in system.
  (Double, [(Int, Time)])
statsWithErrs numSamples xs = (f/d, errs)
  where
    (Time f, Time d, _, _, errs) =
      foldl go (0, 0, 0, 0, []) .
      Stream.take numSamples .
      Stream.mapMaybe (preview _Right) $
      xs
    go (f, d, n, w, errs) (Delayed t ev) =
      ( f + (fromIntegral n * t)
      , d + t
      , nNew
      , wNew
      , errsOf ev ++ errs
      )
      where
        nNew = n + dnumOf ev
        wNew = (if n > 0 then w - t else w) + dworkOf ev
        dnumOf (EvEnter _ _) = 1
        dnumOf (EvExit _ _ _) = (-1)
        dworkOf (EvEnter _ w) = w
        dworkOf (EvExit _ _ _) = 0
        errsOf (EvEnter _ _) = []
        errsOf (EvExit _ nEv wEv) = [(nNew - nEv, wNew - wEv)]

-- Mean number in system.
-- A bit faster than `fst . statsWithErrs`.
stats :: Int -> Stream (Either t (Delayed Event)) -> (Double, Double, Double)
stats numSamples xs = fs & each %~ fromTime . (/d)
  where
    (fs, d, _) =
      foldl go ((0, 0, 0), 0, (0, 0, 0)) .
      Stream.take numSamples .
      Stream.mapMaybe (preview _Right) $
      xs
    go (fs, d, ns) (Delayed t ev) =
      ( fs `zipPlus` (ns & each %~ (*t) . fromIntegral)
      , d + t
      , ns `zipPlus` dnumsOf ev :: (Int, Int, Int)
      )
    dnumsOf (EvEnter jt _) = jtVec jt
    dnumsOf (EvExit jt _ _) = jtVec jt & each %~ negate
    zipPlus (a1, a2, a3) (b1, b2, b3) = (a1+b1, a2+b2, a3+b3)
    jtVec JtMulti = (1, 1, 0)
    jtVec JtDmrl = (1, 0, 1)
    fromTime (Time t) = t

sizeJb :: JobBase -> Time
sizeJb =
  sumOf $
  to agesDone . each <>
  to ((*) <$> negate . fromIntegral . length . agesDone <*> Job.ageStart)

-- Taking statistics on runs.

type Params = (Int, (Double, Double), ArrivalConfig)

type Data =
  ( (Double, Double, Double)
  , (Double, Double, Double)
  , (Double, Double, Double)
  )

analyze :: IO ()
analyze = do
  runs <- map read . lines <$> readFile "out.txt" :: IO [(Params, Data)]
  let clumps = clumpOn (view params) runs & each . _2 . each %~ view _2
      errsOf ::
        Getter Data Double ->
        [(Params, (Double, Double, Double))]
      errsOf f = clumps & each . _2 %~ confidenceOf (each . f)
  print $ last (errsOf (_1 . _1))
  print $ last (errsOf (_2 . _1))
  print $ last (errsOf (to ratios))
  where
    params = _1 . to ((_3 . seed) .~ 0)
    ratios d = d ^. _2 . _1 / d ^. _1 . _1

meanOf :: Fractional a => Fold s a -> s -> a
meanOf f xs = sumOf f xs / fromIntegral (lengthOf f xs)

stddevOf :: Floating a => Fold s a -> s -> a
stddevOf f xs =
  sqrt ( sumOf (f . to (subtract m) . to (^2)) xs /
         fromIntegral (lengthOf f xs - 1)
       )
  where
    m = meanOf f xs

confidenceOf :: Floating a => Fold s a -> s -> (a, a, a)
confidenceOf f xs = (m - 2*err, m, m + 2*err)
  where
    m = meanOf f xs
    err = stddevOf f xs / sqrt (fromIntegral (lengthOf f xs))

clumpOn :: Ord b => (a -> b) -> [a] -> [(b, [a])]
clumpOn f = map (\xs -> (f (head xs), xs)) . clumpBy (compare `on` f)

clumpBy :: (a -> a -> Ordering) -> [a] -> [[a]]
clumpBy cmp = groupBy eq . sortBy cmp
  where
    eq x y
      | cmp x y == EQ = True
      | otherwise = False

{-
  Check that load is really 0.5:

  > Stream.take 50000 jsfs &
  > (/) <$>
  > sumOf (each . object . to (sizeJb . jbJo . joJsf)) <*>
  > sumOf (each . delay)
  Time 0.5004608201008964

  For debugging.
  > traverse_ (putStrLn . (++"\n") . show) . zip [0..] . Stream.take 100 $ simulate jsfs

  This used to have a bug, but it works now.
  > last . Stream.take 6757 $ simulate jos
 -}

{-
  Past bug: used to hang on following runs (first one is exactly at bad point):
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9003 0.8 11 11 1.0 3.0 94075
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9002 0.9 1 8 1.0 20.0 100000
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9001 0.9 1 3 1.0 100.0 10000
  ./.stack-work/install/x86_64-osx/lts-8.18/8.0.2/bin/qsim-exe 9001 0.8 1 1 1.0 1000.0 20
  Seems to be caused by large variability in starting age.
  Solution: increase acceptable bisection error.
 -}

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

class Expand t where
  type Compact t
  type Compact t = [t]
  expand :: Compact t -> [t]
  default expand :: Compact t ~ [t] => Compact t -> [t]
  expand = id

instance Expand Int
instance Expand Double
instance Expand Time

instance (Expand a, Expand b) => Expand (a, b) where
  type Compact (a, b) =
    ( Compact a
    , Compact b)
  expand (as, bs) =
    [ (a, b)
    | a <- expand as, b <- expand bs
    ]

instance (Expand a, Expand b, Expand c) => Expand (a, b, c) where
  type Compact (a, b, c) =
    ( Compact a
    , Compact b
    , Compact c
    )
  expand (as, bs, cs) =
    [ (a, b, c)
    | a <- expand as, b <- expand bs, c <- expand cs
    ]

instance (Expand a, Expand b, Expand c, Expand d) =>
  Expand (a, b, c, d) where
  type Compact (a, b, c, d) =
    ( Compact a
    , Compact b
    , Compact c
    , Compact d
    )
  expand (as, bs, cs, ds) =
    [ (a, b, c, d)
    | a <- expand as, b <- expand bs, c <- expand cs, d <- expand ds
    ]
