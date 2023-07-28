{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    decode,
    defaultOptions,
    encode,
    genericToEncoding,
  )
import Data.List (sortBy)
import Data.List.Split
import Data.Map (Map, insert, lookup)
import GHC.Generics (Generic)
import Gogol.FireStore as FS ()
import Network.HTTP.Client
import Network.HTTP.Types

someFunc :: IO ()
someFunc = putStrLn "someFunc"

type Duration = Double

data DurationSelectionCriteria
  = BestDuration
  | WorstDuration
  | AverageDuration
  deriving (Show, Generic)

instance ToJSON DurationSelectionCriteria where
  -- No need to provide a toJSON implementation.
  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance FromJSON DurationSelectionCriteria

data RoundSelectionCriteria
  = Top8
  | FirstAndLastGroupBy4
  | H1Take1_16_17_32
  | H2Take8_9_24_25
  | H3Take5_12_21_28
  | H4Take4_13_20_29
  | H5Take3_14_19_30
  | H6Take6_11_22_27
  | H7Take7_10_23_26
  | H8Take2_15_18_31
  | TakeTop2
  | TakeTop32
  | TakeTop16
  deriving (Show, Generic)

instance ToJSON RoundSelectionCriteria where
  -- No need to provide a toJSON implementation.
  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance FromJSON RoundSelectionCriteria

type Category = String

data Round = Round
  { roundName :: String,
    roundDuration :: Duration,
    panelty :: [Penalty],
    timeStr :: String,
    roundCategory :: Category,
    isCurrentRound :: Bool,
    subRounds :: [Round],
    currentSubRoundName :: String,
    timeSelectionCriteria :: DurationSelectionCriteria,
    selectionRounds :: [String],
    roundSelectionCriteria :: RoundSelectionCriteria
  }
  deriving (Show, Generic)

instance ToJSON Round where
  -- No need to provide a toJSON implementation.
  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Round

data Penalty = Penalty
  { paneltyDuration :: Duration,
    note :: String
  }
  deriving (Show, Generic)

instance ToJSON Penalty where
  -- No need to provide a toJSON implementation.
  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Penalty

-- Athlete data type for storing athlete information and round chanin
data Athlete = Athlete
  { athleteName :: String,
    athleteAge :: Int,
    nationality :: String,
    bibNo :: Int,
    athleteRounds :: Map String Round,
    athleteCategory :: Category
  }
  deriving (Show, Generic)

instance ToJSON Athlete where
  -- No need to provide a toJSON implementation.
  -- For efficiency, we write a simple toEncoding implementation, as
  -- the default version uses toJSON.
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Athlete

-- roundDurationWithPenalty adds roundtime with paneltytime and returns the in Duration
roundDurationWithPenalty :: Maybe Round -> Duration
roundDurationWithPenalty Nothing = 0.0
roundDurationWithPenalty
  ( Just
      Round
        { roundDuration = rt,
          panelty = p
        }
    ) =
    rt + sum (map paneltyDuration p)

newtype Athletes = Athletes [Athlete] deriving (Show)

-- Add round to athlete
addRoundToAthlete :: Athlete -> Round -> Athlete
addRoundToAthlete a r =
  a
    { athleteRounds =
        -- Find round with same name and replace it
        insert (roundName r) r (athleteRounds a)
    }

-- addRoundsToAthlete adds a list of rounds to each athlete in Athletes
addRoundsToAthlete :: [Round] -> Athletes -> Athletes
addRoundsToAthlete rs (Athletes as) =
  Athletes
    ( map
        ( \a ->
            foldr
              ( \r a' ->
                  addRoundToAthlete a' r
              )
              a
              rs
        )
        as
    )

-- Get athlete from list
getAthlete :: Int -> Athletes -> Maybe Athlete
getAthlete _ (Athletes []) = Nothing
getAthlete n (Athletes (a : as))
  | n == bibNo a = Just a
  | otherwise = getAthlete n (Athletes as)

-- make roun chain
makeRoundChain :: Round -> [Round] -> RoundChain
makeRoundChain r [] = [r]
makeRoundChain r (x : xs) = r : x : xs

type RoundChain = [Round]

-- matchCurrentRound takes Round, dotted notation of name round.subround as string and returns true if the round is current
matchCurrentRound :: Round -> String -> Bool
matchCurrentRound r currentRnd =
  if roundName r == currentRnd
    then True
    else case subRounds r of
      [] -> False
      (x : xs) ->
        if roundName x == currentRnd
          then True
          else matchCurrentRound x currentRnd

convertTimeStrToDuration :: String -> Maybe Duration
convertTimeStrToDuration "DNF" = Nothing
convertTimeStrToDuration "DNS" = Nothing
convertTimeStrToDuration "DSQ" = Nothing
--  time String format = "00:00:00.00"
convertTimeStrToDuration time =
  case splitOn ":" time of
    [h, m, s, ms] ->
      Just $
        (read h :: Double) * 3600
          + (read m :: Double) * 60
          + (read s :: Double)
          + ((read ms :: Double) / 1000)
    [h, m, sms] ->
      case splitOn "." sms of
        [s, ms] ->
          Just $
            (read h :: Double) * 3600
              + (read m :: Double) * 60
              + (read s :: Double)
              + ((read ms :: Double) / 1000)
        _ -> Nothing
    _ -> Nothing

-- setCurrentRound
-- Takes Round and dotted notation of name round.subround as string and marks the round and sub round as current
-- If the round has subround dotted notation is mandatory
setCurrent :: Round -> String -> String -> Round
setCurrent r rnd subRnd =
  case subRounds r of
    [] ->
      if roundName r == rnd
        then (r {isCurrentRound = True})
        else r
    (x : xs) ->
      if roundName x == rnd
        then
          ( r
              { isCurrentRound = True,
                subRounds = setCurrentRoundInChain (x : xs) subRnd
              }
          )
        else r

-- setCurrentRoundInChain
setCurrentRoundInChain :: RoundChain -> String -> RoundChain
setCurrentRoundInChain roundChain rndSubrnd =
  map
    ( \r ->
        case parseRoundSubRoundString rndSubrnd of
          Nothing -> r
          Just (rnd, subRnd) ->
            if roundName r == rnd
              then
                ( r
                    { isCurrentRound = True,
                      subRounds =
                        case subRnd of
                          Nothing -> subRounds r
                          Just s -> setCurrentRoundInChain (subRounds r) s
                    }
                )
              else r
    )
    roundChain

-- getCurrentRound returns the current round in the round chain in dotted notation
getCurrentRoundName :: RoundChain -> Maybe String
getCurrentRoundName (r : rs) =
  if isCurrentRound r
    then Just (roundName r)
    else case subRounds r of
      [] -> Nothing
      (x : xs) ->
        if isCurrentRound x
          then Just (roundName r ++ "." ++ roundName x)
          else getCurrentRoundName xs

getCurrentRound :: RoundChain -> Round
getCurrentRound [] = undefined
getCurrentRound (r : rs) =
  if isCurrentRound r
    then r
    else case subRounds r of
      [] -> r
      (x : xs) ->
        if isCurrentRound x
          then x
          else getCurrentRound xs

nextRound :: RoundChain -> Round
nextRound [] = undefined
nextRound (r : rs) = head rs

advanceRound :: RoundChain -> RoundChain
advanceRound [] = undefined
advanceRound (_ : rs) = rs

sortAthletesBasedOnDuration :: Round -> [Athlete] -> [Athlete]
sortAthletesBasedOnDuration r as =
  sortBy
    ( \a1 a2 ->
        compare
          (roundDurationWithPenalty (Data.Map.lookup (roundName r) (athleteRounds a1)))
          (roundDurationWithPenalty (Data.Map.lookup (roundName r) (athleteRounds a2)))
    )
    as

-- Based on the Round selection criteria, for a given list of Athlete, do the selection
-- and return the list of athletes for the next round
applyRoundSelectionCriteria :: Round -> [Athlete] -> [Athlete]
applyRoundSelectionCriteria r as =
  case roundSelectionCriteria r of
    Top8 ->
      -- Get round from Athlete find round duration with panelty and sort the list, take top 8
      ( take
          8
          ( sortBy
              ( \a1 a2 ->
                  compare
                    (roundDurationWithPenalty (Data.Map.lookup (roundName r) (athleteRounds a1)))
                    (roundDurationWithPenalty (Data.Map.lookup (roundName r) (athleteRounds a2)))
              )
              as
          )
      )
    H1Take1_16_17_32 ->
      -- Take 0th, 15th 17th and 32nd element from sored list of athletes, sorted on round duration
      do
        let sortedAthletes = sortAthletesBasedOnDuration r as
        [sortedAthletes !! 0, sortedAthletes !! 15, sortedAthletes !! 16, sortedAthletes !! 31]
    H2Take8_9_24_25 ->
      -- Take 8th, 9th 24th and 25th element from sored list of athletes, sorted on round duration
      do
        let sortedAthletes = sortAthletesBasedOnDuration r as
        [sortedAthletes !! 7, sortedAthletes !! 8, sortedAthletes !! 23, sortedAthletes !! 24]
    H3Take5_12_21_28 ->
      -- Take 5th, 12th 21st and 28th element from sored list of athletes, sorted on round duration
      do
        let sortedAthletes = sortAthletesBasedOnDuration r as
        [sortedAthletes !! 4, sortedAthletes !! 11, sortedAthletes !! 20, sortedAthletes !! 27]
    H4Take4_13_20_29 ->
      -- Take 4th, 13th 20th and 29th element from sored list of athletes, sorted on round duration
      do
        let sortedAthletes = sortAthletesBasedOnDuration r as
        [sortedAthletes !! 3, sortedAthletes !! 12, sortedAthletes !! 19, sortedAthletes !! 28]
    H5Take3_14_19_30 ->
      -- Take 3rd, 14th 19th and 30th element from sored list of athletes, sorted on round duration
      do
        let sortedAthletes = sortAthletesBasedOnDuration r as
        [sortedAthletes !! 2, sortedAthletes !! 13, sortedAthletes !! 18, sortedAthletes !! 29]
    H6Take6_11_22_27 ->
      -- Take 6th, 11th 22nd and 27th element from sored list of athletes, sorted on round duration
      do
        let sortedAthletes = sortAthletesBasedOnDuration r as
        [sortedAthletes !! 5, sortedAthletes !! 10, sortedAthletes !! 21, sortedAthletes !! 26]
    H7Take7_10_23_26 ->
      -- Take 7th, 10th 23rd and 26th element from sored list of athletes, sorted on round duration
      do
        let sortedAthletes = sortAthletesBasedOnDuration r as
        [sortedAthletes !! 6, sortedAthletes !! 9, sortedAthletes !! 22, sortedAthletes !! 25]
    H8Take2_15_18_31 ->
      -- Take 2nd, 15th 18th and 31st element from sored list of athletes, sorted on round duration
      do
        let sortedAthletes = sortAthletesBasedOnDuration r as
        [sortedAthletes !! 1, sortedAthletes !! 14, sortedAthletes !! 17, sortedAthletes !! 30]
    TakeTop2 ->
      take 2 $ sortAthletesBasedOnDuration r as
    TakeTop32 ->
      take 32 $ sortAthletesBasedOnDuration r as
    TakeTop16 ->
      take 16 $ sortAthletesBasedOnDuration r as

-- Getlist of atheletes for the nextRound
-- Based on the next round selection criteria, use current round for atheletes to make a sorted list
-- getAthletesForNextRound :: [Athlete] -> String -> Maybe Athletes
-- getAthletesForNextRound as nextRoundName = do
--   -- For each athlete for a given nextRoundName compute the roundChainDurationWithPenalty for previous rounds
--   -- and sort the athletes based on the round selection criteria
--   map
--     ( \a ->
--         case lookup nextRoundName (athleteRounds a) of
--           Nothing -> Nothing
--           Just r ->
--             Just
--               ( a
--                   { athleteRounds =
--                       insert
--                         nextRoundName
--                         (r {roundChainDurationWithPenalty = roundChainDurationWithPenalty (makeRoundChain r (athleteRounds a))})
--                         (athleteRounds a)
--                   }
--               )
--               -- case roundSelectionCriteria (lookup nextRoundName (athleteRounds $ head a)) of
--               --   AscendingDuration ->
--               --     Nothing
--               --   Top8 ->
--               --     Nothing
--     )
--     as

-- sort athletes based on the round selection criteria
-- sortAthletes :: Athletes -> String -> Athletes
-- sortAthletes (Athletes as) roundName =
--   case roundSelectionCriteria (lookup roundName (athleteRounds $ head as)) of
--     AscendingDuration ->
--       Athletes
--         ( sortBy
--             ( \a1 a2 ->
--                 compare
--                   ( roundChainDurationWithPenalty
--                       (makeRoundChain (getCurrentRound (athleteRounds a1)) (athleteRounds a1))
--                   )
--                   ( roundChainDurationWithPenalty
--                       (makeRoundChain (getCurrentRound (athleteRounds a2)) (athleteRounds a2))
--                   )
--             )
--             as
--         )
--     Top8 ->
--       Athletes
--         ( take
--             8
--             ( sortBy
--                 ( \a1 a2 ->
--                     compare
--                       (roundChainDurationWithPenalty (makeRoundChain (getCurrentRound (athleteRounds a1)) (athleteRounds a1)))
--                       (roundChainDurationWithPenalty (makeRoundChain (getCurrentRound (athleteRounds a2)) (athleteRounds a2)))
--                 )
--                 as
--             )
--         )
--     FirstAndLastGroupBy4 ->
--       Athletes
--         ( take
--             4
--             ( sortBy
--                 ( \a1 a2 ->
--                     compare
--                       (roundChainDurationWithPenalty (makeRoundChain (getCurrentRound (athleteRounds a1)) (athleteRounds a1)))
--                       (roundChainDurationWithPenalty (makeRoundChain (getCurrentRound (athleteRounds a2)) (athleteRounds a2)))
--                 )
--                 as
--             )
--             ++ take
--               4
--               ( sortBy
--                   ( \a1 a2 ->
--                       compare
--                         (roundChainDurationWithPenalty (makeRoundChain (getCurrentRound (athleteRounds a1)) (athleteRounds a1)))
--                         (roundChainDurationWithPenalty (makeRoundChain (getCurrentRound (athleteRounds a2)) (athleteRounds a2)))
--                   )
--                   (reverse as)
--               )
--         )

-- Advance round for selected athletes by removing current round
-- advanceAtheleteRound :: Athletes -> Athletes
-- advanceAtheleteRound (Athletes as) =
--   Athletes
--     ( map
--         ( \a ->
--             Athlete (athleteName a) (athleteAge a) (nationality a) (bibNo a) (advanceRound (athleteRounds a))
--         )
--         as
--     )

-- updateDurationForAthlete :: Athlete -> String -> Duration -> Maybe Athlete
-- updateDurationForAthlete a rnd dur =
--   a
--     { athleteRounds = do
--         case lookup rnd (athleteRounds a) of
--           Nothing -> Nothing
--           Just r -> do
--             insert rnd (updateDurationForRound r dur) (athleteRounds a)

--         map
--           ( \r ->
--               if roundName r == rnd
--                 then r {roundDuration = dur}
--                 else r
--           )
--           (athleteRounds a)
--     }

-- update the current round with duration for the athelete
-- updateCurrentAthleteRound :: Athlete -> Duration -> Athlete
-- updateCurrentAthleteRound a d =
--   Athlete
--     (athleteName a)
--     (athleteAge a)
--     (nationality a)
--     (bibNo a)
--     ( map
--         ( \r ->
--             if roundName r == roundName (getCurrentRound (athleteRounds a))
--               then
--                 Round
--                   { roundName = roundName r,
--                     roundDuration = d,
--                     panelty = panelty r,
--                     timeStr = timeStr r,
--                     subRounds = subRounds r,
--                     timeSelectionCriteria = timeSelectionCriteria r,
--                     roundSelectionCriteria = roundSelectionCriteria r
--                   }
--               else r
--         )
--         (athleteRounds a)
--     )

-- Add time to athleteRounds
-- Take list of atheletes, take bib number, take time, add time for current round and update the round for the athlete
-- addTimeToAthleteInAtheletes :: Athletes -> Int -> Duration -> Athletes
-- addTimeToAthleteInAtheletes (Athletes as) n t =
--   Athletes
--     ( map
--         ( \a ->
--             if bibNo a == n
--               then updateCurrentAthleteRound a t
--               else a
--         )
--         as
--     )

parseRoundSubRoundString :: String -> Maybe (String, Maybe String)
parseRoundSubRoundString rnd =
  let (r, s) = break (== '.') rnd
   in if s /= ""
        then Just (r, Just (tail s))
        else Just (r, Nothing)

-- dropStore
dropStore :: IO String
dropStore = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:5001/goodtimer-9db1b/us-central1/dropAllCollections"
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = methodPost,
            requestHeaders = [(hContentType, "application/json")]
          }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  return (show $ statusCode $ responseStatus response)
