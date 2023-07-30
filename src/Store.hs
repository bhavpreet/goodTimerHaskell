{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Store where

import Data.Aeson
  ( FromJSON,
    ToJSON (toEncoding),
    decode,
    defaultOptions,
    encode,
    genericToEncoding,
  )
-- import runIO

import Data.List.Split
import Data.Map (empty, lookup)
import GHC.IO (unsafePerformIO)
-- import GHC.TopHandler (runIO)
import Lib
import Network.HTTP.Client
import Network.HTTP.Types
import Prelude hiding (lookup)

addRoundsToStore :: [Round] -> IO String
addRoundsToStore [] = return "200"
addRoundsToStore (r : rs) =
  addRoundToStore r
    >>= \resp ->
      if resp == "200"
        then addRoundsToStore rs
        else return resp

--  Add round by making POST call to preconfigured URL
addRoundToStore :: Round -> IO String
addRoundToStore r = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:5001/goodtimer-9db1b/us-central1/addRoundH"
  let body = encode r
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = methodPost,
            requestHeaders = [(hContentType, "application/json")],
            requestBody = RequestBodyLBS body
          }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  return (show $ statusCode $ responseStatus response)

getRoundFromStore :: String -> IO (Maybe Round)
getRoundFromStore rnd = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:5001/goodtimer-9db1b/us-central1/getRound?roundName=" ++ rnd
  let body = encode rnd
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = methodPost,
            requestHeaders = [(hContentType, "application/json")],
            requestBody = RequestBodyLBS body
          }
  response <- httpLbs request manager
  putStrLn $ "getRoundFromStore The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  return (decode $ responseBody response)

listRoundsFromStore :: IO (Maybe [Round])
listRoundsFromStore = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:5001/goodtimer-9db1b/us-central1/listRounds"
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = methodGet,
            requestHeaders = [(hContentType, "application/json")]
          }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  return (decode $ responseBody response)

addRoundChainToStore :: Category -> [Round] -> IO String
addRoundChainToStore cat rs = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:5001/goodtimer-9db1b/us-central1/makeRoundChain"
  let body = encode (cat, rs)
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = methodPost,
            requestHeaders = [(hContentType, "application/json")],
            requestBody = RequestBodyLBS body
          }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  return (show $ statusCode $ responseStatus response)

upsertAtheletToStore :: Athlete -> IO String
upsertAtheletToStore a = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:5001/goodtimer-9db1b/us-central1/upsertAthlete"
  let body = encode a
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = methodPost,
            requestHeaders = [(hContentType, "application/json")],
            requestBody = RequestBodyLBS body
          }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  return (show $ statusCode $ responseStatus response)

addAtheletesToStore :: [Athlete] -> IO String
addAtheletesToStore [] = return "200"
addAtheletesToStore (a : as) =
  upsertAtheletToStore a
    >>= \resp ->
      if resp == "200" || resp == "400"
        then addAtheletesToStore as
        else return resp

-- Add athlets from CSV file
addAthletsToStoreFromCSV :: String -> IO String
addAthletsToStoreFromCSV csvFile = do
  -- read csv file
  csvData <- readFile csvFile
  -- parse csv file
  let csvLines = lines csvData
  print csvLines
  -- csvValues are in format: [Age, Name, Country, Bib]
  -- convert to Athelet
  let athletes =
        map
          ( \l ->
              do
                let v = splitOn "," l
                Athlete
                  { athleteName = v !! 1,
                    athleteAge = read (head v) :: Int,
                    nationality = v !! 2,
                    bibNo = read (v !! 3) :: Int,
                    athleteRounds = empty,
                    athleteCategory = takeWhile (/= '\r') $ head $ lines $ v !! 4
                  }
          )
          $ tail csvLines
  -- add athelets to store
  addAtheletesToStore athletes

-- findAtheleteInStore: takes bibNo and returns athelete
getAtheleteInStore :: Int -> IO (Maybe Athlete)
getAtheleteInStore bibNo = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:5001/goodtimer-9db1b/us-central1/getAthlete?bibNo=" ++ (show bibNo)
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = methodGet,
            requestHeaders = [(hContentType, "application/json")]
          }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  return (decode $ responseBody response)

-- addAthleteTime: takes  bibNo, roundName, time and adds time to round
addAthleteTime :: Int -> String -> String -> IO String
addAthleteTime bibNo rnd timeStr = do
  let time = convertTimeStrToDuration timeStr
  case time of
    Nothing -> return ("Invalid time format" ++ timeStr)
    Just t -> do
      athlete <- getAtheleteInStore bibNo
      case athlete of
        Nothing -> return "404"
        Just a -> do
          -- Check if round is already in atheleteRounds
          -- if round is already in atheleteRounds, update that round otherwise add new round to the list
          let roundToUpdate = lookup rnd (athleteRounds a)
          case roundToUpdate of
            Nothing -> do
              -- add new round to atheleteRounds
              round' <- getRoundFromStore rnd
              case round' of
                Nothing -> return "404"
                Just r -> do
                  upsertAtheletToStore $ addRoundToAthlete a $ r {roundDuration = t, timeStr = timeStr}
            Just r -> do
              -- update round in atheleteRounds
              let newRound = r {roundDuration = t, timeStr = timeStr}
              upsertAtheletToStore $ addRoundToAthlete a newRound

addAthletePenalty :: Int -> String -> Duration -> String -> IO String
addAthletePenalty bibNo rnd penalty' penaltyDescription = do
  case penaltyDescription of
    "" -> return "Add description"
    ps -> do
      athlete <- getAtheleteInStore bibNo
      case athlete of
        Nothing -> return "404: Athlete not found"
        Just a -> do
          -- Check if round is already in atheleteRounds
          -- if round is already in atheleteRounds, update that round otherwise add new round to the list
          let roundToUpdate = lookup rnd (athleteRounds a)
          case roundToUpdate of
            Nothing -> do
              -- add new round to atheleteRounds
              round' <- getRoundFromStore rnd
              case round' of
                Nothing -> return "404: Round not found"
                Just r -> do
                  upsertAtheletToStore $
                    addRoundToAthlete a $
                      r
                        { penalty = Penalty {penaltyDuration = penalty', note = ps} : penalty r
                        }
            Just r -> do
              -- update round in atheleteRounds
              let newRound =
                    r
                      { penalty = Penalty {penaltyDuration = penalty', note = ps} : penalty r
                      }
              upsertAtheletToStore $ addRoundToAthlete a newRound

addAthleteRank :: Int -> String -> String -> IO String
addAthleteRank bibNo rnd timeStr = do
  let rank = convertRankStrToInt timeStr
  case rank of
    Nothing -> return "Invalid rank format"
    Just rank' -> do
      athlete <- getAtheleteInStore bibNo
      case athlete of
        Nothing -> return "404"
        Just a -> do
          -- Check if round is already in atheleteRounds
          -- if round is already in atheleteRounds, update that round otherwise add new round to the list
          let roundToUpdate = lookup rnd (athleteRounds a)
          case roundToUpdate of
            Nothing -> do
              -- add new round to atheleteRounds
              round' <- getRoundFromStore rnd
              case round' of
                Nothing -> return "404"
                Just r -> do
                  upsertAtheletToStore $ addRoundToAthlete a $ r {rank = rank', timeStr = timeStr}
            Just r -> do
              -- update round in atheleteRounds
              let newRound = r {rank = rank', timeStr = timeStr}
              upsertAtheletToStore $ addRoundToAthlete a newRound

-- getAthletesByRound: takes roundName and returns list of atheletes
getAthletesByRound :: String -> String -> IO [Athlete]
getAthletesByRound category rnd = do
  putStrLn $ "getAthletesByRound: " ++ rnd
  manager <- newManager defaultManagerSettings
  let url =
        "http://localhost:5001/goodtimer-9db1b/us-central1/listAthletesByRound?roundName="
          ++ rnd
          ++ "&athleteCategory="
          ++ category
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = methodGet,
            requestHeaders = [(hContentType, "application/json")]
          }
  response <- httpLbs request manager
  putStrLn $ "getAthletesByRound The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  let res = (decode $ responseBody response)
  case res of
    Nothing -> return []
    Just res' -> return res'

{-
TODO:
- No need to implement subrounds
- Add Qualifying Duration for each athlete and List Athletes for each round
-}

getAtheletesByCategory :: String -> IO [Athlete]
getAtheletesByCategory category = do
  manager <- newManager defaultManagerSettings
  let url = "http://localhost:5001/goodtimer-9db1b/us-central1/listAthletesByCategory?athleteCategory=" ++ category
  initialRequest <- parseRequest url
  let request =
        initialRequest
          { method = methodGet,
            requestHeaders = [(hContentType, "application/json")]
          }
  response <- httpLbs request manager
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  print $ responseBody response
  let res = (decode $ responseBody response)
  case res of
    Nothing -> return []
    Just res' -> return res'

-- TODO: remove unsafePerformIO
joinAthletes :: [Athlete] -> [IO [Athlete]] -> [Athlete]
joinAthletes [] _ = []
joinAthletes a [] = a
joinAthletes a (rs : rss) = do
  joinAthletes (a ++ unsafePerformIO rs) rss

-- Get List of athletes for next round; round name -> category
getAthletesForNextRound :: String -> String -> IO [Athlete]
getAthletesForNextRound [] _ = return []
getAthletesForNextRound nxtRnd category = do
  -- Get nxtRnd from store
  round' <- getRoundFromStore nxtRnd
  case round' of
    Nothing -> return []
    Just nxtRnd' -> do
      let selectRnds = selectionRounds nxtRnd'
      putStrLn $ "getAthletesForNextRound: selectRnds = " ++ (show selectRnds)
      -- Get athletes for each selection round
      case selectRnds of
        [] -> do
          -- Get All Atheletes
          ath <- getAtheletesByCategory category
          return $
            applyRoundSelectionCriteria
              (roundSelectionCriteria nxtRnd')
              (sortCriteria nxtRnd')
              (roundName nxtRnd')
              ath
        rs' -> do
          -- Get athletes for each selection round
          putStrLn $ "getAthletesForNextRound: rs' : " ++ (show rs')
          return $
            joinAthletes
              []
              ( map
                  ( \r -> do
                      rnd <- getRoundFromStore r
                      case rnd of
                        Nothing -> return []
                        Just rnd' -> do
                          fmap
                            ( applyRoundSelectionCriteria
                                (roundSelectionCriteria nxtRnd')
                                (sortCriteria rnd')
                                r
                            )
                            (getAthletesByRound category r)
                  )
                  rs'
              )

-- Get List of athletes for next round; round name -> category
-- getAthletesForNextRound :: String -> String -> IO [Athlete]
-- getAthletesForNextRound nxtRnd category = do
--   -- Get nxtRnd from store
--   round' <- getRoundFromStore nxtRnd
--   case round' of
--     Nothing -> return []
--     Just nxtRnd' -> do
--       let selectRnds = selectionRounds nxtRnd'
--       -- Get athletes for each selection round
--       case selectRnds of
--         [] -> do
--           -- Get All Atheletes
--           -- TODO: appliy roundSelectionCriteria
--           allAthletes <- getAtheletesByCategory category
--           case allAthletes of
--             Nothing -> return []
--             Just aa' -> return aa'
--         rs ->
--           return
--             map
--             ( \r ->
--                 concat $ getAthletesByRound category r
--             )
--             rs

-- -- map
-- --   (
-- --     \r -> do
-- --       res <- getAthletesByRound category r
-- --       case res of
-- --       _ -> []
-- --     ) rs

-- -- r : rs -> do
-- --   rAthletes <- getAthletesByRound category r
-- --   return $
-- --     Data.List.foldl (++) rAthletes $
-- --       map
-- --         ( \r' ->
-- --             runIO $
-- --               getAtheletesByCategory
-- --                 category
-- --                 r'
-- --         )
-- --         rs

-- -- map
-- --   ( \r' ->
-- --       runIO $ getAthletesByRound category r'
-- --   )
-- --   rs

-- -- ++ map
-- --   ( \r' -> do
-- --       rsAthletes <-
-- --         ( getAthletesByRound
-- --             category
-- --             r'
-- --           )
-- --       case rsAthletes of
-- --         Nothing -> []
-- --         Just rsRounds' -> rsRounds'
-- --   )
-- --   rs
