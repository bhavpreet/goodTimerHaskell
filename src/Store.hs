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
import Data.List.Split
import Data.Map (empty, lookup)
-- import Data.Vector qualified as V
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
  -- csvData <- BL.readFile "salaries.csv"
  -- read csv file
  csvData <- readFile csvFile
  let csvLines = lines csvData
  -- parse csv file
  -- csvValues are in format: [Age, Name, Country, Bib]
  -- convert to Athelet
  let athletes =
        map
          ( \l ->
              do
                let v = splitOn "," l
                Athlete
                  { athleteAge = read (head v) :: Int,
                    athleteName = v !! 1,
                    nationality = v !! 2,
                    bibNo = read (v !! 3) :: Int,
                    athleteRounds = Data.Map.empty,
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
addAthleteRank bibNo rnd rankStr = do
  let rank' = convertRankStrToInt rankStr
  case rank' of
    Nothing -> return "Invalid rank format"
    Just rank'' -> do
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
                  upsertAtheletToStore $ addRoundToAthlete a $ r {rank = rank'', timeStr = rankStr}
            Just r -> do
              -- update round in atheleteRounds
              let newRound = r {rank = rank'', timeStr = rankStr}
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
  -- putStrLn $ "getAthletesByRound The status code was: " ++ (show $ statusCode $ responseStatus response)
  -- print $ responseBody response
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
  putStrLn $ "getAtheletesByCategory The status code was: " ++ (show $ statusCode $ responseStatus response)
  -- print $ responseBody response

  let res = (decode $ responseBody response)
  case res of
    Nothing -> return []
    Just res' -> return res'

joinAthletes :: [Athlete] -> [[Athlete]] -> [Athlete]
joinAthletes a [] = a
joinAthletes a (rs : rss) = do
  joinAthletes (a ++ rs) rss

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
          aths <- mapM (applySelectionAndSortCriteriaOnPreviousRound category nxtRnd') rs'
          putStrLn $ "length of aths = " ++ (show $ length aths)
          putStrLn $ show aths
          return $ joinAthletes [] aths

applySelectionAndSortCriteriaOnPreviousRound :: String -> Round -> String -> IO [Athlete]
applySelectionAndSortCriteriaOnPreviousRound category nxtRnd' previousRoundName = do
  aths <- getAthletesByRound category previousRoundName
  rnd <-
    getRoundFromStore
      previousRoundName
  case rnd of
    Nothing -> return []
    Just rnd' -> do
      print $ show (roundSelectionCriteria nxtRnd') ++ " " ++ show (sortCriteria rnd') ++ " " ++ previousRoundName ++ " " ++ (show $ length aths)
      return $
        applyRoundSelectionCriteria
          (roundSelectionCriteria nxtRnd')
          (sortCriteria rnd')
          previousRoundName
          aths

athleteToCSV :: Athlete -> String -> IO ()
athleteToCSV a rndName = do
  let round = Data.Map.lookup rndName (athleteRounds a)
  case round of
    Nothing -> do
      return ()
    Just r -> do
      putStrLn $
        (athleteName a)
          ++ ","
          ++ (show $ bibNo a)
          ++ ","
          ++ (show $ athleteCategory a)
          ++ ","
          ++ show (timeStr r)
          ++ ","
          ++ show (roundDuration r)
          ++ ","
          ++ show (penalty r)

      return ()

athletesToCSV :: [Athlete] -> String -> IO ()
athletesToCSV [] _ = return ()
athletesToCSV (a : as) roundName = do
  athleteToCSV a roundName
  athletesToCSV as roundName

-- Takes RoundName, Category
listAthletesForRound :: String -> String -> IO ()
listAthletesForRound rndName cat = do
  aths <- getAthletesForNextRound rndName cat
  round <- getRoundFromStore rndName
  case round of
    Nothing -> return ()
    Just rnd -> do
      mapM_ (athletesToCSV aths) (selectionRounds rnd)
