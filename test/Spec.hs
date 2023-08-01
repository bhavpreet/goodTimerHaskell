{-# LANGUAGE BlockArguments #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Lib
import Network.HTTP.Types (requestHeaderFieldsTooLarge431)
import Store
import Test.Hspec

testAddRounds :: IO ()
testAddRounds = hspec $ do
  describe "addRound" $ do
    dropResp <- runIO dropStore
    it "should be able to drop store" $ do
      dropResp
        `shouldBe` "200"
    addRoundResp <-
      runIO $
        addRoundToStore
          ( Round
              { roundName = "Round2",
                roundDuration = 0.0,
                rank = 9999,
                penalty = [],
                timeStr = "",
                subRounds = [],
                isCurrentRound = False,
                currentSubRoundName = "",
                roundCategory = "MEN_PRO",
                selectionRounds = [],
                sortCriteria = BestDuration,
                roundSelectionCriteria = TakeTop16
              }
          )
    it "adds a round to firebase" $ do
      addRoundResp `shouldBe` "200"

testMRF :: IO ()
testMRF = hspec $ do
  describe "test MRF" $ do
    dropResp <- runIO dropStore
    it "drop store for fresh start" $ do
      dropResp `shouldBe` "200"
    describe "adds a MRF rounds" $ do
      let rounds =
            [ ( Round
                  { roundName = "Qualifying",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    sortCriteria = BestDuration,
                    selectionRounds = [],
                    roundSelectionCriteria = TakeTop32,
                    roundCategory = "MEN_PRO",
                    subRounds =
                      []
                  }
              ),
              ( Round
                  { roundName = "Heat1",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H1Take1_16_17_32
                  }
              ),
              ( Round
                  { roundName = "Heat2",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H2Take8_9_24_25
                  }
              ),
              ( Round
                  { roundName = "Heat3",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H3Take5_12_21_28
                  }
              ),
              ( Round
                  { roundName = "Heat4",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H4Take4_13_20_29
                  }
              ),
              ( Round
                  { roundName = "Heat5",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H5Take3_14_19_30
                  }
              ),
              ( Round
                  { roundName = "Heat6",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H6Take6_11_22_27
                  }
              ),
              ( Round
                  { roundName = "Heat7",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H7Take7_10_23_26
                  }
              ),
              ( Round
                  { roundName = "Heat8",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H8Take2_15_18_31
                  }
              ),
              -- QuarterFinals
              ( Round
                  { roundName = "QF1",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Heat1", "Heat2"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "QF2",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Heat3", "Heat4"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "QF3",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Heat5", "Heat6"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "QF4",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["Heat7", "Heat8"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              -- SemiFinals
              ( Round
                  { roundName = "SF1",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["QF1", "QF2"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "SF2",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["QF3", "QF4"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              -- Finals
              ( Round
                  { roundName = "Final",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_PRO",
                    sortCriteria = TopRanking,
                    selectionRounds = ["SF1", "SF2"],
                    roundSelectionCriteria = TakeTop2
                  }
              )
            ]

      addQualifyingResp <- runIO $ addRoundsToStore rounds
      it "add Qualifying Round " $ do
        addQualifyingResp `shouldBe` "200"

-- Add athletes from CSV file
testAddAthletesToStoreFromCSV :: IO ()
testAddAthletesToStoreFromCSV = hspec $
  do
    describe "Add Atheletes to Store from CSV" $ do
      let csvFile = "/Users/bhav/Downloads/menPro.csv"
      addAtheletesResp <- runIO $ addAthletsToStoreFromCSV csvFile
      it "add Atheletes from CSV file " $ do
        addAtheletesResp `shouldBe` "200"

{-
exec: addAthleteTime bibNo rnd timeStr

BIB Time
-}
updateAthelteTimeForQualifying = do
  addAthleteTime 52 "Qualifying" "1:17.263"
  addAthleteTime 37 "Qualifying" "1:19.888"
  addAthleteTime 24 "Qualifying" "1:23.700"
  addAthleteTime 35 "Qualifying" "1:25.650"
  addAthleteTime 23 "Qualifying" "1:27.465"
  addAthleteTime 27 "Qualifying" "1:27.901"
  addAthleteTime 71 "Qualifying" "1:28.053"
  addAthleteTime 69 "Qualifying" "1:31.269"
  addAthleteTime 46 "Qualifying" "1:31.661"
  addAthleteTime 50 "Qualifying" "1:31.898"
  addAthleteTime 25 "Qualifying" "1:34.626"
  addAthleteTime 28 "Qualifying" "1:37.820"
  addAthleteTime 39 "Qualifying" "1:38.349"
  addAthleteTime 33 "Qualifying" "1:40.608"
  addAthleteTime 32 "Qualifying" "1:40.941"
  addAthleteTime 49 "Qualifying" "1:44.281"
  addAthleteTime 21 "Qualifying" "1:47.970"
  addAthleteTime 45 "Qualifying" "1:48.947"
  addAthleteTime 31 "Qualifying" "1:51.255"
  addAthleteTime 22 "Qualifying" "1:34.171"
  addAthleteTime 43 "Qualifying" "1:46.643"
  addAthleteTime 40 "Qualifying" "1:52.007"
  addAthleteTime 47 "Qualifying" "2:04.282"
  addAthleteTime 53 "Qualifying" "1:32.078"
  addAthleteTime 44 "Qualifying" "1:32.773"
  addAthleteTime 36 "Qualifying" "1:29.296"
  addAthleteTime 51 "Qualifying" "2:04.370"
  addAthleteTime 70 "Qualifying" "1:31.101"
  addAthleteTime 54 "Qualifying" "1:39.112"
  addAthleteTime 48 "Qualifying" "1:41.738"
  addAthleteTime 30 "Qualifying" "1:43.355"
  addAthleteTime 38 "Qualifying" "1:53.780"
  addAthleteTime 41 "Qualifying" "1:26.971"
  addAthleteTime 34 "Qualifying" "1:46.993"
  addAthleteTime 29 "Qualifying" "DNS"
  addAthleteTime 42 "Qualifying" "DNS"
  addAthleteTime 55 "Qualifying" "1:39.700"

-- Add panelty
{-
addAthletePenalty bibNo rnd penalty' penaltyDescription
-}
addPanelty = do
  addAthletePenalty 22 "Qualifying" 50 "Gate 10"
  addAthletePenalty 43 "Qualifying" 100 "Gate 08, 10"
  addAthletePenalty 40 "Qualifying" 100 "Gate 08, 10"
  addAthletePenalty 47 "Qualifying" 100 "Gate 07"
  addAthletePenalty 53 "Qualifying" 50 "Gate 06"
  addAthletePenalty 44 "Qualifying" 50 "Gate 06"
  addAthletePenalty 36 "Qualifying" 50 "Gate 06"
  addAthletePenalty 51 "Qualifying" 100 "Gate 06, 10"
  addAthletePenalty 70 "Qualifying" 100 "Gate 06, 07"
  addAthletePenalty 54 "Qualifying" 100 "Gate 06, 07"
  addAthletePenalty 48 "Qualifying" 100 "Gate 06, 07"
  addAthletePenalty 30 "Qualifying" 150 "Gate 06, 07, 10"
  addAthletePenalty 38 "Qualifying" 150 "Gate 06, 07, 10"
  addAthletePenalty 41 "Qualifying" 250 "Gate 06, 07, 08, 09, 10"
  addAthletePenalty 34 "Qualifying" 100 "Gate 06, 07, 08, 10"

-- Heat1
