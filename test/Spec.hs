{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NondecreasingIndentation #-}
-- {-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- {-# OPTIONS_GHC -F -pgmF hspec-discover #-}

import Lib
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
                subRoundsSelectionCriteria = BestOfAll,
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
                  { roundName = "QualifyingWomen",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    sortCriteria = BestDuration,
                    selectionRounds = [],
                    roundSelectionCriteria = TakeTop16,
                    roundCategory = "WOMEN_INTERMEDIATE",
                    subRounds =
                      [],
                    subRoundsSelectionCriteria = BestOfAll
                  }
              ),
              ( Round
                  { roundName = "WQF1",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "WOMEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["QualifyingWomen"],
                    roundSelectionCriteria = WH1Take1_8_12_16
                  }
              ),
              ( Round
                  { roundName = "WQF2",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "WOMEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["QualifyingWomen"],
                    roundSelectionCriteria = WH2Take4_5_9_13
                  }
              ),
              ( Round
                  { roundName = "WQF3",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "WOMEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["QualifyingWomen"],
                    roundSelectionCriteria = WH3Take3_6_10_14
                  }
              ),
              ( Round
                  { roundName = "WQF4",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "WOMEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["QualifyingWomen"],
                    roundSelectionCriteria = WH4Take2_7_11_15
                  }
              ),
              -- QuarterFinals
              ( Round
                  { roundName = "WSF1",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "WOMEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["WQF1", "WQF2"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "WSF2",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "WOMEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["WQF3", "WQF4"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "WOMEN_INT_FINAL",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "WOMEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["WSF1", "WSF2"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              -- MEN
              ( Round
                  { roundName = "QualifyingMen",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    sortCriteria = BestDuration,
                    selectionRounds = [],
                    roundSelectionCriteria = TakeTop16,
                    roundCategory = "MEN_INTERMEDIATE",
                    subRounds =
                      [],
                    subRoundsSelectionCriteria = BestOfAll
                  }
              ),
              ( Round
                  { roundName = "MQF1",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["QualifyingMen"],
                    roundSelectionCriteria = WH1Take1_8_12_16
                  }
              ),
              ( Round
                  { roundName = "MQF2",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["QualifyingMen"],
                    roundSelectionCriteria = WH2Take4_5_9_13
                  }
              ),
              ( Round
                  { roundName = "MQF3",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["QualifyingMen"],
                    roundSelectionCriteria = WH3Take3_6_10_14
                  }
              ),
              ( Round
                  { roundName = "MQF4",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["QualifyingMen"],
                    roundSelectionCriteria = WH4Take2_7_11_15
                  }
              ),
              ( Round
                  { roundName = "MSF1",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["MQF1", "MQF2"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "MSF2",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["MQF3", "MQF4"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "MEN_INT_FINAL",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "MEN_INTERMEDIATE",
                    sortCriteria = TopRanking,
                    selectionRounds = ["MSF1", "MSF2"],
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
  addAthleteTime 37 "Qualifying" "" "1:19.888"
  addAthleteTime 52 "Qualifying" "" "1:17.263"
  addAthleteTime 54 "Qualifying" "" "1:39.112"
  addAthleteTime 27 "Qualifying" "" "1:27.901"
  addAthleteTime 24 "Qualifying" "" "1:23.700"
  addAthleteTime 23 "Qualifying" "" "1:27.465"
  addAthleteTime 69 "Qualifying" "" "1:31.269"
  addAthleteTime 30 "Qualifying" "" "1:43.355"
  addAthleteTime 35 "Qualifying" "" "1:25.650"
  addAthleteTime 71 "Qualifying" "" "1:28.053"
  addAthleteTime 31 "Qualifying" "" "1:51.255"
  addAthleteTime 40 "Qualifying" "" "1:52.007"
  addAthleteTime 47 "Qualifying" "" "2:04.282"
  addAthleteTime 53 "Qualifying" "" "1:32.078"
  addAthleteTime 70 "Qualifying" "" "1:31.101"
  addAthleteTime 38 "Qualifying" "" "1:53.780"
  addAthleteTime 46 "Qualifying" "" "1:31.661"
  addAthleteTime 50 "Qualifying" "" "1:31.898"
  addAthleteTime 25 "Qualifying" "" "1:34.626"
  addAthleteTime 28 "Qualifying" "" "1:37.820"
  addAthleteTime 39 "Qualifying" "" "1:38.349"
  addAthleteTime 33 "Qualifying" "" "1:40.608"
  addAthleteTime 32 "Qualifying" "" "1:40.941"
  addAthleteTime 49 "Qualifying" "" "1:44.281"
  addAthleteTime 21 "Qualifying" "" "1:47.970"
  addAthleteTime 45 "Qualifying" "" "1:48.947"
  addAthleteTime 22 "Qualifying" "" "1:34.171"
  addAthleteTime 43 "Qualifying" "" "1:46.643"
  addAthleteTime 44 "Qualifying" "" "1:32.773"
  addAthleteTime 36 "Qualifying" "" "1:29.296"
  addAthleteTime 51 "Qualifying" "" "2:04.370"
  addAthleteTime 48 "Qualifying" "" "1:41.738"
  addAthleteTime 41 "Qualifying" "" "1:26.971"
  addAthleteTime 55 "Qualifying" "" "1:39.700"
  addAthleteTime 34 "Qualifying" "" "1:46.993"
  addAthleteTime 29 "Qualifying" "" "DNS"
  addAthleteTime 42 "Qualifying" "" "DNS"

-- addAthleteTime 1 "WomenDownRiver" "Round1" "1:58.546"
-- addAthleteTime 3 "WomenDownRiver" "Round1" "2:03.697"
-- addAthleteTime 4 "WomenDownRiver" "Round1" "2:04.358"
-- addAthleteTime 5 "WomenDownRiver" "Round1" "2:07.985"
-- addAthleteTime 2 "WomenDownRiver" "Round1" "2:13.271"
-- addAthleteTime 7 "WomenDownRiver" "Round1" "2:26.220"
-- addAthleteTime 6 "WomenDownRiver" "Round1" "2:19.584"

-- addAthleteTime 1 "WomenDownRiver" "Round2" "1:57.542"
-- addAthleteTime 3 "WomenDownRiver" "Round2" "2:01.708"
-- addAthleteTime 4 "WomenDownRiver" "Round2" "2:05.288"
-- addAthleteTime 5 "WomenDownRiver" "Round2" "2:04.870"
-- addAthleteTime 2 "WomenDownRiver" "Round2" "2:29.523"
-- addAthleteTime 7 "WomenDownRiver" "Round2" "2:18.127"
-- addAthleteTime 6 "WomenDownRiver" "Round2" "DNF"

-- Add panelty
{-
addAthletePenalty bibNo rnd penalty' penaltyDescription
-}
addPanelty = do
  addAthletePenalty 22 "Qualifying" "" 50 "Gate 10"
  addAthletePenalty 43 "Qualifying" "" 100 "Gate 08, 10"
  addAthletePenalty 40 "Qualifying" "" 100 "Gate 08, 10"
  addAthletePenalty 47 "Qualifying" "" 100 "Gate 07"
  addAthletePenalty 53 "Qualifying" "" 50 "Gate 06"
  addAthletePenalty 44 "Qualifying" "" 50 "Gate 06"
  addAthletePenalty 36 "Qualifying" "" 50 "Gate 06"
  addAthletePenalty 51 "Qualifying" "" 100 "Gate 06, 10"
  addAthletePenalty 70 "Qualifying" "" 100 "Gate 06, 07"
  addAthletePenalty 54 "Qualifying" "" 100 "Gate 06, 07"
  addAthletePenalty 48 "Qualifying" "" 100 "Gate 06, 07"
  addAthletePenalty 30 "Qualifying" "" 150 "Gate 06, 07, 10"
  addAthletePenalty 38 "Qualifying" "" 150 "Gate 06, 07, 10"
  addAthletePenalty 41 "Qualifying" "" 250 "Gate 06, 07, 08, 09, 10"
  addAthletePenalty 34 "Qualifying" "" 100 "Gate 06, 07, 08, 10"

-- Heat1

-- Amature
addAmatures =
  addAthletsToStoreFromCSV "/Users/bhav/downloads/men_amature.csv"

addAmatureTimes = do
  addAthleteTime 92 "Amature" "Round1" "1:48.401"
  addAthleteTime 91 "Amature" "Round1" "1:48.471"
  addAthleteTime 81 "Amature" "Round1" "1:55.779"
  addAthleteTime 90 "Amature" "Round1" "2:37.677"
  addAthleteTime 89 "Amature" "Round1" "1:58.869"
  addAthleteTime 82 "Amature" "Round1" "2:09.672"
  addAthleteTime 83 "Amature" "Round1" "2:27.499"
  addAthleteTime 88 "Amature" "Round1" "2:16.658"
  addAthleteTime 87 "Amature" "Round1" "2:22.078"
  addAthleteTime 86 "Amature" "Round1" "2:38.056"
  addAthleteTime 85 "Amature" "Round1" "DNS"

  addAthleteTime 92 "Amature" "Round2" "1:43.658"
  addAthleteTime 91 "Amature" "Round2" "1:63.023"
  addAthleteTime 81 "Amature" "Round2" "DNF"
  addAthleteTime 90 "Amature" "Round2" "1:57.559"
  addAthleteTime 89 "Amature" "Round2" "2:00.349"
  addAthleteTime 82 "Amature" "Round2" "2:10.052"
  addAthleteTime 83 "Amature" "Round2" "2:10.836"
  addAthleteTime 88 "Amature" "Round2" "DNF"
  addAthleteTime 87 "Amature" "Round2" "2:23.675"
  addAthleteTime 86 "Amature" "Round2" "2:32.270"
  addAthleteTime 85 "Amature" "Round2" "DNS"

listAmatures = do
  wa <- getAthletesInSequenceForRounds "MEN_AMATURE" [["Amature"]] "Amature" []
  athletesToCSV "Amature" wa

addIntermediateWomen = do
  addAthleteTime 21 "QualifyingWomen" "" "1:44.221"
  addAthleteTime 22 "QualifyingWomen" "" "2:00.284"
  addAthleteTime 23 "QualifyingWomen" "" "1:42.705"
  addAthleteTime 24 "QualifyingWomen" "" "1:56.676"
  addAthleteTime 25 "QualifyingWomen" "" "1:48.837"
  addAthleteTime 26 "QualifyingWomen" "" "DNS"
  addAthleteTime 27 "QualifyingWomen" "" "DNF"
  addAthleteTime 28 "QualifyingWomen" "" "DNF"
  addAthleteTime 29 "QualifyingWomen" "" "3:06.595"
  addAthleteTime 30 "QualifyingWomen" "" "DNF"
  addAthleteTime 31 "QualifyingWomen" "" "2:10.746"

addIntermediateWomenPenalty = do
  addAthletePenalty 31 "QualifyingWomen" "" 70 "Gate 2"
  addAthletePenalty 29 "QualifyingWomen" "" 80 "Gate 1"
  addAthletePenalty 27 "QualifyingWomen" "" 120 "Gate 2,4"
  addAthletePenalty 30 "QualifyingWomen" "" 180 "Gate 2,3,4"
