{-# LANGUAGE BlockArguments #-}
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
                  { roundName = "WomenDownRiver",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    sortCriteria = BestDuration,
                    selectionRounds = [],
                    roundSelectionCriteria = TakeTop32,
                    roundCategory = "PRO_WOMEN",
                    subRounds =
                      [ ( Round
                            { roundName = "Round1",
                              roundDuration = 0.0,
                              rank = 9999,
                              penalty = [],
                              timeStr = "",
                              isCurrentRound = False,
                              currentSubRoundName = "",
                              sortCriteria = BestDuration,
                              selectionRounds = [],
                              roundSelectionCriteria = TakeTop32,
                              roundCategory = "PRO_WOMEN",
                              subRounds = [],
                              subRoundsSelectionCriteria = BestOfAll
                            }
                        ),
                        ( Round
                            { roundName = "Round2",
                              roundDuration = 0.0,
                              rank = 9999,
                              penalty = [],
                              timeStr = "",
                              isCurrentRound = False,
                              currentSubRoundName = "",
                              sortCriteria = BestDuration,
                              selectionRounds = [],
                              roundSelectionCriteria = TakeTop32,
                              roundCategory = "PRO_WOMEN",
                              subRounds = [],
                              subRoundsSelectionCriteria = BestOfAll
                            }
                        )
                      ],
                    subRoundsSelectionCriteria = BestOfAll
                  }
              ),
              ( Round
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
                      [],
                    subRoundsSelectionCriteria = BestOfAll
                  }
              ),
              ( Round
                  { roundName = "Heat1",
                    roundDuration = 0.0,
                    rank = 9999,
                    penalty = [],
                    timeStr = "",
                    subRounds = [],
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
                    subRoundsSelectionCriteria = BestOfAll,
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
      let csvFile = "/Users/bhav/Downloads/pro_women.csv"
      addAtheletesResp <- runIO $ addAthletsToStoreFromCSV csvFile
      it "add Atheletes from CSV file " $ do
        addAtheletesResp `shouldBe` "200"

{-
exec: addAthleteTime bibNo rnd timeStr

BIB Time
-}
updateAthelteTimeForQualifying = do
  addAthleteTime 1 "WomenDownRiver" "Round1" "1:58.546"
  addAthleteTime 3 "WomenDownRiver" "Round1" "2:03.697"
  addAthleteTime 4 "WomenDownRiver" "Round1" "2:04.358"
  addAthleteTime 5 "WomenDownRiver" "Round1" "2:07.985"
  addAthleteTime 2 "WomenDownRiver" "Round1" "2:13.271"
  addAthleteTime 7 "WomenDownRiver" "Round1" "2:26.220"
  addAthleteTime 6 "WomenDownRiver" "Round1" "2:19.584"

  addAthleteTime 1 "WomenDownRiver" "Round2" "1:57.542"
  addAthleteTime 3 "WomenDownRiver" "Round2" "2:01.708"
  addAthleteTime 4 "WomenDownRiver" "Round2" "2:05.288"
  addAthleteTime 5 "WomenDownRiver" "Round2" "2:04.870"
  addAthleteTime 2 "WomenDownRiver" "Round2" "2:29.523"
  addAthleteTime 7 "WomenDownRiver" "Round2" "2:18.127"
  addAthleteTime 6 "WomenDownRiver" "Round2" "DNF"

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
