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
                panelty = [],
                timeStr = "",
                subRounds = [],
                isCurrentRound = False,
                currentSubRoundName = "",
                roundCategory = "PRO",
                selectionRounds = [],
                timeSelectionCriteria = BestDuration,
                roundSelectionCriteria = AscendingDuration
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
                    panelty = [],
                    timeStr = "",
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = [],
                    roundSelectionCriteria = AscendingDuration,
                    roundCategory = "PRO",
                    subRounds =
                      [ Round
                          { roundName = "Heat 1",
                            roundDuration = 0.0,
                            panelty = [],
                            timeStr = "",
                            subRounds = [],
                            isCurrentRound = False,
                            currentSubRoundName = "",
                            roundCategory = "PRO",
                            timeSelectionCriteria = BestDuration,
                            selectionRounds = [],
                            roundSelectionCriteria = AscendingDuration
                          },
                        Round
                          { roundName = "Heat 2",
                            roundDuration = 0.0,
                            panelty = [],
                            timeStr = "",
                            subRounds = [],
                            isCurrentRound = False,
                            currentSubRoundName = "",
                            roundCategory = "PRO",
                            timeSelectionCriteria = BestDuration,
                            selectionRounds = [],
                            roundSelectionCriteria = AscendingDuration
                          }
                      ]
                  }
              ),
              ( Round
                  { roundName = "Heat1",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H1Take1_16_17_32
                  }
              ),
              ( Round
                  { roundName = "Heat2",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H2Take8_9_25_25
                  }
              ),
              ( Round
                  { roundName = "Heat3",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H3Take5_12_21_28
                  }
              ),
              ( Round
                  { roundName = "Heat4",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H4Take4_13_20_29
                  }
              ),
              ( Round
                  { roundName = "Heat5",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H5Take3_14_19_30
                  }
              ),
              ( Round
                  { roundName = "Heat6",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H6Take6_11_22_27
                  }
              ),
              ( Round
                  { roundName = "Heat7",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H7Take7_10_23_26
                  }
              ),
              ( Round
                  { roundName = "Heat8",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Qualifying"],
                    roundSelectionCriteria = H8Take2_15_18_31
                  }
              ),
              -- QuarterFinals
              ( Round
                  { roundName = "QF1",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Heat1", "Heat2"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "QF2",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Heat3", "Heat4"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "QF3",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Heat5", "Heat6"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "QF4",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["Heat7", "Heat8"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              -- SemiFinals
              ( Round
                  { roundName = "SF1",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["QF1", "QF2"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              ( Round
                  { roundName = "SF2",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
                    selectionRounds = ["QF3", "QF4"],
                    roundSelectionCriteria = TakeTop2
                  }
              ),
              -- Finals
              ( Round
                  { roundName = "Final",
                    roundDuration = 0.0,
                    panelty = [],
                    timeStr = "",
                    subRounds = [],
                    isCurrentRound = False,
                    currentSubRoundName = "",
                    roundCategory = "PRO",
                    timeSelectionCriteria = BestDuration,
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
