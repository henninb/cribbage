import Cribbage
import Test.Hspec

fourClubs = makeCard "Four Clubs"
aceHearts = makeCard "Ace Hearts"
aceDiamonds = makeCard "Ace Diamonds"
fourDiamonds = makeCard "Four Diamonds"
fiveDiamonds = makeCard "Five Diamonds"
fiveHearts = makeCard "Five Hearts"
fiveSpades = makeCard "Five Spades"
fiveClubs = makeCard "Five Clubs"
sixHearts = makeCard "Six Hearts"
sevenHearts = makeCard "Seven Hearts"
jackDiamonds = makeCard "Jack Diamonds"
eightDiamonds = makeCard "Eight Diamonds"
sixDiamonds = makeCard "Six Diamonds"
twoDiamonds = makeCard "Two Diamonds"
tenDiamonds = makeCard "Ten Diamonds"
jackClubs = makeCard "Jack Clubs"
sevenClubs = makeCard "Seven Clubs"
eightClubs = makeCard "Eight Clubs"
tenClubs = makeCard "Ten Clubs"

spec :: Spec
spec =
    describe "Valid Cribbage hand Counts" $ do
      it "Shows that runs, fifteens, and pairs count is valid" $ do
        scoreTheHand False jackDiamonds [fourClubs, fourDiamonds, fiveClubs, sixHearts] `shouldBe` 14
      it "Shows that run of four count is valid" $ do
        scoreTheHand False jackDiamonds [fourClubs, sevenHearts, fiveClubs, sixHearts] `shouldBe` 8
      it "Shows that run of five is valid" $ do
        scoreTheHand False eightDiamonds [fourClubs, sevenHearts, fiveClubs, sixHearts] `shouldBe` 9
      it "Shows a 29 hand count is valid" $ do
         scoreTheHand False fiveClubs [fiveSpades, fiveDiamonds, fiveHearts, jackClubs] `shouldBe` 29
      it "Shows a 28 hand count is valid" $ do
         scoreTheHand False jackClubs [fiveSpades, fiveDiamonds, fiveHearts, fiveClubs] `shouldBe` 28
      it "Shows a 24 hand count is valid" $ do
         scoreTheHand False sixHearts [sevenHearts, eightDiamonds, sevenClubs, eightClubs] `shouldBe` 24
      it "Shows a zero hand count is valid" $ do
         scoreTheHand False sixHearts [jackDiamonds, eightDiamonds, fourDiamonds, tenClubs] `shouldBe` 0
      it "Shows a hisNobs hand count is valid" $ do
         scoreTheHand False fourDiamonds [jackDiamonds, eightDiamonds, tenClubs, sixHearts] `shouldBe` 1
      it "Shows a four card non crib flush hand count is valid" $ do
         scoreTheHand False tenClubs [jackDiamonds, fourDiamonds, eightDiamonds, sixDiamonds] `shouldBe` 4
      it "Shows a five card non crib flush hand count is valid" $ do
         scoreTheHand False tenDiamonds [twoDiamonds, fourDiamonds, eightDiamonds, sixDiamonds] `shouldBe` 5
      it "Shows a four card crib flush hand count is valid" $ do
         scoreTheHand True tenClubs [jackDiamonds, fourDiamonds, eightDiamonds, sixDiamonds] `shouldBe` 0
main :: IO ()
main = hspec spec
