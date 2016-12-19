{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Data.Csv
import qualified Data.Vector as V
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as M
import Control.Applicative
import Data.Foldable
import System.IO
import System.Exit
import Statistics.Correlation
import Data.List
import Data.Ord
import Text.Layout.Table
import Text.Blaze.Html5 as H hiding (main)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Utf8

type PlayerID = String

data Batting = Batting
    { battingID :: PlayerID
    , atBats    :: !Int
    , homeruns  :: !Int
    } deriving Show

data Player = Player
    { playerID  :: PlayerID
    , nameGiven :: String
    , nameLast  :: String
    , birthYear :: Integer
    } deriving Show

ppName :: Player -> String
ppName p = nameGiven p ++ " " ++ nameLast p

instance FromNamedRecord Batting where
    parseNamedRecord m =
        Batting <$> m .: "playerID"
                <*> (m .: "AB" <|> pure 0)
                <*> (m .: "HR" <|> pure 0)

instance FromNamedRecord Player where
    parseNamedRecord m =
        Player  <$> m .: "playerID"
                <*> m .: "nameGiven"
                <*> m .: "nameLast"
                <*> (m .: "birthYear" <|> pure 1900)

addBattingSums :: Batting -> Batting -> Batting
addBattingSums b1 b2 = Batting
    { battingID = battingID b1
    , atBats = atBats b1 + atBats b2
    , homeruns = homeruns b1 + homeruns b2
    }

{-
Far too slow!
type BattingSums = [Batting]

initBattingSum = []

tallyBattingSum :: BattingSums -> Batting -> BattingSums
tallyBattingSum [] b = [b]
tallyBattingSum (x:xs) b
    | battingID x == battingID b
    = Batting { battingID = battingID x
              , atBats = atBats x + atBats b
              , homeruns = homeruns x + homeruns b
              } : xs
    | otherwise
    = x : tallyBattingSum xs b
-}


type BattingSums = M.Map PlayerID Batting

initBattingSum = M.empty

tallyBattingSum :: BattingSums -> Batting -> BattingSums
tallyBattingSum m b = M.insertWith addBattingSums (battingID b) b m

homerunProb :: Batting -> Double
homerunProb b | atBats b > 0 = fromIntegral (homeruns b) / fromIntegral (atBats b)
              | otherwise    = 0


mkHtmlStats :: Int -> Double -> [(String, Double)] -> Html
mkHtmlStats n corr top10 = docTypeHtml $ do
     H.head $ do
        H.title "My baseball stats"
     body $ do
        h1 "My baseball stats"
        h2 "General statistics"
        p $ do
            "We have stats for "
            toHtml n
            " players."
        p $ do
            "Homerun probability correlates with birth year by "
            toHtml corr
            "."
        h2 "Top 10"
        table $ do
            tr $ do
                th "Player"
                th "Prob"
            for_ top10 $ \(p,b) ->
                tr $ do
                    td (toHtml p)
                    td (toHtml b)

main = do
    rawBatting <- LBS.readFile "Batting.csv"
    batting <- case decodeByName rawBatting of
        Left error -> do
            hPutStrLn stderr error
            exitFailure
        Right (_header, players) -> return players

    rawPlayers <- LBS.readFile "Master.csv"
    players <- case decodeByName rawPlayers of
        Left error -> do
            hPutStrLn stderr error
            exitFailure
        Right (_header, players) -> return players

    let battingSums = V.foldl tallyBattingSum initBattingSum batting
    let playerMap = M.fromList [ (playerID p, p) | p <- V.toList players ]
    let stats = M.intersectionWith (,) playerMap battingSums

    let cor = pearson $ V.fromList
                [ (fromIntegral (birthYear p), homerunProb b)
                | (p,b) <- M.elems stats ]

    let top10 = take 10 $ sortOn (Down . homerunProb . snd) $ M.elems stats

    putStrLn $ "Correlation: " ++ show cor
    putStrLn $ show (length stats) ++ " player stats found."

    putStrLn $ "Top 10:"
    -- for_ top10 $ \(p,b) -> putStrLn $ ppName p ++ ": " ++ show (homerunProb b)
    putStrLn $ tableString [def, numCol]
       unicodeS
       (titlesH ["Name", "Prob"])
       [ rowsG [ [ppName p, show (homerunProb b) ] | (p,b) <- top10 ] ]

    let htmlFile = mkHtmlStats (length stats) cor
            [ (ppName p, homerunProb b) | (p,b) <- top10 ]
    LBS.writeFile "stats.html" (renderHtml htmlFile)


