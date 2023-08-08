{-# LANGUAGE TemplateHaskell, DeriveGeneric, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, OverloadedStrings #-}

-- | A socket that recieves several encodings, and responds with actions from another process. (I used this with a python program that runs a RL gymnastics.)
module AgentSocket where

import Network.Simple.TCP
    ( recv, send, serve, Socket, HostPreference(Host) )
import Data.Aeson ( decode, encode, FromJSON, ToJSON )
import GHC.Generics ( Generic )
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Applicative ((<|>))
import Data.Either.Extra (mapLeft)
import Control.Concurrent.MVar
    ( swapMVar, newMVar, readMVar, MVar )
import Control.Lens ((^.),makeLenses, (.~), (&))
import Data.List.Split (chunksOf, splitPlaces)
import Data.List(span, maximum, elemIndex)
import GHC.Natural (intToNatural, naturalToInt)
import Debug.Trace (trace)
import Data.Bifunctor as BF (first)

import SRC.SDR
    ( SDRRange(..), SDR(..), maxIndex, minIndex, sdr, sdrRange )
import SRC.Package ( Package(_conS, _conR, _value), conH, conR )
import SRC.CommonDataTypes ( BitIndex )
import SRC.Encoder.Config ( EncoderConfig, EncoderType )
import SRC.Region.Config
    ( RegionConfig(_nrOfColumns), nrOfCellsPerColumn, nrOfColumns )
import SRC.Region.Model
    ( CellState(ActivePredictiveCell, ActiveCell),
      cell,
      col,
      Cell,
      cellId,
      cellState,
      Region,
      cells,
      columnState,
      currentStep )
import SRC.Region.Utils ( initRegion )
import SRC.Encoder.Numeric as SDRNum ( getRange, encode )
import REPL ( initEncoderConfig, initRegionConfig, initConfigs )
import SRC.HTM.Config ( learningEnabled, temporalConfig )
import SRC.HTM.HTM ( spatialPooler, temporalPooler, switch )



newtype DecoderConfig = DecoderConfig {
    _categories :: BitIndex -- must be devisible with the SDR value
} deriving (Show, Generic)


makeLenses ''DecoderConfig

newtype ToggleFeatures = ToggleFeatures {
    _learn :: Bool--,
    --_selfPred :: Bool
} deriving (Show, Generic)

makeLenses ''ToggleFeatures

instance FromJSON ToggleFeatures
instance FromJSON EncoderType
instance FromJSON EncoderConfig

data Responce = Responce {status::String, action:: Int} deriving (Show, Generic)
instance ToJSON Responce

---
-- States

type EncodingState = MVar (Maybe [Maybe EncoderConfig])

type Agent = MVar (Maybe [(Package, Region)])


---

main :: IO ()
main = do
    state <- newMVar Nothing
    agent <- newMVar Nothing -- Defining region must be done after Encoder is defined
    serve (Host "127.0.0.1") "5000" $ \(socket, _) -> handleConnection agent state socket

handleConnection :: Agent -> EncodingState -> Socket -> IO ()
handleConnection agent state socket = do
    --putStrLn "[ INFO ] --- New client connected."
    msg <- recv socket 4096
    case msg of
        Nothing -> return () ---putStrLn "[ INFO ] --- Client disconnected prematurely."
        Just bs -> do
            let recievedMessage = BL.fromStrict bs
            case decode recievedMessage :: Maybe [Maybe EncoderConfig] of
                Nothing -> do 
                    case decode recievedMessage :: Maybe [Maybe Int] of 
                        Nothing -> do 
                            send socket (BL.toStrict $ Data.Aeson.encode $ Responce "Error" 0)
                            case decode recievedMessage :: Maybe ToggleFeatures of
                                Nothing -> do 
                                    --putStrLn "[ INFO ] --- Failed to parse JSON."
                                    send socket (BL.toStrict $ Data.Aeson.encode $ Responce "Error" 0)
                                Just toggles -> do
                                    --Toggle learning in Agent 
                                    agentRegion <- readMVar agent
                                    case agentRegion of 
                                        Nothing -> return ()  -- putStrLn "[ INFO ] --- Agent is not constructed: "
                                        Just agentR -> do
                                            --let updated = map (\(p,r) -> (p & (conH .temporalConfig .learningEnabled) .~ (toggleLearning^.learn), r)) agentR -- [(Package,Region)] 
                                            let updated = map (BF.first ((conH . temporalConfig . learningEnabled).~ (toggles ^. learn))) agentR -- [(Package,Region)] 
                                            --let updated2 = map (BF.first ((conH . temporalConfig . selfPredict).~ (toggles ^. selfPred))) updated
                                            swapMVar agent $ Just updated
                                            send socket (BL.toStrict $ Data.Aeson.encode $ Responce "Success" 0)
                        Just values -> do 
                            --putStrLn $ "[ INFO ] --- Received values: " ++ show values
                            encodingsExist <- readMVar state
                            case encodingsExist of
                                Nothing -> send socket (BL.toStrict $ Data.Aeson.encode $ Responce "Error: Please send encoding first, there is no encoding" 0)
                                Just ex -> do
                                    if length ex /= length values 
                                        then send socket (BL.toStrict $ Data.Aeson.encode $ Responce "Error: The observation doesn't match the preset encoding length" 0)
                                        else do
                                            agentRegion <- readMVar agent
                                            case agentRegion of 
                                                Nothing -> return ()  -- putStrLn "[ INFO ] --- Agent is not constructed: "
                                                Just agentR -> do
                                                    --putStrLn "[ INFO ] --- Handling observation "
                                                    (learnedAgent, action) <- handleObservation agentR ex values
                                                    -- update agentR state
                                                    swapMVar agent (Just learnedAgent) 
                                                    send socket (BL.toStrict $ Data.Aeson.encode $ Responce "Success" action)
                Just encodings -> do 
                    --putStrLn $ "[ INFO ] --- Received encodings: " ++ show encodings
                    case mergeEncodings encodings of 
                        Nothing -> do 
                                --putStrLn $ "[ INFO ] --- Receieved invalid encoding: " ++ show encodings
                                send socket (BL.toStrict $ Data.Aeson.encode $ Responce "Failure: invalid encoding receieved" 1)
                        Just sdrRange -> do
                            mRegions <- createRegions sdrRange -- SDRRange -> IO (Maybe [(Package, Region)])
                            swapMVar state (Just encodings)
                            swapMVar agent mRegions 
                            --putStrLn "[ INFO ] --- Encoding recieved and Agent is constructed"
                            send socket (BL.toStrict $ Data.Aeson.encode $ Responce "Success" 1)



handleObservation :: [(Package,Region)] -> [Maybe EncoderConfig] -> [Maybe Int] -> IO ([(Package, Region)], Int) --IO [Maybe Int]
handleObservation agent ex values = do
        let v = zipWith toSDR ex values -- [(encoderConfig, Int)]
        --print v
        let a = concatSDRs v -- Maybe SDR
        case a of 
            Nothing -> return (agent, 0)-- Happends if the values and encoding don't match. E.g. out of range
            Just sdr -> do -- from SDR to Categorical value
                    -- ask the agent for action
                    getAction agent sdr


                    

getAction :: [(Package, Region)] -> SDR -> IO ([(Package, Region)], Int)
getAction agent sdr = do
    [pr1, pr2] <- applyHTM True agent sdr -- TODO: switch and get the pr2 before switch, and see if it learns
    let outputSDR = regionToSDR (fst pr2^.conR) (snd pr2) ---(snd pr2)--(r2 before switch)
   -- case fromSDR (DecoderConfig 2) (trace ("[ TRACE ] --- outputSDR is :" ++ show outputSDR) outputSDR) of -- we should applyHTM here SDR -> Region -> SDR
    case fromSDR (DecoderConfig 2) outputSDR of -- we should applyHTM here SDR -> Region -> SDR
        Nothing -> return ([pr1, pr2], 0) -- default action
        Just y -> return ([pr1, pr2], naturalToInt y) -- final action


---
mergeEncodings :: [Maybe EncoderConfig] -> Maybe SDRRange
mergeEncodings = foldSDRange . map (>>= getRange) 

--[Maybe EncoderConfig] -> [Maybe SDRRange] -> MaybeSDRRange


foldSDRange :: [Maybe SDRRange] -> Maybe SDRRange
foldSDRange [] = Nothing
foldSDRange (x:xs) = foldl combineSDRRange x xs

combineSDRRange :: Maybe SDRRange -> Maybe SDRRange -> Maybe SDRRange
combineSDRRange (Just x) (Just y)  = Just $ SDRRange {
                        _minIndex = min (x ^.minIndex) (y ^.minIndex), -- preserve 0
                        _maxIndex = (x ^.maxIndex) + (y ^.maxIndex) -- combine to get maximum
                        }
combineSDRRange _ _ = Nothing

---

toSDR :: Maybe EncoderConfig ->  Maybe Int -> Maybe SDR
toSDR x y = do
    a <- x
    b <- y
    SDRNum.encode a b


    
{-
EncoderConfig musn't empty first.
- I could use a DataIORef to store the data I want
- I could use StateMonad to store the data too. There is problem with this that it doesn't share state accross connections
- Maybe I can use MVar or TVar, a thread-safe concurrency primiative. 
- I used MVar to store a Encoding state, which can be replaced any time


Compare the length of EncoderConfig & Numbers
- Read the length of EncoderConfig and Number and return failiure


Merge SDRs into larger SDR
- Change it at the Config? [Encodingi] -> Encodingj & mapper from Value -> Encodingi -> Encodingj
- Change it after Config? [SDR] -> SDR

SDR -> SDR -> Int

-}


----
-- Merge SDRs into a single SDR

concatSDRs :: [Maybe SDR] -> Maybe SDR
concatSDRs [] = Nothing 
concatSDRs (x:xs) = foldl combineSDR x xs


combineSDR :: Maybe SDR -> Maybe SDR -> Maybe SDR
combineSDR a b = do
            x <- a
            y <- b
            Just $ SDR {
                    _sdr = (x^.sdr) ++ map (+ x ^. (sdrRange . maxIndex)) (y^.sdr),
                    _sdrRange = SDRRange {
                        _minIndex = min  (x ^. (sdrRange . minIndex)) (y ^. (sdrRange . minIndex)) , -- preserve 0
                        _maxIndex = (x ^. (sdrRange . maxIndex)) + (y ^. (sdrRange . maxIndex)) -- combine to get maximum
                        }
                    }



----

-- Parse SDR to Categorical Value

splitIntoBuckets :: Int -> Int -> [Int]
splitIntoBuckets max k = 
    let (bucketSize, remainder) = max `divMod` k
        sizes = replicate remainder (bucketSize + 1) ++ replicate (k - remainder) bucketSize
    in  init $ tail $ scanl (+) 0 sizes

splitRangeIntoSubList :: BitIndex -> SDRRange -> Maybe [BitIndex]
splitRangeIntoSubList n range = let size = range^.maxIndex - range^.minIndex
                                in if size > n
                                    then Just $ map intToNatural $ splitIntoBuckets (naturalToInt size) (naturalToInt n) --enumFromThenTo (range^.minIndex) step (range^.maxIndex)
                                    else Nothing


splitAtPositions :: [BitIndex] -> [BitIndex] -> [Int]
splitAtPositions xs [] = if null xs
      then [0]
      else [length xs]
splitAtPositions xs (y:ys) = 
    let (front, back) = span (< y) xs
    in if null front
      then 0 : splitAtPositions back ys
      else length front : splitAtPositions back ys


indexOfMax :: Ord a => [a] -> Maybe BitIndex
indexOfMax xs = do 
                x <- elemIndex (maximum xs) xs 
                Just $ intToNatural x


fromSDR ::  DecoderConfig -> SDR -> Maybe BitIndex
fromSDR decoder s = let a = splitRangeIntoSubList (decoder^.categories) (s^.sdrRange)
                        in  case a of 
                                Nothing -> Nothing
                               -- Just a -> indexOfMax $ map length $ splitAtPositions (trace ("[ TRACE ] --- sdr is: " ++ show (s^.sdr))(s^.sdr)) (trace ("[ TRACE ] --- split is " ++ show a) a)
                                Just a -> indexOfMax $ splitAtPositions (s^.sdr) a


---

-- Region -> Output categorical

-- Region ^.currentStep :: [Column] :: [[Cell]] -> SDR :: [BitIndex] + Range
-- Nested list -> List with indecies (assume all columns contain equal number of cells, then Range (0, cellsPerColumn * columns))
-- iterate and Collect all index(column index * nr_cells + cell index) cells that are active 



regionToSDR :: RegionConfig -> Region -> SDR
regionToSDR con r = SDR {
    _sdr = map (toBitIndex con) $ filter isActive $ concatMap (^.cells) (r^.currentStep),--(r^.currentStep),
    _sdrRange = SDRRange {
            _minIndex = 0,
            _maxIndex = (con^. nrOfCellsPerColumn) * (con^. nrOfColumns)
            }
        }


isActive :: Cell -> Bool
isActive c = (c^.cellState == ActiveCell) || (c^.cellState == ActivePredictiveCell) -- TODO change to ActiveCell


toBitIndex ::  RegionConfig -> Cell -> BitIndex
toBitIndex con c = c^. (cellId . col) * con^.nrOfCellsPerColumn + c^.( cellId . cell)


---

-- Append Region  :: RegionsConfig -> DAG of Regions

-- Start with Region -> Region ::  (inputField:Region) -> SDR -> (inputField:Region)?
-- map _inputField for each column to cells from the other Region
-- connect regions ::  Region -> Region -> RegionMapping
-- RegionMapping {colInOutputRegion -> cellIDInInputRegion}
-- SpatialPooling :: inputSDR -> inputField -> Column State Active/Not

-- To match two regions: 
    -- I need to initialize the inputField to map to the cells of the first region at construction (I need the range of the inputField be the same as the number of cells)
    -- When Value is passed through the first, then it should be able to pass through the second with Spetial pooling
-- Region2^.columns^._inputField = Region1^.nrOfColumns * Region1^. nrofCellsPerColumn
-- Region initInputField uses EncoderConfig
-- Maybe the regions EncoderConfig should an alias for InputSDRConfig or something
-- getRange EncoderConfig and Just Range are not quite the same
-- getRange is useful for InputSDR that come from real values, vs inputSDRConfig can come from both value and otherRegions
-- inputSDRConfig is a superset of EncoderConfig


encoderToInputSDRConfig :: EncoderConfig -> Maybe SRC.SDR.SDRRange
encoderToInputSDRConfig = getRange

-- return getRange 
regionToInputSDRConfig :: RegionConfig -> Maybe SRC.SDR.SDRRange
regionToInputSDRConfig r = Just $ SRC.SDR.SDRRange 0 (r^.nrOfCellsPerColumn * r^.nrOfColumns)

---

-- Combine regions

-- create 2 regions and combine them

createRegions :: SDRRange -> IO (Maybe [(Package, Region)])
createRegions range = do
    let conR1 = initRegionConfig {_nrOfColumns=40}
    let range = regionToInputSDRConfig conR1
    case range of
        Nothing -> return Nothing
        Just r -> do
            let conR2 = initRegionConfig {_nrOfColumns=20}
            packageConR1 <- initConfigs 
            let packaeConR1t = packageConR1 {_conR = conR1, _conS = range}
            packageConR2 <- initConfigs 
            let packageConR2t = packageConR2 {_conR = conR2, _conS = range}

            a <- mapM getout [packaeConR1t, packageConR2t]
            return $ Just a


getout :: Package -> IO (Package, Region)
getout p = do
    a <- initRegion (_conS p) (_conR p)
    return (p,a)


---

-- test encoderConfig -> IO [Region]

{-testCreateRegions :: IO (Maybe [Region])
testCreateRegions = do
    createRegions initEncoderConfig-}


--- 
-- SDR1 = toSDR val
-- apply spacial and temporal to r1 <- SDR1
    -- r1_temp = spatialPooler p {_value = SDR1} r1 
    -- r1_out = temporalPooler p r1_temp
    -- SDR2 = regionToSDR r1_out
    -- r1 = switch r1_out

-- apply spacial and temporal to r2 <- SDR2
    -- r2_temp = spatialPooler p {_value = SDR2} r1 
    -- r2_out = temporalPooler p r2_temp
    -- SDR2 = regionToSDR r2_out
    -- r2 = switch r2_out

-- regionToSDR ::  Region -> SDR


---
applyHTM :: Bool -> [(Package, Region)] -> SDR -> IO [(Package, Region)]
applyHTM b [] sdr = return []
applyHTM b [(p,r)] sdr = do
    --r_out <-  maybeSwitch b <$> htm (p,r) (trace ("[ TRACE ] --- inputSDR last:" ++ show sdr) sdr)
    r_out <-  maybeSwitch b <$> htm (p,r) sdr
    return [(p, r_out)]
applyHTM b ((p,r):prs) sdr = do
    --r_out <- maybeSwitch b <$> htm (p,r) (trace ("[ TRACE ] --- inputSDR:" ++ show sdr) sdr) -- switch optionally
    r_out <- maybeSwitch b <$> htm (p,r) sdr
    prst <- applyHTM b prs (regionToSDR (p^.conR) r_out)
    return $ (p, r_out) : prst

maybeSwitch :: Bool -> Region -> Region
maybeSwitch b r = if b then switch r else r

htm :: (Package, Region) -> SDR -> IO Region
htm (p, r) sdr = do 
    let spec = spatialPooler p {_value = sdr} r
    --temp <- temporalPooler p (trace ("[ TRACE ] --- afterSpacialPooler:" ++ showCells spec) spec)
    temporalPooler p spec
    --return (trace ("[ TRACE ] --- afterTemporalPooler:" ++ showCells temp) temp)
    --return temp

---



--- Display Region parts
showColumns :: Region -> String
showColumns r =  show $ concatMap (show . (^.columnState)) (r^.currentStep)

showCells :: Region -> String 
showCells r =  show $ concatMap (concatMap (show . (^.cellState)) . (^.cells)) (r^.currentStep)

-- advansed :: DAG Region -> Val -> Dag Region
-- evenMoreAdvanced :: State (DAG Region) Val -> State (Dag Region) ()

--
-- Show :: Region -> String -- show cells or columnsstates of a region

---
{- main :: Region -> Region -> IO SDR
main conR1 conR2 = do 
        r1 <- initRegion (getRange encoding) conR1
        r2 <- initRegion (regionToInputSDRConfig conR1) conR2
        regions <- [r1,r2] 
        new_regions <- simplified regions (toSDR val)
        return regionToSDR (r2 before switch)-}


main2 :: Int -> IO (Maybe BitIndex)
main2 int = do
    let e = initEncoderConfig
    case getRange e of 
        Nothing -> return Nothing
        Just b -> do 
            a <- createRegions b
            case a of 
                Nothing -> return Nothing
                Just prs -> do
                    let val = Just int
                    case toSDR (Just e) val of
                        Nothing -> return Nothing
                        Just sdr -> do
                            [pr1, pr2] <- applyHTM False prs sdr
                            let a = regionToSDR (fst pr2^.conR) (snd pr2) --(r2 before switch)
                            return $ fromSDR (DecoderConfig 3) a





