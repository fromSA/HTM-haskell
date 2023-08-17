{-# LANGUAGE TemplateHaskell, DeriveGeneric, NoMonomorphismRestriction, FlexibleContexts, TypeFamilies, OverloadedStrings #-}

module RegionGraph where
import SRC.Region.Model
import GHC.Natural (Natural, naturalToInt, intToNatural)
import SRC.SDR
import Data.Graph.Inductive
import SRC.Package
import SRC.Region.Config
import SRC.Encoder.Numeric
import SRC.Encoder.Config
import Control.Lens as LS ((^.), makeLenses, iat,  (.~), (&), (?~), (%~)) 
import Data.Graph.Inductive.Query (topsort)
import GHC.Generics (Generic)
import SRC.HTM.Helper (initRegion2, Region2, cols)
import REPL ( initEncoderConfig, initRegionConfig, initConfigs )
import Data.Tuple.Extra (secondM)
import System.Random (Random (randomR), getStdRandom, newStdGen, randomRs)
import Data.List (nub, sort, elemIndex)
import SRC.HTM.HTM2 as HTM2
import Data.Foldable (foldlM)
import SRC.CommonDataTypes (BitIndex)
import Data.Maybe (isJust)

-- | Build a graph of the agents mind


---------------------------------------------------------
----                  Main function                 -----
---------------------------------------------------------
--- | print graph
main = do 
    --let g = myGraph
    ---let edgesInn = filter isWaterEdge $ inn g 6 -- all edges into a node
    --let nodesFrom = filter isModuleOrOutput $ map (\(a,_,_) -> lab' $ context g a) edgesInn

    --print nodesFrom
    --print $ context myGraph a
    return () --prettyPrint myGraph



---------------------------------------------------------
----             Design the data structure          -----
---------------------------------------------------------

--- | The nodes

newtype DecoderConfig = DecoderConfig Natural deriving (Show)-- must be devisible with the SDR value

data MemoryContent = MemoryContent {_currentGoal :: Maybe Region2 , _allGoals :: [Region2]} deriving (Show, Generic) 
makeLenses ''MemoryContent


-- I am not sure this should be just the SDRs. 
-- Maybe it should be the cell states, and their location in the region. 
    -- If the memory is used as a gate, it does need a region structure. or maybe not? getCell :: CellID -> Cell = CellState
    -- If the memory is used as a water, then is sufficient to have it as a region.
-- Excluding synapces they connect to?

data MyNode = Input (Maybe Int)  -- This might need to be a list of Input Value in the future
    | LocationField (Maybe SDR) -- inputfield that is initilized randomly and updates itsself 
    | InputField (Maybe SDR) (Maybe EncoderConfig) -- input that takes values from the outside
    | Module (Maybe Region2) (Maybe Package) -- this region is meant to handle a normal region
    | Output (Maybe Region2) (Maybe Package) -- this region is meant to symbolize that the module is used as an out region. TODO maybe is it more beneficial to have a same module for this and normal region.
    | Memory (Maybe MemoryContent) -- this used for handling a memory module
    | OutputValue (Maybe BitIndex) (Maybe DecoderConfig) deriving (Show)

--- | Edge
    
data Signal = Water | Gate deriving (Show, Eq) -- water = input singal, Gate = predictive signal
newtype MyEdge = MyEdge Signal deriving (Show, Eq)


-- | Define the agents mind

data NodeS = NodeScetch Int MyNode -- a nodeid and a node
data EdgeS = EdgeScetch Int Int MyEdge deriving (Show, Eq) -- from a node to another node with a Edge definition

extractNS :: NodeS -> (Int, MyNode)
extractNS (NodeScetch a b) = (a, b)
extractES :: EdgeS -> (Int, Int, MyEdge)
extractES (EdgeScetch a b c) = (a, b, c)


---
type InputSDR = SDR 
type PredSDRs = [SDR] 
type RegionConfigs = [EncoderConfig] 
data Package2 = PWater InputSDR EncoderConfig
                | PGates PredSDRs RegionConfigs
                | RegionConfig 
                | RandomGenerator
                | OutputConfig EncoderConfig  
                deriving (Show)

---------------------------------------------------------
----            Design the graph structure          -----
---------------------------------------------------------

a1Nodes :: [(Int, MyNode)]
a1Nodes = map extractNS [
        NodeScetch 0 $ Input Nothing,
        NodeScetch 1 $ InputField Nothing Nothing, -- input to obs
        NodeScetch 2 $ Module Nothing Nothing, -- [obs, loc] -> obj
        NodeScetch 3 $ Module Nothing Nothing, -- obj -> [act_opt]
        NodeScetch 4 $ Module Nothing Nothing, -- loc_old -> loc
        NodeScetch 5 $ Module Nothing Nothing, -- [obj,loc] -> loc2
        NodeScetch 6 $ Module Nothing Nothing, -- [loc2, [act_opt]] -> [loc3]
        NodeScetch 7 $ Module Nothing Nothing, -- [[loc3], goal] -> [loc_match]
        NodeScetch 8 $ Module Nothing Nothing, -- [[act_opt], [loc_match]] -> [act_match]
        NodeScetch 9 $ Output Nothing Nothing, -- [act_match] -> act
        NodeScetch 10 $ Output Nothing Nothing, -- [[act_match], [loc_match]] -> loc_new = loc_old in the next step
        NodeScetch 11 $ LocationField Nothing, -- input to loc, updated by loc_new after a run, initilized with a random SDR, should this have the same SDRRange as the loc_new?
        NodeScetch 12 $ Memory Nothing, -- Nothing -> goal
        NodeScetch 13 $ OutputValue Nothing Nothing -- act -> Action_Value :: Int
        ] 

a1Edges :: [(Int, Int, MyEdge)]
a1Edges = map extractES [
        EdgeScetch 0 1 $ MyEdge Water, -- Pass value to obs
        
        --  This might be uncessary duplicate!!!
        --  It is also a problem if we are gonna user topological sorting to process values!
        -- EdgeScetch 11 4 $ MyEdge 0 Water, -- Pass loc_old -> loc 
        
        EdgeScetch 4 2 $ MyEdge Gate, -- Pass loc -> [obs, loc] x
        EdgeScetch 1 2 $ MyEdge Water, -- Pass obs -> [obs, loc] x
        
        EdgeScetch 4 5 $ MyEdge Water, -- Pass loc -> [loc, obj] x
        EdgeScetch 2 5 $ MyEdge Water, -- Pass obj -> [loc, obj] x
        
        EdgeScetch 2 3 $ MyEdge Water, -- Pass obj -> obj x
        
        EdgeScetch 5 6 $ MyEdge Water, -- Pass loc2 -> [loc2, [act_opt]] x
        EdgeScetch 3 6 $ MyEdge Gate, -- Pass [act_opt] -> [loc2, [act_opt]] x
        
        EdgeScetch 12 7 $ MyEdge Gate, -- Pass goal -> [[loc3], goal]
        EdgeScetch 6 7 $ MyEdge Water, -- Pass [loc3] -> [[loc3], goal]  x
        
        -- Not sure if this will work.
        EdgeScetch 3 8 $ MyEdge Water, -- Pass [act_opt] -> [[act_opt], [loc_match]] x
        EdgeScetch 7 8 $ MyEdge Gate, -- Pass [loc_match] -> [[act_opt], [loc_match]] x 
        
        EdgeScetch 8 9 $ MyEdge Water, -- Pass [act_match] -> act x
       
        EdgeScetch 8 10 $ MyEdge Water, -- Pass [act_match] -> [[act_match], [loc_match]] x
        EdgeScetch 7 10 $ MyEdge Water, -- Pass [loc_match] -> [[act_match], [loc_match]] x
        
        EdgeScetch 9 13 $ MyEdge Water, -- Pass act -> ActionValue x
        EdgeScetch 10 11 $ MyEdge Water -- Pass loc_new -> loc_old : Replace loc_old with loc_new  x
        ]


getRandomSDR :: Maybe SDRRange  -> IO (Maybe SDR)
getRandomSDR conR = do
    case conR of 
        Nothing -> return Nothing
        Just conRV -> do
            gen <- newStdGen
            let high = naturalToInt $ conRV^.minIndex
            let low = naturalToInt $ conRV^.maxIndex
            let rangeSize = high - low + 1
            let subsetSize = max 1 (rangeSize `div` 5) 
            let randomValues = map intToNatural $ take subsetSize $ nub $ randomRs (low, high) gen
            return $ Just $ SDR (sort randomValues) conRV


-- | given empty graph nodes, inject initial values, like packages
constructNodes :: EncoderConfig -> DecoderConfig -> MyNode -> IO MyNode
constructNodes conS conO n = do
    defP <- initConfigs -- maybe define each Region with seperate Configs
    r <- initRegion2 (getRange conS) (defP^.conR)
    return $
        case n of 
            InputField Nothing Nothing  -> InputField Nothing (Just conS) -- build an SDR place holder from encodingConfig
            Module Nothing Nothing  -> Module (Just r) (Just defP) -- init Module with default Package
            Output Nothing (Just p)  -> Output (Just r) (Just defP) -- init Output with default Package
            OutputValue Nothing Nothing -> OutputValue Nothing (Just conO) -- init the DecoderConfig
            _ -> n


--- | Define a graph with nodes labeled by Ints and edges labeled by Strings
type MyGraph = Gr MyNode MyEdge

--- | Create a graph
myGraph :: EncoderConfig -> DecoderConfig -> IO MyGraph
myGraph conS conO = do
    defP <- initConfigs 
    a1Nodess <- mapM (secondM (constructNodes conS conO)) a1Nodes
    return $ mkGraph a1Nodess a1Edges

---------------------------------------------------------
----                 Build the graph                -----
---------------------------------------------------------

initAgent :: MyGraph -> MyGraph -> MyGraph
initAgent sketch config = 
    -- define structure
    sketch
    -- init config Package with default Package
    --let a = initConfigs


buildGraph :: MyGraph -> IO MyGraph 
buildGraph graph =  do
    let nodes = labNodes graph
        edges = labEdges graph
    newNodes <- mapM constructSingleRegion nodes
    let graph2 = mkGraph newNodes edges
    contexts <- forM nodes $ \(n, _) -> do
        -- Step 2: Apply constructSingleRegion to each context
        maybeCtx <- context graph2 n
        case maybeCtx of
            Just ctx -> connectRegionToInput ctx
            Nothing -> error "Node not found in graph"

   -- gmap connectRegionToGate . gmap connectRegionToInput

    return $ buildGr contexts
    --return $ 
    

constructSingleRegion :: (Node, MyNode) -> IO (Node, MyNode)
constructSingleRegion (n, Module _ (Just p)) = do 
        r <- initRegion2 (p^.conR)
        return (n, Module (Just r) (Just p))
constructSingleRegion (Output (Just r) (Just p)) = do 
        r <- initRegion2 (p^.conR)
        return (n, Output (Just r) (Just p))
constructSingleRegion n = n
    

connectRegionToInput :: Context MyNode MyEdge -> IO Context MyNode MyEdge
connectRegionToInput Context (Module _ (Just p)) = do
    let thisNode = lab' cxt 
        ws = getMyNodes graph cxt Water -- currenly this is just one node. There aren't more than one Region to Region Water connections
        case thisNodes of 
            
    return cxt
   -- case thisNode of 


-- Build graph
-- Build Region
    -- Construct a Region, connect it to itsself (Proxmial) -- we can use nmap for this
        -- r <- initRegion2 p^.conR -- :: Region
    -- Connect the region to the input space -- we need gmap for this
        -- inputField val encoder
        -- cols <- connectToInputField (getRange encoder) (p^.conR) (r^.cols) --:: [Column]
    -- Connect the region to other regions (Distal) -- we need gmap for this
        -- r <- connectDistalRegions [[Column]] conR r --:: Region
        -- Inject package to Module -> then build region from the input Region, and Package
-- Init LocationField after the OutPutRegion it connected to is done. This should be done at a final phase! -- we need gmap for this
        -- Get LocationField
        -- get the inn node that is an output node
        -- get the SDRRange it creates
        -- create a random SDR within that range.
        -- LocationField (sdr )

-- | Given graph scafoling, initialize all nodes with default values
buildNodes :: MyNode -> MyNode
buildNodes n = case n of 
        Input (Just i) -> n -- Do nothing
        LocationField (Just sdr)  -> n -- Initilize with random SDR, maybe it is easier to initialize with EncoderConfig
            -- LocationField (initilize with start SDR)
        InputField (Just sdr) (Just encoding) -> n -- build an SDR place holder from encodingConfig
            -- InputField (Nothing) (Just encoding parameter)
        Module Nothing (Just p)  -> n -- buildRegion from Package 
            -- Module (Nothing) (default package modified by custom htm and region config)
        Output Nothing (Just p)  -> n -- buildRegion from Package
            -- Output (Nothing)
        Memory Nothing  -> n -- Do nothing
        OutputValue (Just o) (Just conf) -> n -- Do noting here, but define the outputConfig
        _ -> n

initConfigGraph :: Package -> MyGraph -> MyGraph
initConfigGraph p = gmap $ initRegionsWithDefault p

initRegionsWithDefault :: Package -> Context a b  -> Context a b -- Context c d 
initRegionsWithDefault p c = c   
   --- | c == (nodesFrom , Module Nothing, b , c ) = (nodesFrom, Module $ initRegion (p^. conS) (p^.conR), b ,c)  -- configure with default package
   --- | c == (nodesFrom , Module Nothing, b , c ) = (nodesFrom, Output $ initRegion (p^. conS) (p^.conR), b, c)
   --- | otherwise = c 


buildEdges :: Context MyNode MyEdge -> Context MyNode MyEdge
buildEdges c = c 
    -- connect Waters
        -- input - inputfield
        -- locationfield - inputfield
        -- inputfield - module
        -- inputfield - output
        -- output - outputvalue
    -- connect Gates
        -- module - module
        -- module - output
        -- memory - module
        -- memory - ouput
    --randomSDR <- getRandomSDR (defaultOutPutP ^.conS) -- TODO initilize SDRRange of previous Region
    --return $
        --case n of 
       --     LocationField Nothing  -> LocationField randomSDR -- Initilize with random SDR, maybe it is easier to initialize with EncoderConfig

--- | Inject default package into the Module and Output regions
myGraph2 :: Package -> MyGraph
myGraph2 defP = let a1Nodes2 = map (injectDefualtPackage defP) a1Nodes 
                        in mkGraph a1Nodes2 a1Edges


injectDefualtPackage :: Package -> (Int, MyNode) -> (Int, MyNode)
injectDefualtPackage p n = n
    --- | n == (i, Module r Nothing) = (i, Module r p)
    --- | n == (i, Output r Nothing) = (i, Output r p)
    --- | otherwise = n

{-
    condecne Region out to [InputModules] -> Package ^. SDRRange

    TODO: Connect the Regions with Gates Too, Currently only Water is connected to the Region
        First init  all Regions (Column and cells with proximal dendirtes)
        Then init all regions with distal dendrite, by passing in the incomming regions
--}
condenceRegionOutput :: MyGraph -> Context MyNode MyEdge  -> Context MyNode MyEdge
condenceRegionOutput g here = let mSdrRange = foldSDRange $ map myNodeToSDRRange $ getMyNodes g here Water
                                  (f, n, a, t) = here
                                in case a of 
                                    Module r (Just p) -> (f,  n, Module r $ Just $ p {_conS = mSdrRange}, t)
                                    Output r (Just p) -> (f, n, Output r $ Just $ p {_conS = mSdrRange}, t)
                                    _ -> here
                                


getMyNodes :: MyGraph -> Context MyNode MyEdge -> Signal -> [MyNode]
getMyNodes g here signal = let edgesInn = filter (isSignal signal) $ inn' here -- all water edges to the node
                in map (lab' . context g . nodeFrom) edgesInn -- all nodes with an edge to the node

nodeFrom :: LEdge MyEdge -> Node
nodeFrom (a,b,e) = a

isSignal :: Signal -> LEdge MyEdge -> Bool
isSignal signal (a,b,e) = e == MyEdge signal

myNodeToSDRRange :: MyNode -> Maybe SDRRange
myNodeToSDRRange n = case n of 
                        Module r p -> {-case p of 
                            Nothing -> Nothing
                            Just pack -> Just $ regionToInputSDRConfig $ pack^.conR-}
                            p >>= \pack -> regionToInputSDRConfig $ pack^.conR
                        InputField r _ -> case r of 
                                Nothing -> Nothing 
                                Just sdr -> Just (sdr^.sdrRange)

                        _ -> Nothing

regionToInputSDRConfig :: RegionConfig -> Maybe SDRRange
regionToInputSDRConfig r = Just $ SDRRange 0 (r^.nrOfCellsPerColumn * r^.nrOfColumns)


foldSDRange :: [Maybe SDRRange] -> Maybe SDRRange
foldSDRange [] = Nothing
foldSDRange (x:xs) = foldl combineSDRRange x xs

combineSDRRange :: Maybe SDRRange -> Maybe SDRRange -> Maybe SDRRange
combineSDRRange (Just x) (Just y)  = Just $ SDRRange {
                        _minIndex = min (x ^.minIndex) (y ^.minIndex), -- preserve 0
                        _maxIndex = (x ^.maxIndex) + (y ^.maxIndex) -- combine to get maximum
                        }
combineSDRRange _ _ = Nothing














---------------------------------------------------------
----                    Run graph                   -----
---------------------------------------------------------

newtype InputValue = Value Int
type SwitchGoal = Bool 
type Memorize = Bool 

data HyperParameters = HyperParameters SwitchGoal Memorize

runGraph :: HyperParameters -> InputValue -> IO MyGraph -> IO MyGraph
runGraph param v gIO = do 
                g <- gIO
            -- insert value -> InputNode
                let g2 = nmap (insertValue v) g
                    sortedNodes = topsort' g2
                updateNodes param sortedNodes g2
            -- Sort by Edges by order, and group them by sets (use topological sorting to)
                -- And update value 
                
            -- Run next set of nodes in (parallel, in the future)
                -- get all nodes at current order
                -- from each node get pre nodes
                -- update from each pre nodes

insertValue :: InputValue -> MyNode -> MyNode
insertValue (Value v) n = -- replace a input node with the SDR, OBS! assuming there is only one Input Node.
                case n of 
                    Input _ -> Input $ Just v
                    _ -> n

updateNodes :: HyperParameters -> [MyNode] -> MyGraph -> IO MyGraph
updateNodes param sortedNodes graph = do 
        foldlM update graph sortedNodes
    where
        sortedNodes = topsort graph
        update g node = do
            replaceNode node (processMyNode param) g-- apply change to the node in the graph


replaceNode :: Node -> (MyGraph -> Node -> IO (Maybe MyNode)) -> MyGraph -> IO MyGraph
replaceNode node f graph = do
    let graphWithoutNode = delNode node graph
    newNodeLabel <- f graph node  
    case newNodeLabel of 
        Nothing -> return graph
        Just newNode -> return $ insNode (node, newNode) graphWithoutNode

processMyNode :: HyperParameters -> MyGraph -> Node -> IO (Maybe MyNode)
processMyNode param graph node = do
    let cxt = context graph node
        thisNode = lab' cxt 
        ws = getMyNodes graph cxt Water -- currenly this is just one node. There aren't more than one Region to Region Water connections
        gs = getMyNodes graph cxt Gate -- There can however be many regions
    case thisNode of 
        Module (Just r) (Just p) -> do 
            -- get water SDR and gate regions
            let wss = mapM mapToSDR ws -- TODO convert edges to SDRs, and merge them -- handles taking input from locationField too.
                gss = mapM getRegion gs -- convergt eges to SDRs, don't merge them
                isJustwgs = isJust gss && isJust wss      
            processWaterGate p r wss gss
        Output (Just r) (Just p) -> do 
            -- get water SDR and gate regions
            let wss = mapM mapToSDR ws -- TODO convert edges to SDRs, and merge them -- handles taking input from locationField too.
                gss = mapM getRegion gs -- convergt eges to SDRs, don't merge them -- handles memory too.
                isJustwgs = isJust gss && isJust wss      
            processWaterGate p r wss gss
        InputField _ (Just conS) -> do
            -- getInputVal from previous edge
            let inputVal = head <$> mapM (fmap (encode conS) . getInputVal) ws
            case inputVal of 
                Just v -> return $ Just $ InputField v (Just conS)
                _ -> return Nothing
        LocationField _ -> do
            -- store the sdr of the previous region
            let outputVal = head <$> mapM regionToSDR2 ws
            case outputVal of 
                Just val -> return $ Just $ LocationField (Just val) 
                _ -> return Nothing
        Memory (Just mem) -> do
            -- either update the memory or not, this is something I should take in as a hyperparameter?
            -- TODO seems like the memory node should store the Region, rather than the SDR values. 
            -- The question is should the memory module be a Region itself? - We can prepend a region to it, if need be. Therefor this Memory should store a region to.
            let rM = head <$> mapM getRegion ws -- if prev node is memory, then you will be getting connect of that memory. Obs when you store this.
            case rM of 
                Just r -> do
                    let HyperParameters switchGoal memorize = param
                        mem2 = if switchGoal then mem LS.& currentGoal ?~ r else mem
                        mem3 = if memorize then mem LS.& allGoals %~ (++ [r]) else mem2
                    return $ Just $ Memory $ Just mem3
                _ -> return Nothing
        OutputValue (Just i) (Just conD) -> do 
            let outputVal = fromSDR conD . head <$> mapM regionToSDR2 ws
            case outputVal of 
                Just valM -> return $ Just $ OutputValue valM (Just conD)
                _ -> return Nothing
        _ -> return $ Just thisNode


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
fromSDR (DecoderConfig cat) s = let a = splitRangeIntoSubList cat (s^.sdrRange)
                        in  case a of 
                                Nothing -> Nothing
                               -- Just a -> indexOfMax $ map length $ splitAtPositions (trace ("[ TRACE ] --- sdr is: " ++ show (s^.sdr))(s^.sdr)) (trace ("[ TRACE ] --- split is " ++ show a) a)
                                Just a -> indexOfMax $ splitAtPositions (s^.sdr) a


regionToSDR2 :: MyNode -> Maybe SDR 
regionToSDR2 (Output (Just r) (Just p)) = Just $ regionToSDR (p^.conR) r
regionToSDR2 _ = Nothing



getInputVal :: MyNode -> Maybe Int 
getInputVal (Input v) = v
getInputVal _ = Nothing

processWaterGate :: Package -> Region2 ->  Maybe [SDR] -> Maybe [Region2] -> IO (Maybe MyNode)
processWaterGate p r (Just w) (Just os) = do 
                let p2 = value .~ head w $ p
                r2 <- HTM2.apply p2 r os
                return $ Just $ Module (Just r2) (Just p2)
processWaterGate _ _ _ _ = return Nothing
                
                
getRegion :: MyNode -> Maybe Region2 
getRegion (Module (Just r) (Just p)) = Just r
getRegion (Memory (Just mem)) = mem ^. currentGoal
getRegion _ = Nothing

mapToSDR :: MyNode -> Maybe SDR
mapToSDR (Module (Just r) (Just p)) = Just $ regionToSDR (p^.conR) r
mapToSDR (LocationField vM) = vM
mapToSDR (InputField vM conM) = vM
mapToSDR _ = Nothing

regionToSDR :: RegionConfig -> Region2 -> SDR
regionToSDR con r = SDR {
    _sdr = map (toBitIndex con) $ filter isActive $ concatMap (^.cells) (r^.cols),
    _sdrRange = SDRRange {
            _minIndex = 0,
            _maxIndex = (con^. nrOfCellsPerColumn) * (con^. nrOfColumns)
            }
        }

isActive :: Cell -> Bool
isActive c = (c^.cellState == ActiveCell) || (c^.cellState == ActivePredictiveCell) 

toBitIndex ::  RegionConfig -> Cell -> BitIndex
toBitIndex con c = c^. (cellId . col) * con^.nrOfCellsPerColumn + c^.( cellId . cell)
----
-- Post Processings
    -- Get Outputs, MemoryNodes, etc.

{----------------------------


            Init regions, inputfield, memory, given configs
            
            Compute context in the given orderes and produce the output value

            1. 
                - pass in value
                    update the input Value
                    update the memory module
                        store | continue | switch Int -- which memory?
                - sync the computation order
                    sort edges by order
                    for each step until all edges are visited
                        go through all edges that are supposed to be computed at this step
                        compute the edge
                            take in the inputs from the other edges it points to
                            compute and update the node
                - update the output value

            2. call the function
                - extract the output value!
            
            3. function calls
                - compute :: InputValue -> OutputValue
                - updateMemory :: Agent -> UpdateType -> Agent
                - updaet

            3. Checklist 
                - [x] update internal state
                - [x] make sure all nodes are visited in the given order
                - [ ] Control memory module?


 Define a new Package

data WaterInput = WIPackage (Maybe SDR) (Maybe EncodingConfig) -- value and encodingconfig useful for spatial pooler
data GateInfo = GIPackage (Maybe SDR) (Maybe SDRRange) -- value and sdrRange useful for temporal pooler
data WaterOutput = WOPackage (Maybe DecodingConfig) -- dont ne


data DataFlow = DataFlow ([Maybe WaterInput]) ([Maybe GateInfo]) (Maybe RegionConfig) (Maybe WaterOutput) -- handles a list of inputregions and predictionregions
data AlgoConfig = AlgoConfig HTMConfig (Maybe StdGen)

data Package = Package HTMConfig RegionConfig (Maybe SDRRange) SDR StdGen deriving (Show)

data NewPackage = NewPackage DataFlow AlgoConfig -- configures the dataflow and algorithm

-- OLD Algorithms

initRegion :: Maybe SDRRange -> RegionConfig -> IO Region

spatialPooler :: Package -> Region -> Region 
temporalPooler :: Package -> Region -> IO Region

-- NEW Algorithms

dataflow1 = DataFlow [(WIPackage (Just Nothing) (Just encoding))] [(GIPackage Just Nothing (Just sdrRange))] initConfig (WOPackage Nothing)
initRegion :: dataflow1 -> RegionConfig -> IO Region  

beforeValue = dataflow1 (Just seed)

dataflow2 = DataFlow [(WIPackage (Just SDRValue) (Just encoding))] [(GIPackage (Just SDRValue) (Just sdrRange))] initConfig (WOPackage Nothing)
whenValue = dataflow2 (Just seed)
spatialPooler :: NewPackage -> Region -> Region 
temporalPooler :: NewPackage -> Region -> IO Region


--- | Graph definition

makeGraph = mkGraph a1Nodes a1Edges -- empty nodes, with specified edges

--- | Graph build

initGraph = MyGraph -> MyGraph -- add package, encoderConfig and decoderConfig into the nodes. 
        -- The package also defines the SDRRange of the inputfield.
        -- It is missing SDR
                -- we inject it during the value
        -- It is missing StdGen -> When should it be included? 
                -- initRegion: to randomly select synapsese to the inputfields. -- but we don't need a random seed here. It uses IO random
                -- spatialPooler: We dont need it here.
                -- TempPooler: We need it here to grow ny synapses
-> Package HTMConfig RegionConfig (Maybe SDRRange) SDR StdGen deriving (Show) 




            1. See if I can create a general config for all regions:
                - use an generic initalizer for each region
                
            2. and ways of speciallizing the configs after the fact.
                - map specialization configs to the generic ones.

            3. Design agents with various region matching option-selections
                - given list of regions and other list of regions, match them various ways
                    1-1, 1-many, many-1, many-many 
            
            4. Design regions that with inihibition signals.


for each node
     if the edge to it is a gate, then initialize the the node to look at the gate region 

     if the edge to it is a water, then initalize the node to have the inputspace the same as the water region

     = (prev region, region, output SDRRange)

    filter all edges from Edge
    map region to output SDRrange -> initlized region from the nodeFrom with Water 
    zip SDRRange with region (region, SDRRange) 
    initilize region [(prev region_i, SDRRange)] -> 


    -- travese nodes and update them with corresponding nodeConfig
    -- map two graphs ontop of eachother
 

    append the package in Module and Output Regions with conS = SDRRange of the inputs space. 
    
    where SDRRange comes from combing the SDRRange of all input regions

    Should I have a new package module, that includes taking in input from several regions? 
    Sinse I am syncronizing the inputs to a single SDR, it shouldn't be a problem to just append the SDRs together.
    But then I might need to merge all Pre-Region outputs. 
    
    condenceRegionOutput :: Context a b -> [Adj a] -> [Module Region Package] -> [Maybe SRC.SDR.SDRRange] -> Maybe SRC.SDR.SDRRange
    

    
    setInputField :: Package -> SDRRange -> Package

    -- Value is set
    mergeRegionOutputs:: [Region] -> SDR
    

    and update the package with the new SDR Value

Init regions from package

Context a b -> Context a b
-}