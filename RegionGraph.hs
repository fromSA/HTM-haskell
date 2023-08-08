module RegionGraph where
import SRC.Region.Model (Region(Region))
import GHC.Natural (Natural)
import SRC.SDR
import Data.Graph.Inductive
import SRC.Package
import SRC.Region.Config
import SRC.Encoder.Config
import Control.Lens ((^.))
import Data.Graph.Inductive.Query (topsort)

-- | Build a graph of the agents mind

--- | The nodes

data MemoryContent = MemoryContent {currentGoal :: Maybe SDR , allGoals :: [SDR]} deriving (Show) 

data MyNode = Input (Maybe Int)  -- This might need to be a list of Input Value in the future
    | LocationField (Maybe SDR) -- inputfield that is initilized randomly and updates itsself 
    | InputField (Maybe SDR) (Maybe EncoderConfig) -- input that takes values from the outside
    | Module (Maybe Region) (Maybe Package)
    | Output (Maybe Region) (Maybe Package)
    | Memory (Maybe MemoryContent)
    | OutputValue (Maybe Int) (Maybe EncoderConfig) deriving (Show) -- TODO change to output config

--- | Edge
    
data Signal = Water | Gate deriving (Show, Eq) -- water = input singal, Gate = predictive signal
data MyEdge = MyEdge {order ::  Natural, kind :: Signal} deriving (Show)


-- | Define the agents mind

data NodeS = NodeScetch Int MyNode -- a nodeid and a node
data EdgeS = EdgeScetch Int Int MyEdge -- from a node to another node with a Edge definition

extractNS :: NodeS -> (Int, MyNode)
extractNS (NodeScetch a b) = (a, b)
extractES :: EdgeS -> (Int, Int, MyEdge)
extractES (EdgeScetch a b c) = (a, b, c)


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
        NodeScetch 11 $ LocationField Nothing, -- input to loc, updated by loc_new after a run, initilized with a random SDR
        NodeScetch 12 $ Memory Nothing, -- Nothing -> goal
        NodeScetch 13 $ OutputValue Nothing Nothing -- act -> Action_Value :: Int
        ] 

a1Edges = map extractES [
        EdgeScetch 0 1 $ MyEdge 0 Water, -- Pass value to obs
        
        -- | This might be uncessary duplicate!!!
        -- | It is also a problem if we are gonna user topological sorting to process values!
        --EdgeScetch 11 4 $ MyEdge 0 Water, -- Pass loc_old -> loc 
        
        EdgeScetch 4 2 $ MyEdge 1 Gate, -- Pass loc -> [obs, loc] x
        EdgeScetch 1 2 $ MyEdge 1 Water, -- Pass obs -> [obs, loc] x
        
        EdgeScetch 4 5 $ MyEdge 2 Water, -- Pass loc -> [loc, obj] x
        EdgeScetch 2 5 $ MyEdge 2 Water, -- Pass obj -> [loc, obj] x
        
        EdgeScetch 2 3 $ MyEdge 2 Water, -- Pass obj -> obj x
        
        EdgeScetch 5 6 $ MyEdge 3 Water, -- Pass loc2 -> [loc2, [act_opt]] x
        EdgeScetch 3 6 $ MyEdge 3 Gate, -- Pass [act_opt] -> [loc2, [act_opt]] x
        
        EdgeScetch 12 7 $ MyEdge 4 Gate, -- Pass goal -> [[loc3], goal]
        EdgeScetch 6 7 $ MyEdge 4 Water, -- Pass [loc3] -> [[loc3], goal]  x
        
        -- | Not sure if this will work.
        EdgeScetch 3 8 $ MyEdge 5 Water, -- Pass [act_opt] -> [[act_opt], [loc_match]] x
        EdgeScetch 7 8 $ MyEdge 5 Gate, -- Pass [loc_match] -> [[act_opt], [loc_match]] x 
        
        EdgeScetch 8 9 $ MyEdge 6 Water, -- Pass [act_match] -> act x
       
        EdgeScetch 8 10 $ MyEdge 6 Water, -- Pass [act_match] -> [[act_match], [loc_match]] x
        EdgeScetch 7 10 $ MyEdge 6 Water, -- Pass [loc_match] -> [[act_match], [loc_match]] x
        
        EdgeScetch 9 13 $ MyEdge 7 Water, -- Pass act -> ActionValue x
        EdgeScetch 10 11 $ MyEdge 7 Water -- Pass loc_new -> loc_old : Replace loc_old with loc_new  x
        ]




--- make graph


-- Define a graph with nodes labeled by Ints and edges labeled by Strings
type MyGraph = Gr MyNode MyEdge

-- Create a graph
myGraph :: MyGraph
myGraph = mkGraph a1Nodes a1Edges

-- print graph
main = do 
    --let g = myGraph
    ---let edgesInn = filter isWaterEdge $ inn g 6 -- all edges into a node
    --let nodesFrom = filter isModuleOrOutput $ map (\(a,_,_) -> lab' $ context g a) edgesInn

    --print nodesFrom
    --print $ context myGraph a
    prettyPrint myGraph




{----------------------------


            1. See if I can create a general config for all regions:
                - use an generic initalizer for each region
                
            2. and ways of speciallizing the configs after the fact.
                - map specialization configs to the generic ones.

            3. Design agents with various region matching option-selections
                - given list of regions and other list of regions, match them various ways
                    1-1, 1-many, many-1, many-many 
            
            4. Design regions that with inihibition signals.

-------------------------------}

initConfigGraph :: Package -> MyGraph -> MyGraph
initConfigGraph p = gmap $ initRegionsWithDefault p

initRegionsWithDefault :: Package -> Context a b  -> Context a b -- Context c d 
initRegionsWithDefault p c = c   
   -- | c == (nodesFrom , Module Nothing, b , c ) = (nodesFrom, Module $ initRegion (p^. conS) (p^.conR), b ,c)  -- configure with default package
   -- | c == (nodesFrom , Module Nothing, b , c ) = (nodesFrom, Output $ initRegion (p^. conS) (p^.conR), b, c)
   -- | otherwise = c 


{- 
for each node
     if the edge to it is a gate, then initialize the the node to look at the gate region 

     if the edge to it is a water, then initalize the node to have the inputspace the same as the water region

     = (prev region, region, output SDRRange)

    filter all edges from Edge
    map region to output SDRrange -> initlized region from the nodeFrom with Water 
    zip SDRRange with region (region, SDRRange) 
    initilize region [(prev region_i, SDRRange)] -> 
--}


-- travese nodes and update them with corresponding nodeConfig
-- map two graphs ontop of eachother
-- 


{--
    Inject default package into the Module and Output regions
--}
myGraph2 :: Package -> MyGraph
myGraph2 defP = let a1Nodes2 = map (injectDefualtPackage defP) a1Nodes 
                        in mkGraph a1Nodes2 a1Edges


injectDefualtPackage :: Package -> (Int, MyNode) -> (Int, MyNode)
injectDefualtPackage p n = n
    -- | n == (i, Module r Nothing) = (i, Module r p)
    -- | n == (i, Output r Nothing) = (i, Output r p)
    -- | otherwise = n




{-
    condecne Region out to [InputModules] -> Package ^. SDRRange

    TODO: Connect the Regions with Gates Too
--}
condenceRegionOutput :: MyGraph -> Context MyNode MyEdge  -> Context MyNode MyEdge
condenceRegionOutput g here = let mSdrRange = foldSDRange $ map myNodeToSDRRange $ getRegions g here
                                  (f, n, a, t) = here
                                in case a of 
                                    Module r (Just p) -> (f,  n, Module r $ Just $ p {_conS = mSdrRange}, t)
                                    Output r (Just p) -> (f, n, Output r $ Just $ p {_conS = mSdrRange}, t)
                                    _ -> here
                                

getRegions :: MyGraph -> Context MyNode MyEdge -> [MyNode]
getRegions g here = let edgesInn = filter isWater $ inn' here -- all water edges to the node
                in map (lab' . context g . nodeFrom) edgesInn -- all nodes with an edge to the node
    --let xs = pre myGraph 4  -- the previus elements
    --in map (context myGraph) xs -- get the context of all previus elements?

nodeFrom :: LEdge MyEdge -> Node
nodeFrom (a,b,e) = a

isWater :: LEdge MyEdge -> Bool
isWater (a,b,e) = kind e == Water

myNodeToSDRRange :: MyNode -> Maybe SDRRange
myNodeToSDRRange n = case n of 
                        Module r p -> {-case p of 
                            Nothing -> Nothing
                            Just pack -> Just $ regionToInputSDRConfig $ pack^.conR-}
                            p >>= \pack -> regionToInputSDRConfig $ pack^.conR
                        InputField r _ -> case r of 
                                Nothing -> Nothing 
                                Just sdr -> Just (sdr^.sdrRange)

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

{-
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
-}

{-

Init regions from package

Context a b -> Context a b
-}


buildGraph :: MyGraph -> MyGraph 
buildGraph = nmap buildNodes

buildNodes :: MyNode -> MyNode
buildNodes n = case n of 
        Input (Just i) -> n -- Do nothing
        LocationField (Just sdr)  -> n -- Initilize with random SDR, maybe it is easier to initialize with EncoderConfig
        InputField (Just sdr) (Just encoding) -> n -- build an SDR place holder from encodingConfig
        Module (Just r) (Just p)  -> n -- buildRegion from Package
        Output (Just r) (Just p)  -> n -- buildRegion from Package
        Memory Nothing  -> n -- Do nothing
        OutputValue (Just o) (Just conf) -> n -- Do noting here, but define the outputConfig
        _ -> n


newtype InputValue = Value Int

runGraph :: InputValue -> MyGraph -> MyGraph
runGraph v g = 
            -- insert value -> InputNode
                let g2 = nmap (insertValue v) g
                    sortedNodes = topsort' g2
                    in updateNodes sortedNodes g2
            -- Sort by Edges by order, and group them by sets (use topological sorting to)
                -- And update value 
                
            -- Run next set of nodes in (parallel, in the future)
                -- get all nodes at current order
                -- from each node get pre nodes
                -- update from each pre nodes

insertValue :: InputValue -> MyNode -> MyNode
insertValue (Value v) n = -- replace a inputfield node with the SDR, OBS! assuming there is only one Input Node
                case n of 
                    Input _ -> Input $ Just v
                    _ -> n

updateNodes :: [MyNode] -> MyGraph -> MyGraph
updateNodes sortedNodes graph = foldl update graph sortedNodes
    where
        sortedNodes = topsort graph
        update g node = replaceNode node processMyNode g-- apply change to the node in the graph


replaceNode :: Node -> (MyGraph -> Node -> MyNode) -> MyGraph -> MyGraph
replaceNode node f graph = 
    let graphWithoutNode = delNode node graph
        newNodeLabel = f graph node  
        newGraph = insNode (node, newNodeLabel) graphWithoutNode
    in  newGraph




processMyNode :: MyGraph -> Node -> MyNode
processMyNode graph node = lab' $ context graph node  -- Process Node here with HTM
        -- get nodes in
        -- split the nodes by their singal type (Water, Gate)
        -- Get the SDRS from the nodes
        -- process the HTM with this input and signal type, in the their correct parts

----
-- Post Processings
    -- Get Outputs, MemoryNodes, etc.


initAgent :: MyGraph -> MyGraph -> MyGraph
initAgent sketch config = 
    -- define structure
    sketch
    -- init config Package with default Package
    --let a = initConfigs




{----------------------------


            Init regions, inputfield, memory, given configs

-------------------------------}




{----------------------------


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


-------------------------------}