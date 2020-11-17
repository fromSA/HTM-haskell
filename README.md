# HTM-haskell
A Haskell implementation of the Hierarchical Temporal Memory algorithm by Numenta.

## Implementation
- [x] spatial pooler [The inspirational paper](https://numenta.com/resources/biological-and-machine-intelligence/spatial-pooling-algorithm/), [and also for the boosting](https://arxiv.org/pdf/1601.06116.pdf)
    - [x] Compute overlap
    - [x] Inhition
    - [x] Learning
- [x] temporal pooler [The inspirational paper](https://numenta.com/assets/pdf/temporal-memory-algorithm/Temporal-Memory-Algorithm-Details.pdf)
    - [x] Add context
        - [x] activatePredictedColumn
            - [x] select winner cells
            - [x] learn
        - [x] burst Column
            - [x] select winner cell 
                - [x] best matching
                - [x] least used
            - [x] learn
        - [x] punishPredictedColumn
    - [x] maintain sparcity 
        - [x] grow new segments
        - [x] add new synapses
        - [x] remove dead synapses
        - [x] punishing synapses
    - [x] Predict
        - [x] activate predicted synpases

## Functions Tested
- [ ] updateOverlap
    - [ ] Compute overlap
    - [ ] isColumnActive
- [ ] updateInhibition
- [ ] learn
- 

# Documented Functions and Records
- [x] HTMConfig
- [ ] Package 

# Future TODOS
- [ ] Encoders
    - [ ] create an Encoder Module
    - [ ] add other types of Encoders.
        {- Numeric Range
        | NumbericLog -- These are types of encodings.
        | Delta
        | Category Cyclic Order
        | Geospatial Range Speed
        | Text

        data Range = Bounded | UnBounded
        data InputValue = Number | Vector
        data Number = Continues Range | Discrete Range
        -}
-[ ]  