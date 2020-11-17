# HTM-haskell
A Haskell implementation of the Hierarchical Temporal Memory algorithm by Numenta.

## Implementation
- [ ] spatial pooler 
    - [x] Compute overlap
    - [x] Inhition
    - [x] Learning
- [ ] temporal pooler [The inspirational paper](https://numenta.com/assets/pdf/temporal-memory-algorithm/Temporal-Memory-Algorithm-Details.pdf)
    - [ ] Add context
        - [ ] activatePredictedColumn
            - [ ] select winner cells
            - [ ] learn
        - [ ] burst Column
            - [ ] select winner cell 
                - [ ] best matching
                - [ ] least used
            - [ ] learn
        - [ ] punishPredictedColumn
    - [ ] Learning
        - [ ] grow new segments
        - [ ] maintain sparcity by growing synapses, and punishing synapses. 
    - [ ] Predict
        - [ ] activate predicted synpases

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