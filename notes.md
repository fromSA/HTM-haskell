# TODO
- Document the code.
- use haddock to generate documentation for the code
- upload the package to hackage 
- Test using QuickCheck.
- Describe the process, the good and the bad

# Problems
- The last cell is always the winnerCell. Not what we want.

--------------------------------------------------------------------------------
# Question: 
- How is the sense of time encoded into the htm?
- Is it possible to generate a region that is created deterministicaly, that also maintains the distribution? This would make the creation of a region a pure function.
- Can a cell be activated by mulitple segments? - YES
- punishing synapses: Eventually as we grow synpases, cell might end up connecting to all othercells. Should we prune dead synapses, and when should we prune? I.e. Should there be a fixed size of "potential synapses" for all segments at all times?
- Is the inhibition radius necessary? It creates a spatsial encoding. Think CovNet.
- How does removing dead synapses affect the htm?

# Notes:
- The region had to be an IO, because of the random number generator in haskell.
- OBS! Let vs where might cause a segnificant delay in processing time.
- Don't know which dendrite to select during buring when growing segments on the least used cell. I just select the first dendrite, assuming there is one. There should be one for all cells when the region initialises.


# Notes:
1. Find a fast parallel way of selecting winnercells
    - collect winners and pass it to the segments? O(n)
        - winnercells = [cellID] 
        -> map to functions that take a Destinations and returns synapses 
        -> Exclude the synapses that do not contain the source
        -> sample random x subset 
        -> apply the functions and collect them to segments.
    - choose the closest columns? O(k)
    - 

2. subsample a list
- Generator (update) -> generate cmd -> (update) -> 