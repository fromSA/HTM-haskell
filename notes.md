# TODO
- use haddock to generate documentation for the code
- upload the package to hackage 

# TODO Code
- The columns depend on the moving average when being initilized. This should something the HTM algorithm should deside.

- Is the inhibition radius necessary. It creates a spacial encoding. Think CovNet.

# Question: 
- How is the sense of time encoded into the htm?
- Is it possible to generate a region that is created deterministicaly, that also maintains the distribution? This would make the creation of a region a pure function.
- Can a cell be activated by mulitple segments?

# Notes:
- The region had to be an IO, because of the random number generator in haskell.
- OBS! Let vs where might cause a segnificant delay in processing time.
- Don't know which dendrite to select during buring when growing segments on the least used cell. I just select the first dendrite, assuming there is one. There should be one for all cells when the region initialises.