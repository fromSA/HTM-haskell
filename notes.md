# TODO
- Document the code.
- use haddock to generate documentation for the code
- upload the package to hackage 
- Test using QuickCheck.
- Describe the process, the good and the bad
# TODO Code
- The columns depend on the moving average when being initilized. This should something the HTM algorithm should deside.

-- [ ] Allow saving and loading the model.
-- [ ] TODO fix this, it is ugly! getting random cell in HTM.Region.Utils
-- [ ] TODO double check that no duplicates occure in randIndecies in Region.Utils
- [ ] Convert explisit numbers in module to a constant value

- [ ] combine newSynapse in HTM.hs with initSynapse in Region.hs
- [ ] converte MovingAverage to a bit, use Data.Bits
- [ ] update inhibition radius
- [ ] doublecheck boosting update.
- [ ]initConnecitonStrength should be a distribution around a center
- [ ] break ties randomly when choosing the least used cell or best matching.
- [ ] Clean up and improve Code
    - [ ] avoid code duplicate
- [ ] perhaps use a rotational representation of the encoder? When getting neighbours.
- [ ] Make the temporalpooler pure.
    - [ ] The Random Generator, see if it can be pas
- [ ] Create a lighter Region. We dont want to pass large data around. Also, I want to parallelise the algorithm.
    - [ ] add a step value in cell ID (Why?)
    - [ ] See if there is an abstraction that does not need a duplicaiton of the a whole region for each step
        - [ ] The dendrites are the same for currentStep and previous step.

- [ ] Change _connectionStrength and _conStr to PermenanceValue. Maybe not.
- [ ] How does removing dead synapses affect the htm?


-- TODO set initConnectionStrength defined by a kernel function.

-- TODO double check that no duplicates occure
-- FIXME this is a list of the synapses
-- todo need a random seed
-- TODO set initConnectionStrength defined by a kernel function.

# Problems
- The last cell is always the winnerCell. Not what we want.


--------------------------------------------------------------------------------
# Question: 
- How is the sense of time encoded into the htm?
- Is it possible to generate a region that is created deterministicaly, that also maintains the distribution? This would make the creation of a region a pure function.
- Can a cell be activated by mulitple segments? - YES
- punishing synapses: Eventually as we grow synpases, cell might end up connecting to all othercells. Should we prune dead synapses, and when should we prune? I.e. Should there be a fixed size of "potential synapses" for all segments at all times?
- Is the inhibition radius necessary? It creates a spatsial encoding. Think CovNet.

# Notes:
- The region had to be an IO, because of the random number generator in haskell.
- OBS! Let vs where might cause a segnificant delay in processing time.
- Don't know which dendrite to select during buring when growing segments on the least used cell. I just select the first dendrite, assuming there is one. There should be one for all cells when the region initialises.



