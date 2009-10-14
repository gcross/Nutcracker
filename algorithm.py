#@+leo-ver=4-thin
#@+node:gcross.20091014124839.1475:@thin algorithm.py
#@@language Python

#@<< import needed modules >>
#@+node:gcross.20091014124839.1492:<< import needed modules >>
from twisted.internet.defer import DeferredList, inlineCallbacks, returnValue

from trial import TrialTask
#@-node:gcross.20091014124839.1492:<< import needed modules >>
#@nl

#@+others
#@+node:gcross.20091014124839.1476:Functions
#@+node:gcross.20091014124839.1477:compute_level
@inlineCallbacks
def compute_level(controller,simulation_parameters,old_state_fetchers=[],minimum_occurances=3,batch_size=4):
    minimum_level = MinimumLevel()
    while minimum_level.occurances < minimum_occurances:
        yield DeferredList([controller.add_task(TrialTask(simulation_parameters,old_state_fetchers)).addCallback(minimum_level.update) for _ in xrange(4)])
    returnValue(minimum_level.result)
#@-node:gcross.20091014124839.1477:compute_level
#@-node:gcross.20091014124839.1476:Functions
#@+node:gcross.20091014124839.1487:Classes
#@+node:gcross.20091014124839.1488:MinimumLevel
class MinimumLevel(object):
    #@    @+others
    #@+node:gcross.20091014124839.1489:__slots__
    __slots__ = [
        "energy",
        "result",
        "occurances",
    ]
    #@-node:gcross.20091014124839.1489:__slots__
    #@+node:gcross.20091014124839.1490:__init__
    def __init__(self):
        self.energy = 1e100
        self.occurances = 0
    #@-node:gcross.20091014124839.1490:__init__
    #@+node:gcross.20091014124839.1491:update
    def update(self,result):
        if abs(self.energy-result.energy) < 1e-7:
            self.occurances += 1
        elif self.energy-result.energy > 1e-7:
            self.energy = result.energy
            self.result = result
            self.occurances = 1
        return result
    #@-node:gcross.20091014124839.1491:update
    #@-others
#@-node:gcross.20091014124839.1488:MinimumLevel
#@-node:gcross.20091014124839.1487:Classes
#@-others

__all__ = ["compute_level"]
#@-node:gcross.20091014124839.1475:@thin algorithm.py
#@-leo
