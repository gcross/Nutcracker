#@+leo-ver=4-thin
#@+node:gcross.20091013163748.4180:@thin simulate.py
#@@language Python

import warnings
warnings.simplefilter('ignore', DeprecationWarning)

#@<< Import needed modules >>
#@+node:gcross.20091013163748.4182:<< Import needed modules >>
from twisted.internet import reactor
from twisted.internet.defer import inlineCallbacks

from controller import *
from trial import *
from models import *
#@nonl
#@-node:gcross.20091013163748.4182:<< Import needed modules >>
#@nl

@inlineCallbacks
def main(controller,furl):
    #@    << Main >>
    #@+node:gcross.20091013163748.4181:<< Main >>
    print "FURL:",furl

    number_of_sites = 10
    physical_dimension = 2
    starting_bandwidth_dimension = 2
    perturbation_strength = 0.1

    simulation_parameters = \
        SimulationParameters(
            number_of_sites,
            physical_dimension,
            starting_bandwidth_dimension,
            GadgetHamiltonianFactory(number_of_sites,perturbation_strength)
        )

    spawnWorkerProcess()
    spawnWorkerProcess()
    spawnWorkerProcess()
    spawnWorkerProcess()

    result1 = yield controller.add_task(TrialTask(simulation_parameters))
    result2 = yield controller.add_task(TrialTask(simulation_parameters,[result1.state_fetcher]))
    result3 = yield controller.add_task(TrialTask(simulation_parameters,[result1.state_fetcher,result2.state_fetcher]))

    reactor.stop()
    #@-node:gcross.20091013163748.4181:<< Main >>
    #@nl

becomeControllerAndThenCall(main)
#@-node:gcross.20091013163748.4180:@thin simulate.py
#@-leo
