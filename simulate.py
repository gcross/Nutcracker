#@+leo-ver=4-thin
#@+node:gcross.20091013163748.4180:@thin simulate.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20091013163748.4182:<< Import needed modules >>
from twisted.internet import reactor
from twisted.internet.defer import inlineCallbacks

from grid import *
from trial import *
#@-node:gcross.20091013163748.4182:<< Import needed modules >>
#@nl

@inlineCallbacks
def main():
    #@    << Main >>
    #@+node:gcross.20091013163748.4181:<< Main >>
    controller, furl = yield startController()
    worker = yield startWorker(furl)

    number_of_sites = 10
    physical_dimension = 2
    starting_bandwidth_dimension = 2
    perturbation_strength = 0.1

    from models import GadgetHamiltonianFactory
    operator_factory = GadgetHamiltonianFactory(number_of_sites,perturbation_strength)

    result1 = yield controller.add_task(
        TrialTask(
            number_of_sites,
            physical_dimension,
            starting_bandwidth_dimension,
            operator_factory,
            []
        )
    )

    result2 = yield controller.add_task(
        TrialTask(
            number_of_sites,
            physical_dimension,
            starting_bandwidth_dimension,
            operator_factory,
            [result1.state_fetcher]
        )
    )

    result3 = yield controller.add_task(
        TrialTask(
            number_of_sites,
            physical_dimension,
            starting_bandwidth_dimension,
            operator_factory,
            [result1.state_fetcher,result2.state_fetcher]
        )
    )

    reactor.stop()
    #@-node:gcross.20091013163748.4181:<< Main >>
    #@nl


reactor.callLater(0,main)
reactor.run()
#@-node:gcross.20091013163748.4180:@thin simulate.py
#@-leo
