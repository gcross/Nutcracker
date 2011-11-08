#@+leo-ver=5-thin
#@+node:gcross.20111107131531.3606: * @file environment.py
#@+<< License >>
#@+node:gcross.20111107131531.3607: ** << License >>
#@-<< License >>

#@+<< Imports >>
#@+node:gcross.20111107131531.3608: ** << Imports >>
from nutcracker.tensors import LeftExpectationBoundary, RightExpectationBoundary, StateSite, OperatorSite
import nutcracker.core as core
#@-<< Imports >>

#@+others
#@+node:gcross.20111107131531.3610: ** Classes
#@+node:gcross.20111107131531.3611: *3* Environment
class Environment(object):
    #@+others
    #@+node:gcross.20111107131531.3612: *4* __init__
    def __init__(self,left_environment,right_environment,state_site,operator_site):
        self.left_environment = left_environment
        self.right_environment = right_environment
        self.state_site = state_site
        self.operator_site = operator_site
    #@+node:gcross.20111107131531.3613: *4* computeExpectation
    def computeExpectation(self):
        return core.compute_expectation(
            self.left_environment.data.transpose(),
            self.state_site.data.transpose(),
            self.operator_site.index_table.transpose(),
            self.operator_site.matrix_table.transpose(),
            self.right_environment.data.transpose()
        )
    #@+node:gcross.20111107131531.3625: *4* random
    @staticmethod
    def random(
        physical_dimension,
        left_state_dimension,
        right_state_dimension,
        left_operator_dimension,
        right_operator_dimension
    ):  return Environment(
            left_environment =
                LeftExpectationBoundary.random(
                    state_dimension = left_state_dimension,
                    operator_dimension = left_operator_dimension,
                ),
            right_environment =
                RightExpectationBoundary.random(
                    state_dimension = right_state_dimension,
                    operator_dimension = right_operator_dimension,
                ),
            state_site =
                StateSite.random(
                    left_dimension = left_state_dimension,
                    right_dimension = right_state_dimension,
                    physical_dimension = physical_dimension,
                ),
            operator_site =
                OperatorSite.random(
                    left_dimension = left_operator_dimension,
                    right_dimension = right_operator_dimension,
                    physical_dimension = physical_dimension,
                )
        )
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111107131531.3609: ** << Exports >>
__all__ = [
    "Environment",
]
#@-<< Exports >>
#@-leo
