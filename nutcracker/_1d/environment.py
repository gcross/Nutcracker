# Imports {{{
from .. import core
from .tensors import LeftExpectationBoundary, RightExpectationBoundary, StateSite, OperatorSite
# }}}

class Environment(object):
    def __init__(self,left_environment,right_environment,state_site,operator_site): # {{{
        self.left_environment = left_environment
        self.right_environment = right_environment
        self.state_site = state_site
        self.operator_site = operator_site
    # }}}

    def computeExpectation(self): # {{{
        return core.compute_expectation(
            self.left_environment.data.transpose(),
            self.state_site.data.transpose(),
            self.operator_site.index_table.transpose(),
            self.operator_site.matrix_table.transpose(),
            self.right_environment.data.transpose()
        )
    # }}}

    def computeOptimizationMatrix(self): # {{{
        return core.compute_optimization_matrix(
            self.left_environment.data.transpose(),
            self.operator_site.index_table.transpose(),
            self.operator_site.matrix_table.transpose(),
            self.right_environment.data.transpose()
        ).transpose().reshape((self.state_site.size(),)*2)
    # }}}

    @classmethod
    def random( # {{{
        cls,
        physical_dimension,
        left_state_dimension,
        right_state_dimension,
        left_operator_dimension,
        right_operator_dimension
    ):  return cls(
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
    # }}}


# Exports {{{
__all__ = [
    "Environment",
]
# }}}
