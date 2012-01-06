# Imports {{{
from numpy import array, complex128

from .chains import *
from ..enumerations import Direction
from ..environment import Environment
from ..tensors import *
# }}}

class InfiniteEnvironment(Environment): # {{{
    def __init__(self,operator_chain,observation=0,state_chain=None): # {{{
        if state_chain is None:
            state_chain = \
                StateChain.simpleObservation(
                    physical_dimension=operator_chain.physical_dimension,
                    observation=observation
                )
        super(type(self),self).__init__(
            LeftExpectationBoundary.formFromOuterProduct(
                operator = operator_chain.left_boundary_vector,
                state = state_chain.left_boundary_vector,
            ),
            RightExpectationBoundary.formFromOuterProduct(
                operator = operator_chain.right_boundary_vector,
                state = state_chain.right_boundary_vector,
            ),
            state_chain.site,
            operator_chain.site,
        )
# }}}

    def dangerouslyContractWithoutNormalizing(self,direction): # {{{
        if direction not in Direction.values():
            raise ValueError("contraction direction must be either Direction.left or Direction.right, not {}".format(direction))
        if direction == Direction.left:
            self.left_environment = self.left_environment.absorb(state_site=self.state_site,operator_site=self.operator_site)
        elif direction == Direction.right:
            self.right_environment = self.right_environment.absorb(state_site=self.state_site,operator_site=self.operator_site)
# }}}

    def normalizeAndContract(self,direction): # {{{
        if direction not in Direction.values():
            raise ValueError("contraction direction must be either Direction.left or Direction.right, not {}".format(direction))
        state_site_to_contract, self.state_site = self.state_site.normalizeAndDenormalize(direction,self.state_site)
        if direction == Direction.left:
            self.left_environment = self.left_environment.absorb(state_site=state_site_to_contract,operator_site=self.operator_site)
        elif direction == Direction.right:
            self.right_environment = self.right_environment.absorb(state_site=state_site_to_contract,operator_site=self.operator_site)
# }}}

    @classmethod
    def random( # {{{
        cls,
        physical_dimension,
        state_dimension,
        operator_dimension,
    ):
        self = cls.trivial()
        self.left_environment = \
            LeftExpectationBoundary.random(
                state_dimension = state_dimension,
                operator_dimension = operator_dimension,
            )
        self.right_environment = \
            RightExpectationBoundary.random(
                state_dimension = state_dimension,
                operator_dimension = operator_dimension,
            )
        self.state_site = \
            StateSite.random(
                left_dimension = state_dimension,
                right_dimension = state_dimension,
                physical_dimension = physical_dimension,
            )
        self.operator_site = \
            OperatorSite.random(
                left_dimension = operator_dimension,
                right_dimension = operator_dimension,
                physical_dimension = physical_dimension,
            )
        return self
# }}}

    @classmethod
    def trivial(cls): # {{{
        return cls(OperatorChain.trivial())
    # }}}
# }}}

# Exports {{{
__all__ = [
    "InfiniteEnvironment",
]
# }}}
