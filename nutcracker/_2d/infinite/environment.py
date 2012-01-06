# Imports {{{
from numpy import array

from .grids import OperatorGrid, StateGrid
from ..environment import NormalizationEnvironment, ExpectationEnvironment
from ..tensors import *
from ...utils import *
# }}}

class InfiniteNormalizationEnvironment(NormalizationEnvironment): # {{{
    def __init__(self,physical_dimension=None,state_grid=None,observation=0): # {{{
        if state_grid is None:
            if physical_dimension is None:
                raise ValueError("either the physical dimension or an initial state grid must be supplied")
            state_grid = \
                StateGrid.simpleObservation(
                    physical_dimension=physical_dimension,
                    observation=observation
                )
        self.center = state_grid.center
        self.sides = [StateSideSite.simpleInward(vector) for vector in state_grid.boundary_vectors]
        self.corners = [StateCornerSite.trivial()]*4
    # }}}

    def contract(self,*directions): # {{{
        for direction in directions:
            if self.bandwidthDimension(direction) != self.bandwidthDimension(OPP(direction)):
                raise ValueError("The center tensor needs the dimensions of both the forward and reverse links along a direction to match in order to contract in that direction.  (direction = {}, forward bandwidth = {}, reverse bandwidth = {})".format(direction,self.bandwidthDimension(direction),self.bandwidthDimension(OPP(direction))))
            self.sides[direction] = self.sides[direction].absorbCenterSite(self.center,direction)
            self.corners[direction] = self.corners[direction].absorbSideSiteAtCounterClockwise(self.sides[CCW(direction)])
            self.corners[CW(direction)] = self.corners[CW(direction)].absorbSideSiteAtClockwise(self.sides[CW(direction)])
    # }}}

    def increaseAxialBandwidthDimensionsBy(self,increment,direction): # {{{
        self.increaseSingleDirectionBandwidthDimensionBy(increment,direction)
        self.increaseSingleDirectionBandwidthDimensionBy(increment,OPP(direction))
    # }}}

    def increaseAxialBandwidthDimensionsTo(self,dimension,direction): # {{{
        self.increaseSingleDirectionBandwidthDimensionTo(dimension,direction)
        self.increaseSingleDirectionBandwidthDimensionTo(dimension,OPP(direction))
    # }}}

    def increaseSingleDirectionBandwidthDimensionBy(self,increment,direction): # {{{
        self.increaseSingleDirectionBandwidthDimensionTo(self.bandwidthDimension(direction)+increment,direction)
    # }}}

    def increaseSingleDirectionBandwidthDimensionTo(self,new_dimension,direction): # {{{
        self.sides[direction], self.center = \
            increaseDimensionUsingFirstTensorOnlyBetweenTensors(
                self.sides[direction],StateSideSite.inward_index,
                self.center,1+direction,
                new_dimension
            )
    # }}}

    @classmethod
    def trivial(cls): # {{{
        return cls(1)
    # }}}
# }}}

class InfiniteExpectationEnvironment(ExpectationEnvironment,InfiniteNormalizationEnvironment): # {{{
    def __init__(self,operator_grid,physical_dimension=None,state_grid=None): # {{{
        super(type(self),self).__init__(physical_dimension=physical_dimension,state_grid=state_grid)
        if self.physical_dimension != operator_grid.physical_dimension:
            raise ValueError("state physical dimension ({}) does not match operator physical dimensions ({})".format(self.physical_dimension,operator_grid.physical_dimension))
        self.O_center = operator_grid.center
        self.O_sides = [OperatorSideSite.simpleInward(vector) for vector in operator_grid.boundary_vectors]
        self.O_corners = [OperatorCornerSite.trivial()]*4
    # }}}

    def contract(self,*directions): # {{{
        super(type(self),self).contract(*directions)
        for direction in directions:
            self.O_sides[direction] = self.O_sides[direction].absorbCenterSite(self.O_center,direction)
            self.O_corners[direction] = self.O_corners[direction].absorbSideSiteAtCounterClockwise(self.O_sides[CCW(direction)])
            self.O_corners[CW(direction)] = self.O_corners[CW(direction)].absorbSideSiteAtClockwise(self.O_sides[CW(direction)])
    # }}}

    @classmethod
    def trivial(cls): # {{{
        return cls(operator_grid=OperatorGrid.trivial(),state_grid=StateGrid.trivial())
    # }}}
# }}}

# Exports {{{
__all__ = [
    "InfiniteNormalizationEnvironment",
    "InfiniteExpectationEnvironment",
]
# }}}
