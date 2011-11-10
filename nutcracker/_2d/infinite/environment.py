#@+leo-ver=5-thin
#@+node:gcross.20111109104457.1702: * @file environment.py
#@+<< Imports >>
#@+node:gcross.20111109104457.1703: ** << Imports >>
from numpy import array

from .grids import OperatorGrid, StateGrid
from ..environment import NormalizationEnvironment, ExpectationEnvironment
from ..tensors import *
from ...utils import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111109104457.1704: ** Classes
#@+node:gcross.20111109104457.1745: *3* InfiniteNormalizationEnvironment
class InfiniteNormalizationEnvironment(NormalizationEnvironment):
    #@+others
    #@+node:gcross.20111109104457.1706: *4* __init__
    def __init__(self,physical_dimension=None,state_grid=None):
        if state_grid is None:
            if physical_dimension is None:
                raise ValueError("either the physical dimension or an initial state grid must be supplied")
            state_grid = StateGrid.simpleObservation(physical_dimension,0)
        self.center = state_grid.center
        self.sides = state_grid.sides
        self.corners = state_grid.corners
    #@+node:gcross.20111109104457.1723: *4* contract
    def contract(self,*directions):
        for direction in directions:
            if self.bandwidthDimension(direction) != self.bandwidthDimension(OPP(direction)):
                raise ValueError("The center tensor needs the dimensions of both the forward and reverse links along a direction to match in order to contract in that direction.  (direction = {}, forward bandwidth = {}, reverse bandwidth = {})".format(direction,self.bandwidthDimension(direction),self.bandwidthDimension(OPP(direction))))
            self.sides[direction] = self.sides[direction].absorbCenterSite(self.center,direction)
            self.corners[direction] = self.corners[direction].absorbSideSiteAtCounterClockwise(self.sides[CCW(direction)])
            self.corners[CW(direction)] = self.corners[CW(direction)].absorbSideSiteAtClockwise(self.sides[CW(direction)])
    #@+node:gcross.20111014172511.1240: *4* increaseAxialBandwidthDimensionsBy
    def increaseAxialBandwidthDimensionsBy(self,increment,direction):
        self.increaseSingleDirectionBandwidthDimensionBy(increment,direction)
        self.increaseSingleDirectionBandwidthDimensionBy(increment,OPP(direction))
    #@+node:gcross.20111014172511.1242: *4* increaseAxialBandwidthDimensionsTo
    def increaseAxialBandwidthDimensionsTo(self,dimension,direction):
        self.increaseSingleDirectionBandwidthDimensionTo(dimension,direction)
        self.increaseSingleDirectionBandwidthDimensionTo(dimension,OPP(direction))
    #@+node:gcross.20111013080525.3958: *4* increaseSingleDirectionBandwidthDimensionBy
    def increaseSingleDirectionBandwidthDimensionBy(self,increment,direction):
        self.increaseSingleDirectionBandwidthDimensionTo(self.bandwidthDimension(direction)+increment,direction)
    #@+node:gcross.20111013080525.1264: *4* increaseSingleDirectionBandwidthDimensionTo
    def increaseSingleDirectionBandwidthDimensionTo(self,new_dimension,direction):
        self.sides[direction], self.center = \
            increaseDimensionUsingFirstTensorOnlyBetweenTensors(
                self.sides[direction],StateSideSite.inward_index,
                self.center,1+direction,
                new_dimension
            )
    #@+node:gcross.20111109104457.1787: *4* trivial
    @classmethod
    def trivial(cls):
        return cls(1)
    #@-others
#@+node:gcross.20111109104457.1735: *3* InfiniteExpectationEnvironment
class InfiniteExpectationEnvironment(ExpectationEnvironment,InfiniteNormalizationEnvironment):
    #@+others
    #@+node:gcross.20111109104457.1736: *4* __init__
    def __init__(self,operator_grid,physical_dimension=None,state_grid=None):
        super(type(self),self).__init__(physical_dimension=physical_dimension,state_grid=state_grid)
        self.O_center = operator_grid.center
        self.O_sides = operator_grid.sides
        self.O_corners = operator_grid.corners
    #@+node:gcross.20111109104457.1743: *4* contract
    def contract(self,*directions):
        super(type(self),self).contract(*directions)
        for direction in directions:
            self.O_sides[direction] = self.O_sides[direction].absorbCenterSite(self.O_center,direction)
            self.O_corners[direction] = self.O_corners[direction].absorbSideSiteAtCounterClockwise(self.O_sides[CCW(direction)])
            self.O_corners[CW(direction)] = self.O_corners[CW(direction)].absorbSideSiteAtClockwise(self.O_sides[CW(direction)])
    #@+node:gcross.20111109104457.1788: *4* trivial
    @classmethod
    def trivial(cls):
        return cls(operator_grid=OperatorGrid.trivial(),state_grid=StateGrid.trivial())
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111109104457.1744: ** << Exports >>
__all__ = [
    "InfiniteNormalizationEnvironment",
    "InfiniteExpectationEnvironment",
]
#@-<< Exports >>
#@-leo
