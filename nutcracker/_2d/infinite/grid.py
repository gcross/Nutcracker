#@+leo-ver=5-thin
#@+node:gcross.20111109104457.1702: * @file grid.py
#@+<< Imports >>
#@+node:gcross.20111109104457.1703: ** << Imports >>
from numpy import array

from ..environment import NormalizationEnvironment, ExpectationEnvironment
from ..tensors import *
from ...utils import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111109104457.1704: ** Classes
#@+node:gcross.20111109104457.1745: *3* NormalizationGrid
class NormalizationGrid(NormalizationEnvironment):
    #@+others
    #@+node:gcross.20111109104457.1706: *4* __init__
    def __init__(self,physical_dimension=None):
        if not physical_dimension:
            return
        self.sides = [StateSideSite.trivial()]*4
        self.corners = [StateCornerSite.trivial()]*4
        self.center = \
            StateCenterSite(
                physical_dimension = physical_dimension,
                rightward_dimension = 1,
                upward_dimension = 1,
                leftward_dimension = 1,
                downward_dimension = 1,
            )
        self.center.data[:,0,0,0,0] = array([1] + [0]*(physical_dimension-1))
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
    #@-others
#@+node:gcross.20111109104457.1735: *3* ExpectationGrid
class ExpectationGrid(ExpectationEnvironment,NormalizationGrid):
    #@+others
    #@+node:gcross.20111109104457.1736: *4* __init__
    def __init__(self,**keywords):
        if not keywords:
            return
        O_center = keywords["center"]
        O_sides = keywords["sides"]
        O_corners = keywords["corners"]
        super(type(self),self).__init__(O_center.physical_dimension)
        if not isinstance(O_center,OperatorCenterSite):
            raise ValueError("first argument must be the operator center site")
        try:
            assert len(O_sides) == 4
            for i in range(4):
                assert isinstance(O_sides[i],OperatorSideSite)
        except AssertionError:
            raise ValueError("second argument must be a list of 4 operator side boundaries")
        try:
            assert len(O_corners) == 4
            for i in range(4):
                assert isinstance(O_corners[i],OperatorCornerSite)
        except AssertionError:
            raise ValueError("second argument must be a list of 4 operator side corners")
        for i in range(4):
            if O_center.bandwidthDimension(i) != O_sides[i].inward_dimension:
                raise ValueError("the bandwidth dimension of the center site in direction {} is {}, but the inward dimension of the adjacent side is {}".format(i,O_center.bandwidthDimension(i),O_sides[i].inward_dimension))
            if O_sides[i].counterclockwise_dimension != O_corners[i].clockwise_dimension:
                raise ValueError("the counterclockwise dimension of side {} is {}, but the clockwise dimension of its adjacent corner is {}".format(i,O_sides[i].counterclockwise_dimension,O_corners[i].clockwise_dimension))
            if O_sides[i].clockwise_dimension != O_corners[CCW(i)].counterclockwise_dimension:
                raise ValueError("the clockwise dimension of side {} is {}, but the counterclockwise dimension of its adjacent corner is {}".format(i,O_sides[i].counterclockwise_dimension,O_corners[i].clockwise_dimension))
            if O_sides[i].physical_dimension != 1:
                raise ValueError("the physical dimension of side {} is {} when it needs to be exactly 1".format(i,O_sides[i].physical_dimension))
            if O_corners[i].physical_dimension != 1:
                raise ValueError("the physical dimension of corner {} is {} when it needs to be exactly 1".format(i,O_corners[i].physical_dimension))
        if O_center.leftward_dimension != O_center.rightward_dimension:
            raise ValueError("the left and right dimensions of the center tensor need to be equal ({} != {})".format(O_center.left_dimension,O_center.right_dimension))
        if O_center.upward_dimension != O_center.downward_dimension:
            raise ValueError("the upward and downward dimensions of the center tensor need to be equal ({} != {})".format(O_center.upward_dimension,O_center.downward_dimension))
        self.O_center = O_center
        self.O_sides = O_sides
        self.O_corners = O_corners
    #@+node:gcross.20111109104457.1743: *4* contract
    def contract(self,*directions):
        super(type(self),self).contract(*directions)
        for direction in directions:
            self.O_sides[direction] = self.O_sides[direction].absorbCenterSite(self.O_center,direction)
            self.O_corners[direction] = self.O_corners[direction].absorbSideSiteAtCounterClockwise(self.O_sides[CCW(direction)])
            self.O_corners[CW(direction)] = self.O_corners[CW(direction)].absorbSideSiteAtClockwise(self.O_sides[CW(direction)])
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111109104457.1744: ** << Exports >>
__all__ = [
    "NormalizationGrid",
    "ExpectationGrid",
]
#@-<< Exports >>
#@-leo
