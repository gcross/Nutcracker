#@+leo-ver=5-thin
#@+node:gcross.20111110151700.1777: * @file grids.py
#@+<< License >>
#@+node:gcross.20111110151700.1778: ** << License >>
#@-<< License >>

#@+<< Imports >>
#@+node:gcross.20111110151700.1779: ** << Imports >>
from numpy import array, complex128, identity
import random

from ...qubit import Pauli, Qubit
from ...utils import basisVector, CCW
from ..tensors import OperatorCenterSite, OperatorCornerSite, OperatorSideSite, StateCenterSite, StateCornerSite, StateSideSite
#@-<< Imports >>

#@+others
#@+node:gcross.20111110151700.1780: ** Classes
#@+node:gcross.20111110151700.1781: *3* Grid
class Grid(object):
    #@+others
    #@+node:gcross.20111110151700.1782: *4* __init__
    def __init__(self,center,sides,corners):
        if not isinstance(center,self._center_class):
            raise ValueError("the first argument (the center site) must be an instance of {}".format(self._center_class))
        for side in sides:
            if not isinstance(side,self._side_class):
                raise ValueError("all of the items in the second argument (the sides) must be an instance of {}".format(self._side_class))
        for corner in corners:
            if not isinstance(corner,self._corner_class):
                raise ValueError("all of the items in the third argument (the corners) must be an instance of {}".format(self._corner_class))
        if len(sides) != 4:
            raise ValueError("there must be exactly 4 sides, not {}".format(len(sides)))
        if len(corners) != 4:
            raise ValueError("there must be exactly 4 corners, not {}".format(len(corners)))
        for i in range(4):
            if center.bandwidthDimension(i) != sides[i].inward_dimension:
                raise ValueError("the bandwidth dimension of the center site in direction {} is {}, but the inward dimension of the adjacent side is {}".format(i,center.bandwidthDimension(i),sides[i].inward_dimension))
            if sides[i].counterclockwise_dimension != corners[i].clockwise_dimension:
                raise ValueError("the counterclockwise dimension of side {} is {}, but the clockwise dimension of its adjacent corner is {}".format(i,sides[i].counterclockwise_dimension,corners[i].clockwise_dimension))
            if sides[i].clockwise_dimension != corners[CCW(i)].counterclockwise_dimension:
                raise ValueError("the clockwise dimension of side {} is {}, but the counterclockwise dimension of its adjacent corner is {}".format(i,sides[i].clockwise_dimension,corners[CCW(i)].counterclockwise_dimension))
            if sides[i].physical_dimension != 1:
                raise ValueError("the physical dimension of side {} is {} when it needs to be exactly 1".format(i,sides[i].physical_dimension))
            if corners[i].physical_dimension != 1:
                raise ValueError("the physical dimension of corner {} is {} when it needs to be exactly 1".format(i,corners[i].physical_dimension))
        if center.leftward_dimension != center.rightward_dimension:
            raise ValueError("the left and right dimensions of the center tensor need to be equal ({} != {})".format(center.left_dimension,center.right_dimension))
        if center.upward_dimension != center.downward_dimension:
            raise ValueError("the upward and downward dimensions of the center tensor need to be equal ({} != {})".format(center.upward_dimension,center.downward_dimension))
        self.center = center
        self.sides = sides
        self.corners = corners
        self.horizontal_bandwidth_dimension = center.leftward_dimension
        self.vertical_bandwidth_dimension = center.upward_dimension
        self.physical_dimension = center.physical_dimension
    #@+node:gcross.20111110151700.1799: *4* random
    @classmethod
    def random(cls):
        self = cls.trivial()
        horizontal_dimension = random.randint(1,3)
        vertical_dimension = random.randint(1,3)
        self.center = \
            cls._center_class.random(
                physical_dimension=random.randint(1,3),
                rightward_dimension=horizontal_dimension,
                upward_dimension=vertical_dimension,
                leftward_dimension=horizontal_dimension,
                downward_dimension=vertical_dimension,
            )
        self.sides = [
            cls._side_class.random(
                clockwise_dimension = random.randint(1,3),
                counterclockwise_dimension = random.randint(1,3),
                inward_dimension = self.center.bandwidthDimension(i),
                physical_dimension = random.randint(1,3),
            )
            for i in range(4)
        ]
        self.corners = [
            cls._corner_class.random(
                clockwise_dimension = self.sides[i].counterclockwise_dimension,
                counterclockwise_dimension = self.sides[CCW(i)].clockwise_dimension,
                physical_dimension = random.randint(1,3),
            )
            for i in range(4)
        ]
        return self
    #@+node:gcross.20111110151700.1787: *4* simple
    @classmethod
    def simple(cls,component_value):
        return cls(
            center = cls._center_class.simple(component_value),
            sides = [cls._side_class.trivial()]*4,
            corners = [cls._corner_class.trivial()]*4,
        )
    #@+node:gcross.20111110151700.1788: *4* trivial
    @classmethod
    def trivial(cls):
        return cls(
            center = cls._center_class.trivial(),
            sides = [cls._side_class.trivial()]*4,
            corners = [cls._corner_class.trivial()]*4,
        )
    #@-others
#@+node:gcross.20111110151700.1789: *3* OperatorGrid
class OperatorGrid(Grid):
    _center_class = OperatorCenterSite
    _side_class = OperatorSideSite
    _corner_class = OperatorCornerSite
    #@+others
    #@-others

#@+<< Build standard operators >>
#@+node:gcross.20111110151700.1791: *4* << Build standard operators >>
#@+others
#@-others
#@-<< Build standard operators >>
#@+node:gcross.20111110151700.1793: *3* StateGrid
class StateGrid(Grid):
    _center_class = StateCenterSite
    _side_class = StateSideSite
    _corner_class = StateCornerSite
    #@+others
    #@+node:gcross.20111110151700.1794: *4* simpleObservation
    @classmethod
    def simpleObservation(cls,physical_dimension,observation):
        return cls.simple(basisVector(physical_dimension,observation))
    #@-others

#@+<< Build standard states >>
#@+node:gcross.20111110151700.1795: *4* << Build standard states >>
#@+others
#@-others
#@-<< Build standard states >>
#@-others

#@+<< Exports >>
#@+node:gcross.20111110151700.1797: ** << Exports >>
__all__ = [
    "Grid",

    "OperatorGrid",
    "StateGrid",
]
#@-<< Exports >>
#@-leo
