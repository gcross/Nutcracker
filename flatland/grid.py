#@+leo-ver=5-thin
#@+node:gcross.20111009193003.5253: * @file grid.py
#@+<< Imports >>
#@+node:gcross.20111009193003.5254: ** << Imports >>
from numpy import array, identity, product, tensordot

from tensors import StateCenterSite, StateCornerSite, StateSideSite
#@-<< Imports >>

#@+others
#@+node:gcross.20111009193003.5256: ** Classes
#@+node:gcross.20111009193003.5257: *3* Grid
class Grid:
    #@+others
    #@+node:gcross.20111009193003.5258: *4* __init__
    def __init__(self,physical_dimension):
        self.physical_dimension = physical_dimension
        self.sides = [StateSideSite.trivial()]*4
        self.corners = [StateCornerSite.trivial()]*4
        self.center = \
            StateCenterSite(
                physical_dimension = physical_dimension,
                right_dimension = 1,
                up_dimension = 1,
                left_dimension = 1,
                down_dimension = 1,
            )
        self.center.data[:,0,0,0,0] = array([1] + [0]*(physical_dimension-1))
    #@+node:gcross.20111009193003.5260: *4* computeNormalizationMatrix
    def computeNormalizationMatrix(self):
        side_boundaries = [side.formBoundary().absorbCounterClockwiseCornerBoundary(corner.formBoundary()) for (side,corner) in zip(self.sides,self.corners)]
        final_dimension = self.physical_dimension*product([side.inward_dimension for side in self.sides])
        return (
             tensordot(
                 identity(self.physical_dimension),
                 tensordot(
                    side_boundaries[0].absorbCounterClockwiseSideBoundary(side_boundaries[1]).data,
                    side_boundaries[2].absorbCounterClockwiseSideBoundary(side_boundaries[3]).data,
                    ((1,0),(0,1))
                 ),
                 ((),())
             )
            .transpose(0,2,4,1,3,5)
            .reshape(final_dimension,final_dimension)
        )
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111009193003.5255: ** << Exports >>
__all__ = [
    "Grid",
]
#@-<< Exports >>
#@-leo
