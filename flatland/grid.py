#@+leo-ver=5-thin
#@+node:gcross.20111009193003.5253: * @file grid.py
#@+<< Imports >>
#@+node:gcross.20111009193003.5254: ** << Imports >>
from numpy import array, dot, identity, product, tensordot
from numpy.linalg import cond, svd

from flatland.tensors import StateCenterSite, StateCornerSite, StateSideSite
from flatland.utils import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111009193003.5256: ** Classes
#@+node:gcross.20111009193003.5257: *3* Grid
class Grid:
    #@+others
    #@+node:gcross.20111009193003.5258: *4* __init__
    def __init__(self,physical_dimension):
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
    #@+node:gcross.20111014113710.1235: *4* bandwidthDimension
    def bandwidthDimension(self,direction):
        return self.center.bandwidthDimension(direction)
    #@+node:gcross.20111014113710.1237: *4* bandwidthDimensions
    def bandwidthDimensions(self):
        return self.center.bandwidthDimensions()
    #@+node:gcross.20111022200315.1302: *4* compressAllCorners
    def compressAllCorners(self,keep=None,threshold=None):
        for direction in range(4):
            self.compressCorner(direction,keep=keep,threshold=threshold)
    #@+node:gcross.20111024143336.1341: *4* compressAllSideCenterConnections
    def compressAllSideCenterConnections(self,keep=None,threshold=None):
        for direction in range(4):
            self.compressConnectionBetweenSideAndCenter(direction,keep=keep,threshold=threshold)
    #@+node:gcross.20111024143336.1308: *4* compressAllSideCornerConnections
    def compressAllSideCornerConnections(self,keep=None,threshold=None):
        for direction in range(4):
            self.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=keep,threshold=threshold)
            self.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=keep,threshold=threshold)
    #@+node:gcross.20111022200315.1304: *4* compressAllSides
    def compressAllSides(self,keep=None,threshold=None):
        for direction in range(4):
            self.compressSide(direction,keep=keep,threshold=threshold)
    #@+node:gcross.20111024143336.1333: *4* compressConnectionBetweenSideAndCenter
    def compressConnectionBetweenSideAndCenter(self,direction,keep=None,threshold=None):
        self.sides[direction], self.center = \
            compressConnectionUsingFirstTensorOnlyBetweenTensors(
                self.sides[direction],StateSideSite.inward_index,
                self.center,1+direction,
                keep=keep,threshold=threshold,
            )
    #@+node:gcross.20111022200315.1338: *4* compressConnectionBetweenSideAndClockwiseCorner
    def compressConnectionBetweenSideAndClockwiseCorner(self,direction,keep=None,threshold=None):
        self.sides[direction], self.corners[CW(direction)] = \
            compressConnectionBetweenTensors(
                self.sides[direction],StateSideSite.clockwise_index,
                self.corners[CW(direction)],StateCornerSite.counterclockwise_index,
                keep=keep,threshold=threshold,
            )
    #@+node:gcross.20111022200315.1336: *4* compressConnectionBetweenSideAndCounterClockwiseCorner
    def compressConnectionBetweenSideAndCounterClockwiseCorner(self,direction,keep=None,threshold=None):
        self.sides[direction], self.corners[direction] = \
            compressConnectionBetweenTensors(
                self.sides[direction],StateSideSite.counterclockwise_index,
                self.corners[direction],StateCornerSite.clockwise_index,
                keep=keep,threshold=threshold,
            )
    #@+node:gcross.20111022200315.1285: *4* compressCorner
    def compressCorner(self,direction,keep=None,threshold=None):
        self.corners[direction] = \
            compressConnectionToSelfTensor(
                self.corners[direction],
                StateCornerSite.physical_index,
                keep=keep,
                threshold=threshold
            )
    #@+node:gcross.20111024143336.1309: *4* compressEverything
    def compressEverything(self,keep=None,threshold=None):
        self.compressAllCorners(keep,threshold)
        self.compressAllSides(keep,threshold)
        self.compressAllSideCornerConnections(keep,threshold)
        self.compressAllSideCenterConnections(keep,threshold)
    #@+node:gcross.20111022200315.1292: *4* compressSide
    def compressSide(self,direction,keep=None,threshold=None):
        self.sides[direction] = \
            compressConnectionToSelfTensor(
                self.sides[direction],
                StateSideSite.physical_index,
                keep=keep,
                threshold=threshold
            )
    #@+node:gcross.20111013165152.1229: *4* computeNormalization
    def computeNormalization(self):
        return \
            dot(
                dot(
                    self.center.data.reshape(self.physical_dimension,product(self.bandwidthDimensions())),
                    self.computeNormalizationSubmatrix()
                ).ravel(),
                self.center.data.conj().ravel()
            )
    #@+node:gcross.20111013080525.1260: *4* computeNormalizationConditionNumber
    def computeNormalizationConditionNumber(self):
        return cond(self.computeNormalizationSubmatrix())
    #@+node:gcross.20111009193003.5260: *4* computeNormalizationMatrix
    def computeNormalizationMatrix(self):
        submatrix = self.computeNormalizationSubmatrix()
        return (
             tensordot(
                identity(self.physical_dimension),
                submatrix,
                ((),()),
             )
            .transpose(0,2,1,3)
            .reshape(*((self.physical_dimension*submatrix.shape[0],)*2))
        )
    #@+node:gcross.20111013080525.1259: *4* computeNormalizationSubmatrix
    def computeNormalizationSubmatrix(self):
        side_boundaries = [side.formBoundary().absorbCounterClockwiseCornerBoundary(corner.formBoundary()) for (side,corner) in zip(self.sides,self.corners)]
        final_dimension = product([side.inward_dimension for side in self.sides])
        return (
             tensordot(
                side_boundaries[0].absorbCounterClockwiseSideBoundary(side_boundaries[1]).data,
                side_boundaries[2].absorbCounterClockwiseSideBoundary(side_boundaries[3]).data,
                ((1,0),(0,1))
             )
            .transpose(0,2,1,3)
            .reshape(final_dimension,final_dimension)
        )
    #@+node:gcross.20111013080525.1244: *4* contract
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
    #@+node:gcross.20111017110141.1278: *4* normalizeAllSides
    def normalizeAllSides(self):
        self.normalizeSides(*range(4))
    #@+node:gcross.20111017110141.1265: *4* normalizeCornerAndDenormalizeClockwiseSide
    def normalizeCornerAndDenormalizeClockwiseSide(self,corner_index):
        self.corners[corner_index], self.sides[corner_index] = \
            normalizeAndDenormalizeTensors(
                self.corners[corner_index],StateCornerSite.clockwise_index,
                self.sides[corner_index],StateSideSite.counterclockwise_index
            )
    #@+node:gcross.20111017110141.1267: *4* normalizeCornerAndDenormalizeCounterClockwiseSide
    def normalizeCornerAndDenormalizeCounterClockwiseSide(self,corner_index):
        self.corners[corner_index], self.sides[CCW(corner_index)] = \
            normalizeAndDenormalizeTensors(
                self.corners[corner_index],StateCornerSite.counterclockwise_index,
                self.sides[CCW(corner_index)],StateSideSite.clockwise_index
            )
    #@+node:gcross.20111024143336.1312: *4* normalizeEverything
    def normalizeEverything(self):
        for _ in range(10):
            for direction in range(4):
                self.normalizeCornerAndDenormalizeClockwiseSide(direction)
                self.normalizeCornerAndDenormalizeCounterClockwiseSide(direction)
        self.normalizeAllSides()
    #@+node:gcross.20111014113710.1230: *4* normalizeSide
    def normalizeSide(self,direction):
        self.sides[direction], self.center = \
            normalizeAndDenormalizeTensors(self.sides[direction],3,self.center,1+direction)
    #@+node:gcross.20111017110141.1277: *4* normalizeSides
    def normalizeSides(self,*directions):
        for direction in directions:
            self.normalizeSide(direction)
    #@+node:gcross.20111014113710.1234: *4* physical_dimension
    physical_dimension = property(lambda self: self.center.physical_dimension)
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111009193003.5255: ** << Exports >>
__all__ = [
    "Grid",
]
#@-<< Exports >>
#@-leo
