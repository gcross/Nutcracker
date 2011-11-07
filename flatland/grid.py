#@+leo-ver=5-thin
#@+node:gcross.20111009193003.5253: * @file grid.py
#@+<< Imports >>
#@+node:gcross.20111009193003.5254: ** << Imports >>
from numpy import array, dot, identity, product, tensordot
from numpy.linalg import cond, svd

from flatland.tensors import *
from flatland.utils import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111009193003.5256: ** Classes
#@+node:gcross.20111009193003.5257: *3* NormalizationGrid
class NormalizationGrid:
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
        side_boundaries = [side.formNormalizationBoundary().absorbCounterClockwiseCornerBoundary(corner.formNormalizationBoundary()) for (side,corner) in zip(self.sides,self.corners)]
        final_dimension = product(self.bandwidthDimensions())
        return (
             tensordot(
                side_boundaries[0].absorbCounterClockwiseSideBoundary(side_boundaries[1]).data,
                side_boundaries[2].absorbCounterClockwiseSideBoundary(side_boundaries[3]).data,
                ((NormalizationSideBoundary.clockwise_index,NormalizationSideBoundary.counterclockwise_index)
                ,(NormalizationSideBoundary.counterclockwise_index,NormalizationSideBoundary.clockwise_index)
                )
             )
            .transpose(
                NormalizationSideBoundary.inward_index-2,
                NormalizationSideBoundary.inward_index+2-2,
                NormalizationSideBoundary.inward_conjugate_index-2,
                NormalizationSideBoundary.inward_conjugate_index+2-2,
             )
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
#@+node:gcross.20111103170337.1374: *3* ExpectationGrid
class ExpectationGrid(NormalizationGrid):
    #@+others
    #@+node:gcross.20111103170337.1375: *4* __init__
    def __init__(self,O_center,O_sides,O_corners):
        NormalizationGrid.__init__(self,O_center.physical_dimension)
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
    #@+node:gcross.20111107123047.1378: *4* compressConnectionBetweenSideAndClockwiseCorner
    def compressConnectionBetweenSideAndClockwiseCorner(self,direction,keep=None,threshold=None):
        NormalizationGrid.compressConnectionBetweenSideAndClockwiseCorner(self,direction,keep,threshold)
        self.O_sides[direction], self.O_corners[CW(direction)] = \
            compressConnectionBetweenTensors(
                self.O_sides[direction],OperatorSideSite.clockwise_index,
                self.O_corners[CW(direction)],OperatorCornerSite.counterclockwise_index,
                keep=keep,threshold=threshold,
            )
    #@+node:gcross.20111107123047.1380: *4* compressConnectionBetweenSideAndCounterClockwiseCorner
    def compressConnectionBetweenSideAndCounterClockwiseCorner(self,direction,keep=None,threshold=None):
        NormalizationGrid.compressConnectionBetweenSideAndCounterClockwiseCorner(self,direction,keep,threshold)
        self.O_sides[direction], self.O_corners[direction] = \
            compressConnectionBetweenTensors(
                self.O_sides[direction],OperatorSideSite.counterclockwise_index,
                self.O_corners[direction],OperatorCornerSite.clockwise_index,
                keep=keep,threshold=threshold,
            )
    #@+node:gcross.20111103170337.1382: *4* computeExpectation
    def computeExpectation(self):
        return \
            dot(
                dot(
                    self.center.data.ravel(),
                    self.computeExpectationMatrix()
                ),
                self.center.data.conj().ravel()
            )/self.computeNormalization()
    #@+node:gcross.20111103170337.1379: *4* computeExpectationMatrix
    def computeExpectationMatrix(self):
        side_boundaries = [
            O_side.formExpectationBoundary(S_side).absorbCounterClockwiseCornerBoundary(O_corner.formExpectationBoundary(S_corner))
            for (O_side,S_side,O_corner,S_corner) in zip(self.O_sides,self.sides,self.O_corners,self.corners)
        ]
        total_state_bandwidth_dimension = product(self.bandwidthDimensions())
        total_operator_bandwidth_dimension = product(self.O_center.bandwidthDimensions())
        matrix_dimension = self.physical_dimension*total_state_bandwidth_dimension
        return (
             tensordot(
                 self.O_center.data.reshape(self.physical_dimension,self.physical_dimension,product(total_operator_bandwidth_dimension))
                ,tensordot(
                    side_boundaries[0].absorbCounterClockwiseSideBoundary(side_boundaries[1]).data,
                    side_boundaries[2].absorbCounterClockwiseSideBoundary(side_boundaries[3]).data,
                    ((ExpectationSideBoundary.clockwise_index,ExpectationSideBoundary.counterclockwise_index)
                    ,(ExpectationSideBoundary.counterclockwise_index,ExpectationSideBoundary.clockwise_index)
                    )
                 )
                .transpose(
                    ExpectationSideBoundary.inward_operator_index-2,
                    ExpectationSideBoundary.inward_operator_index+3-2,
                    ExpectationSideBoundary.inward_state_index-2,
                    ExpectationSideBoundary.inward_state_index+3-2,
                    ExpectationSideBoundary.inward_state_conjugate_index-2,
                    ExpectationSideBoundary.inward_state_conjugate_index+3-2,
                 )
                .reshape(
                    total_operator_bandwidth_dimension,
                    total_state_bandwidth_dimension,
                    total_state_bandwidth_dimension,
                 )
                ,axes=1
             )
            .transpose(0,2,1,3)
            .reshape(matrix_dimension,matrix_dimension)
        )
    #@+node:gcross.20111103170337.1377: *4* contract
    def contract(self,*directions):
        super(ExpectationGrid,self).contract(*directions)
        for direction in directions:
            self.O_sides[direction] = self.O_sides[direction].absorbCenterSite(self.O_center,direction)
            self.O_corners[direction] = self.O_corners[direction].absorbSideSiteAtCounterClockwise(self.O_sides[CCW(direction)])
            self.O_corners[CW(direction)] = self.O_corners[CW(direction)].absorbSideSiteAtClockwise(self.O_sides[CW(direction)])
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111009193003.5255: ** << Exports >>
__all__ = [
    "NormalizationGrid",
    "ExpectationGrid",
]
#@-<< Exports >>
#@-leo
