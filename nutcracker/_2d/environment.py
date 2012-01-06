# Imports {{{
from numpy import dot, identity, product, tensordot
from numpy.linalg import cond, svd

from .tensors import *
from ..utils import *
# }}}

class NormalizationEnvironment(object): # {{{
    def bandwidthDimension(self,direction): # {{{
        return self.center.bandwidthDimension(direction)
    # }}}

    def bandwidthDimensions(self): # {{{
        return self.center.bandwidthDimensions()
    # }}}

    def compressAllCorners(self,keep=None,threshold=None): # {{{
        for direction in range(4):
            self.compressCorner(direction,keep=keep,threshold=threshold)
    # }}}

    def compressAllSideCenterConnections(self,keep=None,threshold=None): # {{{
        for direction in range(4):
            self.compressConnectionBetweenSideAndCenter(direction,keep=keep,threshold=threshold)
    # }}}

    def compressAllSideCornerConnections(self,keep=None,threshold=None): # {{{
        for direction in range(4):
            self.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=keep,threshold=threshold)
            self.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=keep,threshold=threshold)
    # }}}

    def compressAllSides(self,keep=None,threshold=None): # {{{
        for direction in range(4):
            self.compressSide(direction,keep=keep,threshold=threshold)
    # }}}

    def compressConnectionBetweenSideAndCenter(self,direction,keep=None,threshold=None): # {{{
        self.sides[direction], self.center = \
            compressConnectionUsingFirstTensorOnlyBetweenTensors(
                self.sides[direction],StateSideSite.inward_index,
                self.center,1+direction,
                keep=keep,threshold=threshold,
            )
    # }}}

    def compressConnectionBetweenSideAndClockwiseCorner(self,direction,keep=None,threshold=None): # {{{
        self.sides[direction], self.corners[CW(direction)] = \
            compressConnectionBetweenTensors(
                self.sides[direction],StateSideSite.clockwise_index,
                self.corners[CW(direction)],StateCornerSite.counterclockwise_index,
                keep=keep,threshold=threshold,
            )
    # }}}

    def compressConnectionBetweenSideAndCounterClockwiseCorner(self,direction,keep=None,threshold=None): # {{{
        self.sides[direction], self.corners[direction] = \
            compressConnectionBetweenTensors(
                self.sides[direction],StateSideSite.counterclockwise_index,
                self.corners[direction],StateCornerSite.clockwise_index,
                keep=keep,threshold=threshold,
            )
    # }}}

    def compressCorner(self,direction,keep=None,threshold=None): # {{{
        self.corners[direction] = \
            compressConnectionToSelfTensor(
                self.corners[direction],
                StateCornerSite.physical_index,
                keep=keep,
                threshold=threshold
            )
    # }}}

    def compressEverything(self,keep=None,threshold=None): # {{{
        self.compressAllCorners(keep,threshold)
        self.compressAllSides(keep,threshold)
        self.compressAllSideCornerConnections(keep,threshold)
        self.compressAllSideCenterConnections(keep,threshold)
    # }}}

    def compressSide(self,direction,keep=None,threshold=None): # {{{
        self.sides[direction] = \
            compressConnectionToSelfTensor(
                self.sides[direction],
                StateSideSite.physical_index,
                keep=keep,
                threshold=threshold
            )
    # }}}

    def computeNormalization(self): # {{{
        return \
            dot(
                dot(
                    self.center.data.reshape(self.physical_dimension,product(self.bandwidthDimensions())),
                    self.computeNormalizationSubmatrix()
                ).ravel(),
                self.center.data.conj().ravel()
            )
    # }}}

    def computeNormalizationConditionNumber(self): # {{{
        return cond(self.computeNormalizationSubmatrix())
    # }}}

    def computeNormalizationMatrix(self): # {{{
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
    # }}}

    def computeNormalizationSubmatrix(self): # {{{
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
    # }}}

    def normalizeAllSides(self): # {{{
        self.normalizeSides(*range(4))
    # }}}

    def normalizeCornerAndDenormalizeClockwiseSide(self,corner_index): # {{{
        self.corners[corner_index], self.sides[corner_index] = \
            normalizeAndDenormalizeTensors(
                self.corners[corner_index],StateCornerSite.clockwise_index,
                self.sides[corner_index],StateSideSite.counterclockwise_index
            )
    # }}}

    def normalizeCornerAndDenormalizeCounterClockwiseSide(self,corner_index): # {{{
        self.corners[corner_index], self.sides[CCW(corner_index)] = \
            normalizeAndDenormalizeTensors(
                self.corners[corner_index],StateCornerSite.counterclockwise_index,
                self.sides[CCW(corner_index)],StateSideSite.clockwise_index
            )
    # }}}

    def normalizeEverything(self): # {{{
        for _ in range(10):
            for direction in range(4):
                self.normalizeCornerAndDenormalizeClockwiseSide(direction)
                self.normalizeCornerAndDenormalizeCounterClockwiseSide(direction)
        self.normalizeAllSides()
    # }}}

    def normalizeSide(self,direction): # {{{
        self.sides[direction], self.center = \
            normalizeAndDenormalizeTensors(self.sides[direction],3,self.center,1+direction)
    # }}}

    def normalizeSides(self,*directions): # {{{
        for direction in directions:
            self.normalizeSide(direction)
    physical_dimension = property(lambda self: self.center.physical_dimension)
    # }}}

    @classmethod
    def trivial(cls): # {{{
        self = cls()
        self.trivialize()
        return self
    # }}}

    def trivialize(self): # {{{
        self.sides = [StateSideSite.trivial()]*4
        self.corners = [StateCornerSite.trivial()]*4
        self.center = StateCenterSite.trivial()
    # }}}
# }}}

class ExpectationEnvironment(NormalizationEnvironment): # {{{
    def compressConnectionBetweenSideAndClockwiseCorner(self,direction,keep=None,threshold=None): # {{{
        super(ExpectationEnvironment,self).compressConnectionBetweenSideAndClockwiseCorner(direction,keep,threshold)
        self.O_sides[direction], self.O_corners[CW(direction)] = \
            compressConnectionBetweenTensors(
                self.O_sides[direction],OperatorSideSite.clockwise_index,
                self.O_corners[CW(direction)],OperatorCornerSite.counterclockwise_index,
                keep=keep,threshold=threshold,
            )
    # }}}

    def compressConnectionBetweenSideAndCounterClockwiseCorner(self,direction,keep=None,threshold=None): # {{{
        super(ExpectationEnvironment,self).compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep,threshold)
        self.O_sides[direction], self.O_corners[direction] = \
            compressConnectionBetweenTensors(
                self.O_sides[direction],OperatorSideSite.counterclockwise_index,
                self.O_corners[direction],OperatorCornerSite.clockwise_index,
                keep=keep,threshold=threshold,
            )
    # }}}

    def compressCorner(self,direction,keep=None,threshold=None): # {{{
        self.corners[direction], self.O_corners[direction] = \
            compressHermitianConnectionUsingFirstTensorOnlyBetweenTensors(
                self.corners[direction], StateCornerSite.physical_index,
                self.O_corners[direction], OperatorCornerSite.physical_index, OperatorCornerSite.physical_conjugate_index,
                keep=keep,
                threshold=threshold
            )
    # }}}

    def compressSide(self,direction,keep=None,threshold=None): # {{{
        self.sides[direction], self.O_sides[direction] = \
            compressHermitianConnectionUsingFirstTensorOnlyBetweenTensors(
                self.sides[direction], StateSideSite.physical_index,
                self.O_sides[direction], OperatorSideSite.physical_index, OperatorSideSite.physical_conjugate_index,
                keep=keep,
                threshold=threshold
            )
    # }}}

    def computeExpectation(self): # {{{
        return \
            dot(
                dot(
                    self.center.data.ravel(),
                    self.computeExpectationMatrix()
                ),
                self.center.data.conj().ravel()
            )/self.computeNormalization()
    # }}}

    def computeExpectationMatrix(self): # {{{
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
    # }}}

    def trivialize(self): # {{{
        super(type(self),self).trivialize()
        self.O_sides = [OperatorSideSite.trivial()]*4
        self.O_corners = [OperatorCornerSite.trivial()]*4
        self.O_center = OperatorCenterSite.trivial()
    # }}}
# }}}

# Exports {{{
__all__ = [
    "NormalizationEnvironment",
    "ExpectationEnvironment",
]
# }}}
