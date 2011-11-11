#@+leo-ver=5-thin
#@+node:gcross.20111108103420.1472: * @file tensors.py
#@+<< Imports >>
#@+node:gcross.20111108103420.1473: ** << Imports >>
from numpy import array, complex128, inner, ndarray, tensordot

from ..tensors import *
from ..utils import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111108103420.1474: ** Classes
#@+node:gcross.20111108103420.1475: *3* Metaclasses
#@+node:gcross.20111108103420.1480: *4* MetaSideSiteTensor
class MetaSideSiteTensor(MetaSiteTensor):
    #@+others
    #@+node:gcross.20111108103420.1481: *5* __init__
    def __new__(cls,class_name,bases,data):
        cls = MetaSiteTensor.__new__(cls,class_name,bases,data)
        if hasattr(cls,"_center_site_class"):
            center_cls = cls._center_site_class
            assert cls.physical_dimension_names == center_cls.physical_dimension_names
            contractors = []
            for i in range(4):
                outgoing_connection_for = {
                    "clockwise":[('S',cls.clockwise_index),('C',center_cls.bandwidthIndex(CW(i)))],
                    "counterclockwise":[('S',cls.counterclockwise_index),('C',center_cls.bandwidthIndex(CCW(i)))],
                    "inward":[('C',center_cls.bandwidthIndex(OPP(i))),]
                }
                for (index,name) in enumerate(cls.physical_dimension_names):
                    outgoing_connection_for[name] = [('S',cls.physicalIndex(index)),('C',center_cls.physicalIndex(index))]
                contractors.append(
                    formContractor(
                        ['S','C'],
                        [
                            (('S',cls.inward_index),('C',center_cls.bandwidthIndex(i))),
                        ],
                        [
                            outgoing_connection_for[name] for name in cls.dimension_names
                        ]
                    )
                )
            cls._center_site_contractors = contractors
        return cls
    #@-others
#@+node:gcross.20111108103420.1482: *3* Base classes
#@+node:gcross.20111108103420.1495: *4* SideSiteTensor
class SideSiteTensor(SiteTensor):
    __metaclass__ = MetaSideSiteTensor
    #@+others
    #@+node:gcross.20111108103420.1496: *5* absorbCenterSite
    def absorbCenterSite(self,center,direction):
        return type(self)(self._center_site_contractors[direction](self.data,center.data))
    #@+node:gcross.20111110233742.1789: *5* simpleInward
    @classmethod
    def simpleInward(cls,inward_vector):
        shape = [1]*cls.number_of_dimensions
        shape[cls.inward_index] = len(inward_vector)
        return cls(array(inward_vector,dtype=complex128).reshape(shape))
    #@-others
#@+node:gcross.20111108103420.1497: *3* Tensors
#@+others
#@+node:gcross.20111108103420.1498: *4* CornerBoundary
class CornerBoundary(Tensor):
    dimension_names = ["clockwise","counterclockwise"]
    #@+others
    #@-others
#@+node:gcross.20111108103420.1499: *4* ExpectationSideBoundary
class ExpectationSideBoundary(Tensor):
    dimension_names = ["clockwise","counterclockwise","inward_state","inward_operator","inward_state_conjugate"]
    #@+others
    #@+node:gcross.20111108103420.1500: *5* absorbCounterClockwiseCornerBoundary
    def absorbCounterClockwiseCornerBoundary(self,corner):
        return type(self)(
             tensordot(self.data,corner.data,(self.counterclockwise_index,corner.clockwise_index))
            .transpose(
                self.clockwise_index,
                corner.counterclockwise_index+4-1,
                self.inward_state_index-1,
                self.inward_operator_index-1,
                self.inward_state_conjugate_index-1,
             )
        )
    #@+node:gcross.20111108103420.1501: *5* absorbCounterClockwiseSideBoundary
    def absorbCounterClockwiseSideBoundary(self,side):
        return type(self)(
             tensordot(self.data,side.data,(self.counterclockwise_index,side.clockwise_index))
            .transpose(
                self.clockwise_index,
                side.counterclockwise_index+4-1,
                self.inward_state_index-1,
                side.inward_state_index+4-1,
                self.inward_operator_index-1,
                side.inward_operator_index+4-1,
                self.inward_state_conjugate_index-1,
                side.inward_state_conjugate_index+4-1,
             )
            .reshape(
                self.clockwise_dimension,
                side.counterclockwise_dimension,
                self.inward_state_dimension*side.inward_state_dimension,
                self.inward_operator_dimension*side.inward_operator_dimension,
                self.inward_state_conjugate_dimension*side.inward_state_conjugate_dimension,
             )
        )
    #@-others
#@+node:gcross.20111108103420.1502: *4* NormalizationSideBoundary
class NormalizationSideBoundary(Tensor):
    dimension_names = ["clockwise","counterclockwise","inward","inward_conjugate"]
    #@+others
    #@+node:gcross.20111108103420.1503: *5* absorbCounterClockwiseCornerBoundary
    def absorbCounterClockwiseCornerBoundary(self,corner):
        return type(self)(
             tensordot(self.data,corner.data,(self.counterclockwise_index,corner.clockwise_index))
            .transpose(
                self.clockwise_index,
                corner.counterclockwise_index+3-1,
                self.inward_index-1,
                self.inward_conjugate_index-1,
             )
        )
    #@+node:gcross.20111108103420.1504: *5* absorbCounterClockwiseSideBoundary
    def absorbCounterClockwiseSideBoundary(self,side):
        return type(self)(
             tensordot(self.data,side.data,(self.counterclockwise_index,side.clockwise_index))
            .transpose(
                self.clockwise_index,
                side.counterclockwise_index+3-1,
                self.inward_index-1,
                side.inward_index+3-1,
                self.inward_conjugate_index-1,
                side.inward_conjugate_index+3-1,
             )
            .reshape(
                self.clockwise_dimension,
                side.counterclockwise_dimension,
                self.inward_dimension*side.inward_dimension,
                self.inward_dimension*side.inward_dimension,
             )
        )
    #@-others
#@+node:gcross.20111108103420.1505: *4* OperatorCenterSite
class OperatorCenterSite(SiteTensor):
    dimension_names = ["physical","physical_conjugate","rightward","upward","leftward","downward"]
    #@+others
    #@-others
#@+node:gcross.20111108103420.1506: *4* OperatorCornerSite
class OperatorCornerSite(SiteTensor):
    dimension_names = ["physical","physical_conjugate","clockwise","counterclockwise"]
    #@+others
    #@+node:gcross.20111108103420.1507: *5* absorbSideSiteAtClockwise
    def absorbSideSiteAtClockwise(self,side):
        return type(self)(
             tensordot(self.data,side.data,(self.clockwise_index,side.counterclockwise_index))
            .transpose(
                self.physical_index,
                side.physical_index+3,
                self.physical_conjugate_index,
                side.physical_conjugate_index+3,
                side.clockwise_index+3,
                self.counterclockwise_index-1,
                side.inward_index+3-1,
             )
            .reshape(
                self.physical_dimension*side.physical_dimension,
                self.physical_conjugate_dimension*side.physical_conjugate_dimension,
                side.clockwise_dimension,
                self.counterclockwise_dimension*side.inward_dimension,
             )
        )
    #@+node:gcross.20111108103420.1508: *5* absorbSideSiteAtCounterClockwise
    def absorbSideSiteAtCounterClockwise(self,side):
        return type(self)(
             tensordot(self.data,side.data,(self.counterclockwise_index,side.clockwise_index))
            .transpose(
                self.physical_index,
                side.physical_index+3,
                self.physical_conjugate_index,
                side.physical_conjugate_index+3,
                self.clockwise_index,
                side.inward_index+3-1,
                side.counterclockwise_index+3-1,
             )
            .reshape(
                self.physical_dimension*side.physical_dimension,
                self.physical_conjugate_dimension*side.physical_conjugate_dimension,
                self.clockwise_dimension*side.inward_dimension,
                side.counterclockwise_dimension,
             )
        )
    #@+node:gcross.20111108103420.1509: *5* formExpectationBoundary
    def formExpectationBoundary(self,state):
        return CornerBoundary(
             tensordot(
                tensordot(state.data,self.data,(state.physical_index,self.physical_index)),
                state.data.conj(),
                (self.physical_index+2,state.physical_index),
             )
            .transpose(
                state.clockwise_index-1,
                self.clockwise_index+2-2,
                state.clockwise_index+4-1,
                state.counterclockwise_index-1,
                self.counterclockwise_index+2-2,
                state.counterclockwise_index+4-1,
             )
            .reshape(
                state.clockwise_dimension*self.clockwise_dimension*state.clockwise_dimension,
                state.counterclockwise_dimension*self.counterclockwise_dimension*state.counterclockwise_dimension,
             )
        )
    #@-others
#@+node:gcross.20111108103420.1510: *4* OperatorSideSite
class OperatorSideSite(SideSiteTensor):
    dimension_names = ["physical","physical_conjugate","clockwise","counterclockwise","inward"]
    _center_site_class = OperatorCenterSite
    #@+others
    #@+node:gcross.20111108103420.1511: *5* formExpectationBoundary
    def formExpectationBoundary(self,state):
        return ExpectationSideBoundary(
             tensordot(
                tensordot(state.data,self.data,(state.physical_index,self.physical_index)),
                state.data.conj(),
                (self.physical_index+3,state.physical_index),
             )
            .transpose(
                state.clockwise_index-1,
                self.clockwise_index+3-2,
                state.clockwise_index+6-1,
                state.counterclockwise_index-1,
                self.counterclockwise_index+3-2,
                state.counterclockwise_index+6-1,
                state.inward_index-1,
                self.inward_index+3-2,
                state.inward_index+6-1,
             )
            .reshape(
                state.clockwise_dimension*self.clockwise_dimension*state.clockwise_dimension,
                state.counterclockwise_dimension*self.counterclockwise_dimension*state.counterclockwise_dimension,
                state.inward_dimension,
                self.inward_dimension,
                state.inward_dimension,
             )
        )
    #@-others
#@+node:gcross.20111108103420.1512: *4* StateCenterSite
class StateCenterSite(StateSiteTensor):
    dimension_names = ["physical","rightward","upward","leftward","downward"]
    #@+others
    #@-others
#@+node:gcross.20111108103420.1513: *4* StateCornerSite
class StateCornerSite(SiteTensor):
    dimension_names = ["physical","clockwise","counterclockwise"]
    #@+others
    #@+node:gcross.20111108103420.1514: *5* absorbSideSiteAtClockwise
    def absorbSideSiteAtClockwise(self,side):
        return type(self)(
             tensordot(self.data,side.data,(self.clockwise_index,side.counterclockwise_index))
            .transpose(
                self.physical_index,
                side.physical_index+2,
                side.clockwise_index+2,
                self.counterclockwise_index-1,
                side.inward_index+2-1,
             )
            .reshape(
                self.physical_dimension*side.physical_dimension,
                side.clockwise_dimension,
                self.counterclockwise_dimension*side.inward_dimension,
             )
        )
    #@+node:gcross.20111108103420.1515: *5* absorbSideSiteAtCounterClockwise
    def absorbSideSiteAtCounterClockwise(self,side):
        return type(self)(
             tensordot(self.data,side.data,(self.counterclockwise_index,side.clockwise_index))
            .transpose(
                self.physical_index,
                side.physical_index+2,
                self.clockwise_index,
                side.inward_index+2-1,
                side.counterclockwise_index+2-1
             )
            .reshape(
                self.physical_dimension*side.physical_dimension,
                self.clockwise_dimension*side.inward_dimension,
                side.counterclockwise_dimension,
             )
        )
    #@+node:gcross.20111108103420.1516: *5* formNormalizationBoundary
    def formNormalizationBoundary(self):
        return CornerBoundary(
             tensordot(self.data,self.data.conj(),(self.physical_index,)*2)
            .transpose(
                self.clockwise_index-1,
                self.clockwise_index+2-1,
                self.counterclockwise_index-1,
                self.counterclockwise_index+2-1,
             )
            .reshape(
                self.clockwise_dimension*self.clockwise_dimension,
                self.counterclockwise_dimension*self.counterclockwise_dimension,
             )
        )
    #@+node:gcross.20111108103420.1517: *5* normalizeSelfAndDenormalizeClockwiseSide
    def normalizeSelfAndDenormalizeClockwiseSide(self,side):
        return normalizeAndDenormalizeTensors(self,self.clockwise_index,side,side.counterclockwise_index)
    #@+node:gcross.20111108103420.1518: *5* normalizeSelfAndDenormalizeCounterClockwiseSide
    def normalizeSelfAndDenormalizeCounterClockwiseSide(self,side):
        return normalizeAndDenormalizeTensors(self,self.counterclockwise_index,side,side.clockwise_index)
    #@-others
#@+node:gcross.20111108103420.1519: *4* StateSideSite
class StateSideSite(SideSiteTensor):
    dimension_names = ["physical","clockwise","counterclockwise","inward"]
    _center_site_class = StateCenterSite
    #@+others
    #@+node:gcross.20111108103420.1520: *5* formNormalizationBoundary
    def formNormalizationBoundary(self):
        return NormalizationSideBoundary(
             tensordot(self.data,self.data.conj(),(self.physical_index,)*2)
            .transpose(
                self.clockwise_index-1,
                self.clockwise_index+3-1,
                self.counterclockwise_index-1,
                self.counterclockwise_index+3-1,
                self.inward_index-1,
                self.inward_index+3-1,
             )
            .reshape(
                self.clockwise_dimension*self.clockwise_dimension,
                self.counterclockwise_dimension*self.counterclockwise_dimension,
                self.inward_dimension,
                self.inward_dimension
             )
        )
    #@+node:gcross.20111108103420.1521: *5* normalizeSelfAndDenormalizeCenter
    def normalizeSelfAndDenormalizeCenter(self,center,direction):
        return normalizeAndDenormalizeTensors(self,self.inward_index,center,1+direction)
    #@-others
#@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111108103420.1522: ** << Exports >>
__all__ = [
    "CornerBoundary",
    "ExpectationSideBoundary",
    "NormalizationSideBoundary",
    "OperatorCenterSite",
    "OperatorCornerSite",
    "OperatorSideSite",
    "StateCenterSite",
    "StateCornerSite",
    "StateSideSite",
]
#@-<< Exports >>
#@-leo
