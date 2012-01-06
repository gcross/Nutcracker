# Imports {{{
from numpy import array, complex128, inner, ndarray, tensordot

from ..tensors import *
from ..utils import *
# }}}

# Classes {{{

#   Metaclasses {{{

class MetaSideSiteTensor(MetaSiteTensor): # {{{
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
# }}}

#   }}}

#   Base Classes {{{

class SideSiteTensor(SiteTensor): # {{{
    __metaclass__ = MetaSideSiteTensor

    def absorbCenterSite(self,center,direction): # {{{
        return type(self)(self._center_site_contractors[direction](self.data,center.data))
    # }}}

    @classmethod
    def simpleInward(cls,inward_vector): # {{{
        shape = [1]*cls.number_of_dimensions
        shape[cls.inward_index] = len(inward_vector)
        return cls(array(inward_vector,dtype=complex128).reshape(shape))
    # }}}

# }}}

#   }}}

#   Tensors {{{

class CornerBoundary(Tensor): # {{{
    dimension_names = ["clockwise","counterclockwise"]
# }}}

class ExpectationSideBoundary(Tensor): # {{{
    dimension_names = ["clockwise","counterclockwise","inward_state","inward_operator","inward_state_conjugate"]
    def absorbCounterClockwiseCornerBoundary(self,corner): # {{{
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
    # }}}

    def absorbCounterClockwiseSideBoundary(self,side): # {{{
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
    # }}}
# }}}

class NormalizationSideBoundary(Tensor): # {{{
    dimension_names = ["clockwise","counterclockwise","inward","inward_conjugate"]

    def absorbCounterClockwiseCornerBoundary(self,corner): # {{{
        return type(self)(
             tensordot(self.data,corner.data,(self.counterclockwise_index,corner.clockwise_index))
            .transpose(
                self.clockwise_index,
                corner.counterclockwise_index+3-1,
                self.inward_index-1,
                self.inward_conjugate_index-1,
             )
        )
    # }}}

    def absorbCounterClockwiseSideBoundary(self,side): # {{{
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
    # }}}
# }}}

class OperatorCenterSite(SiteTensor): # {{{
    dimension_names = ["physical","physical_conjugate","rightward","upward","leftward","downward"]
# }}}

class OperatorCornerSite(SiteTensor): # {{{
    dimension_names = ["physical","physical_conjugate","clockwise","counterclockwise"]

    def absorbSideSiteAtClockwise(self,side): # {{{
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
    # }}}

    def absorbSideSiteAtCounterClockwise(self,side): # {{{
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
    # }}}

    def formExpectationBoundary(self,state): # {{{
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
    # }}}
# }}}

class OperatorSideSite(SideSiteTensor): # {{{
    dimension_names = ["physical","physical_conjugate","clockwise","counterclockwise","inward"]
    _center_site_class = OperatorCenterSite

    def formExpectationBoundary(self,state): # {{{
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
    # }}}
# }}}

class StateCenterSite(StateSiteTensor): # {{{
    dimension_names = ["physical","rightward","upward","leftward","downward"]
# }}}

class StateCornerSite(SiteTensor): # {{{
    dimension_names = ["physical","clockwise","counterclockwise"]

    def absorbSideSiteAtClockwise(self,side): # {{{
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
    # }}}

    def absorbSideSiteAtCounterClockwise(self,side): # {{{
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
    # }}}

    def formNormalizationBoundary(self): # {{{
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
    # }}}

    def normalizeSelfAndDenormalizeClockwiseSide(self,side): # {{{
        return normalizeAndDenormalizeTensors(self,self.clockwise_index,side,side.counterclockwise_index)
    # }}}

    def normalizeSelfAndDenormalizeCounterClockwiseSide(self,side): # {{{
        return normalizeAndDenormalizeTensors(self,self.counterclockwise_index,side,side.clockwise_index)
    # }}}
# }}}

class StateSideSite(SideSiteTensor): # {{{
    dimension_names = ["physical","clockwise","counterclockwise","inward"]
    _center_site_class = StateCenterSite

    def formNormalizationBoundary(self): # {{{
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
    # }}}

    def normalizeSelfAndDenormalizeCenter(self,center,direction): # {{{
        return normalizeAndDenormalizeTensors(self,self.inward_index,center,1+direction)
    # }}}
# }}}

#   }}}

# }}}

# Exports {{{
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
# }}}
