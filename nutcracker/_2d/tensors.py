# Imports {{{
from numpy import array, complex128, ndarray

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
            contractAndTranspose(
                (self.data,corner.data),
                ([self.counterclockwise_index],[corner.clockwise_index]),
                new_order=[
                    indexOfFirstTensor(self.clockwise_index),
                    indexOfSecondTensor(corner.counterclockwise_index),
                    indexOfFirstTensor(self.inward_state_index),
                    indexOfFirstTensor(self.inward_operator_index),
                    indexOfFirstTensor(self.inward_state_conjugate_index),
                ]
            )
        )
    # }}}

    def absorbCounterClockwiseSideBoundary(self,side): # {{{
        return type(self)(
            contractAndTransposeAndJoin(
                (self.data,side.data,),
                ([self.counterclockwise_index],[side.clockwise_index]),
                new_grouped_order=[
                    [indexOfFirstTensor(self.clockwise_index)],
                    [indexOfSecondTensor(side.counterclockwise_index)],
                    [indexOfFirstTensor(self.inward_state_index),indexOfSecondTensor(side.inward_state_index)],
                    [indexOfFirstTensor(self.inward_operator_index),indexOfSecondTensor(side.inward_operator_index)],
                    [indexOfFirstTensor(self.inward_state_conjugate_index),indexOfSecondTensor(side.inward_state_conjugate_index)],
                ]
            )
        )
    # }}}
# }}}

class NormalizationSideBoundary(Tensor): # {{{
    dimension_names = ["clockwise","counterclockwise","inward","inward_conjugate"]

    def absorbCounterClockwiseCornerBoundary(self,corner): # {{{
        return type(self)(
            contractAndTranspose(
                (self.data,corner.data),
                ([self.counterclockwise_index],[corner.clockwise_index]),
                [
                    indexOfFirstTensor(self.clockwise_index),
                    indexOfSecondTensor(corner.counterclockwise_index),
                    indexOfFirstTensor(self.inward_index),
                    indexOfFirstTensor(self.inward_conjugate_index),
                ]
            )
        )
    # }}}

    def absorbCounterClockwiseSideBoundary(self,side): # {{{
        return type(self)(
            contractAndTransposeAndJoin(
                (self.data,side.data,),
                ([self.counterclockwise_index],[side.clockwise_index]),
                [
                    [indexOfFirstTensor(self.clockwise_index)],
                    [indexOfSecondTensor(side.counterclockwise_index)],
                    [indexOfFirstTensor(self.inward_index), indexOfSecondTensor(side.inward_index)],
                    [indexOfFirstTensor(self.inward_conjugate_index), indexOfSecondTensor(side.inward_conjugate_index)],
                ]
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
            contractAndTransposeAndJoin(
                (self.data,side.data),
                ([self.clockwise_index],[side.counterclockwise_index]),
                [
                    [indexOfFirstTensor(self.physical_index),indexOfSecondTensor(side.physical_index)],
                    [indexOfFirstTensor(self.physical_conjugate_index),indexOfSecondTensor(side.physical_conjugate_index)],
                    [indexOfSecondTensor(side.clockwise_index)],
                    [indexOfFirstTensor(self.counterclockwise_index),indexOfSecondTensor(side.inward_index)],
                ]
            )
        )
    # }}}

    def absorbSideSiteAtCounterClockwise(self,side): # {{{
        return type(self)(
            contractAndTransposeAndJoin(
                (self.data,side.data),
                ([self.counterclockwise_index],[side.clockwise_index]),
                [
                    [indexOfFirstTensor(self.physical_index),indexOfSecondTensor(side.physical_index)],
                    [indexOfFirstTensor(self.physical_conjugate_index),indexOfSecondTensor(side.physical_conjugate_index)],
                    [indexOfFirstTensor(self.clockwise_index),indexOfSecondTensor(side.inward_index)],
                    [indexOfSecondTensor(side.counterclockwise_index)],
                ]
            )
        )
    # }}}

    def formExpectationBoundary(self,state): # {{{
        return CornerBoundary(
            contractAndTransposeAndJoin(
                (
                    contractAndTransposeAndJoin(
                        (state.data,self.data),
                        ([state.physical_index],[self.physical_index]),
                        [
                            [indexOfSecondTensor(self.physical_conjugate_index)],
                            [indexOfFirstTensor(state.clockwise_index),indexOfSecondTensor(self.clockwise_index)],
                            [indexOfFirstTensor(state.counterclockwise_index),indexOfSecondTensor(self.counterclockwise_index)],
                        ]
                    ),
                    state.data.conj(),
                ),
                ([0],[state.physical_index]),
                [
                    [indexOfFirstTensor(1),indexOfSecondTensor(state.clockwise_index)],
                    [indexOfFirstTensor(2),indexOfSecondTensor(state.counterclockwise_index)],
                ],
            )
        )
    # }}}
# }}}

class OperatorSideSite(SideSiteTensor): # {{{
    dimension_names = ["physical","physical_conjugate","clockwise","counterclockwise","inward"]
    _center_site_class = OperatorCenterSite

    def formExpectationBoundary(self,state): # {{{
        return ExpectationSideBoundary(
            contractAndTransposeAndJoin(
                (
                    contractAndTransposeAndJoin(
                        (state.data,self.data),
                        ([state.physical_index],[self.physical_index]),
                        [
                            [indexOfSecondTensor(self.physical_conjugate_index)],
                            [indexOfFirstTensor(state.clockwise_index),indexOfSecondTensor(self.clockwise_index)],
                            [indexOfFirstTensor(state.counterclockwise_index),indexOfSecondTensor(self.counterclockwise_index)],
                            [indexOfFirstTensor(state.inward_index)],
                            [indexOfSecondTensor(self.inward_index)],
                        ]
                    ),
                    state.data.conj(),
                ),
                ([0],[state.physical_index]),
                [
                    [indexOfFirstTensor(1),indexOfSecondTensor(state.clockwise_index)],
                    [indexOfFirstTensor(2),indexOfSecondTensor(state.counterclockwise_index)],
                    [indexOfFirstTensor(3)],
                    [indexOfFirstTensor(4)],
                    [indexOfSecondTensor(state.inward_index)],
                ],
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
             contractAndTransposeAndJoin(
                (self.data,side.data),
                ([self.clockwise_index],[side.counterclockwise_index]),
                [
                    [indexOfFirstTensor(self.physical_index),indexOfSecondTensor(side.physical_index)],
                    [indexOfSecondTensor(side.clockwise_index)],
                    [indexOfFirstTensor(self.counterclockwise_index),indexOfSecondTensor(side.inward_index),]
                ]
             )
        )
    # }}}

    def absorbSideSiteAtCounterClockwise(self,side): # {{{
        return type(self)(
             contractAndTransposeAndJoin(
                (self.data,side.data),
                ([self.counterclockwise_index],[side.clockwise_index]),
                [
                    [indexOfFirstTensor(self.physical_index),indexOfSecondTensor(side.physical_index)],
                    [indexOfFirstTensor(self.clockwise_index),indexOfSecondTensor(side.inward_index)],
                    [indexOfSecondTensor(side.counterclockwise_index)],
                ]
             )
        )
    # }}}

    def formNormalizationBoundary(self): # {{{
        return CornerBoundary(
             contractAndTransposeAndJoin(
                (self.data,self.data.conj()),
                ([self.physical_index],)*2,
                [
                    [indexOfFirstTensor(self.clockwise_index),indexOfSecondTensor(self.clockwise_index)],
                    [indexOfFirstTensor(self.counterclockwise_index),indexOfSecondTensor(self.counterclockwise_index)],
                ],
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
            contractAndTransposeAndJoin(
                (self.data,self.data.conj()),
                ([self.physical_index],)*2,
                [
                    [indexOfFirstTensor(self.clockwise_index),indexOfSecondTensor(self.clockwise_index)],
                    [indexOfFirstTensor(self.counterclockwise_index),indexOfSecondTensor(self.counterclockwise_index)],
                    [indexOfFirstTensor(self.inward_index)],
                    [indexOfSecondTensor(self.inward_index)],
                ]
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
