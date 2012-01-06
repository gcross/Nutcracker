# Imports {{{
from numpy import array, complex128, identity

from ...lattices import OperatorLattice, StateLattice
from ..tensors import OperatorSite, StateSite
# }}}

class Chain(object): # {{{
    def __init__(self,left_boundary_vector,right_boundary_vector,site): # {{{
        if site.left_dimension != site.right_dimension:
            raise ValueError("the left and right dimensions of the site tensor must match in an infinite chain ({} != {})".format(site.left_dimension,site.right_dimension))
        if len(left_boundary_vector) != site.left_dimension:
            raise ValueError("the length of the left boundary vector does not match the left dimension of the site tensor ({} != {})".format(len(left_boundary_vector),site.left_dimension))
        if len(right_boundary_vector) != site.right_dimension:
            raise ValueError("the length of the right boundary vector does not match the right dimension of the site tensor ({} != {})".format(len(right_operator_boundary),site.right_dimension))
        self.site = site
        self.left_boundary_vector = array(left_boundary_vector,dtype=complex128)
        self.right_boundary_vector = array(right_boundary_vector,dtype=complex128)
        self.bandwidth_dimension = site.left_dimension
        self.physical_dimension = site.physical_dimension
    # }}}

    @classmethod
    def build(cls,left_boundary_vector,right_boundary_vector,components): # {{{
        return cls(
            left_boundary_vector,
            right_boundary_vector,
            cls._site_class.build(
                [("left",len(left_boundary_vector)),("right",len(right_boundary_vector))],
                components=components
            ),
        )
    # }}}

    @classmethod
    def buildAllSitesSameButOne(cls,same_site_value,different_site_value): # {{{
        return cls.build(
            [1,0],
            [0,1],
            [((0,0),same_site_value)
            ,((0,1),different_site_value)
            ,((1,1),same_site_value)
            ],
        )
    # }}}

    @classmethod
    def buildAllSitesSameButTwoNeighbors(cls,same_site_value,different_site_value_1,different_site_value_2): # {{{
        return cls.build(
            [1,0,0],
            [0,0,1],
            [((0,0),same_site_value)
            ,((0,1),different_site_value_1)
            ,((1,2),different_site_value_2)
            ,((2,2),same_site_value)
            ],
        )
    # }}}

    @classmethod
    def simple(cls,component_value): # {{{
        return cls([1],[1],cls._site_class.simple(component_value))
    # }}}

    @classmethod
    def trivial(cls): # {{{
        return cls([1],[1],cls._site_class.trivial())
    # }}}
# }}}

class OperatorChain(OperatorLattice,Chain): # {{{
    _site_class = OperatorSite
    @classmethod
    def buildNearestNeighborSpinCouplingField(cls,field_operator,anti=False):
        return cls.buildAllSitesSameButTwoNeighbors(identity(len(field_operator)),field_operator,field_operator if anti else -field_operator)
# }}}

class StateChain(StateLattice,Chain): # {{{
    _site_class = StateSite
# }}}

# Exports {{{
__all__ = [
    "Chain",

    "OperatorChain",
    "StateChain",
]
# }}}
