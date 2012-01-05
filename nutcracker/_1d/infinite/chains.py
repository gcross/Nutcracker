#@+leo-ver=5-thin
#@+node:gcross.20111109104457.1830: * @file chains.py
#@+<< Imports >>
#@+node:gcross.20111109104457.1832: ** << Imports >>
from numpy import array, complex128, identity

from ...lattices import OperatorLattice, StateLattice
from ..tensors import OperatorSite, StateSite
#@-<< Imports >>

#@+others
#@+node:gcross.20111109104457.1834: ** Classes
#@+node:gcross.20111109104457.1835: *3* Chain
class Chain(object):
    #@+others
    #@+node:gcross.20111109104457.1837: *4* __init__
    def __init__(self,left_boundary_vector,right_boundary_vector,site):
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
    #@+node:gcross.20111109104457.1879: *4* build
    @classmethod
    def build(cls,left_boundary_vector,right_boundary_vector,components):
        return cls(
            left_boundary_vector,
            right_boundary_vector,
            cls._site_class.build(
                [("left",len(left_boundary_vector)),("right",len(right_boundary_vector))],
                components=components
            ),
        )
    #@+node:gcross.20111110144828.1735: *4* buildAllSitesSameButOne
    @classmethod
    def buildAllSitesSameButOne(cls,same_site_value,different_site_value):
        return cls.build(
            [1,0],
            [0,1],
            [((0,0),same_site_value)
            ,((0,1),different_site_value)
            ,((1,1),same_site_value)
            ],
        )
    #@+node:gcross.20111110233742.1813: *4* buildAllSitesSameButTwoNeighbors
    @classmethod
    def buildAllSitesSameButTwoNeighbors(cls,same_site_value,different_site_value_1,different_site_value_2):
        return cls.build(
            [1,0,0],
            [0,0,1],
            [((0,0),same_site_value)
            ,((0,1),different_site_value_1)
            ,((1,2),different_site_value_2)
            ,((2,2),same_site_value)
            ],
        )
    #@+node:gcross.20111109104457.1881: *4* simple
    @classmethod
    def simple(cls,component_value):
        return cls([1],[1],cls._site_class.simple(component_value))
    #@+node:gcross.20111109104457.1838: *4* trivial
    @classmethod
    def trivial(cls):
        return cls([1],[1],cls._site_class.trivial())
    #@-others
#@+node:gcross.20111109104457.1875: *3* OperatorChain
class OperatorChain(OperatorLattice,Chain):
    _site_class = OperatorSite
    #@+others
    #@+node:gcross.20111110233742.1814: *4* buildNearestNeighborSpinCouplingField
    @classmethod
    def buildNearestNeighborSpinCouplingField(cls,field_operator,anti=False):
        return cls.buildAllSitesSameButTwoNeighbors(identity(len(field_operator)),field_operator,field_operator if anti else -field_operator)
    #@-others
#@+node:gcross.20111109104457.1876: *3* StateChain
class StateChain(StateLattice,Chain):
    _site_class = StateSite
    #@+others
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111109104457.1833: ** << Exports >>
__all__ = [
    "Chain",

    "OperatorChain",
    "StateChain",
]
#@-<< Exports >>
#@-leo
