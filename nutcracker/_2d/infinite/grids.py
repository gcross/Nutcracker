# Imports {{{
from numpy import array, complex128
import random

from ...lattices import OperatorLattice, StateLattice
from ...utils import CCW
from ..tensors import OperatorCenterSite, OperatorCornerSite, OperatorSideSite, StateCenterSite, StateCornerSite, StateSideSite
# }}}

# Metaclasses {{{

class MetaGrid(type): # {{{
    def __new__(cls,class_name,bases,data):
        if "_site_class" in data:
            site_class = data["_site_class"]
            class BoundaryProperty(property):
                def __init__(self,bandwidth_index):
                    property.__init__(self,self.get,self.set)
                    self.bandwidth_index = bandwidth_index
                def get(self,obj):
                    return obj.boundary_vectors[self.bandwidth_index]
                def set(self,obj,new_vector):
                    obj.boundary_vectors[self.bandwidth_index] = new_vector
            for i, name in enumerate(site_class.bandwidth_dimension_names):
                data[name + "_boundary_vector"] = BoundaryProperty(i)
        return type.__new__(cls,class_name,bases,data)
# }}}

# }}}

# Classes {{{

class Grid(object): # {{{
    __metaclass__ = MetaGrid

    def __init__(self,leftward_boundary_vector,upward_boundary_vector,rightward_boundary_vector,downward_boundary_vector,site): # {{{
        if site.leftward_dimension != site.rightward_dimension:
            raise ValueError("the leftward and rightward dimensions of the site tensor must match in an infinite grid ({} != {})".format(site.leftward_dimension,site.rightward_dimension))
        if site.upward_dimension != site.downward_dimension:
            raise ValueError("the upward and downward dimensions of the site tensor must match in an infinite grid ({} != {})".format(site.upward_dimension,site.downward_dimension))
        boundary_vector_map = dict(
            leftward = leftward_boundary_vector,
            upward = upward_boundary_vector,
            rightward = rightward_boundary_vector,
            downward = downward_boundary_vector,
        )
        boundary_vectors = [boundary_vector_map[name] for name in site.bandwidth_dimension_names]
        for i, d in enumerate(len(x) for x in boundary_vectors):
            if d != site.bandwidthDimension(i):
                name = site.bandwidth_dimension_names[i]
                raise ValueError("the length of the {} boundary vector does not match the {} dimension of the site tensor ({} != {})".format(name,name,d,site.bandwidthDimension(i)))
        self.center = site
        self.horizontal_bandwidth_dimension = site.leftward_dimension
        self.vertical_bandwidth_dimension = site.upward_dimension
        self.physical_dimension = site.physical_dimension
        self.boundary_vectors = boundary_vectors
    # }}}

    @classmethod
    def build(cls,boundary_vectors_with_names,components): # {{{
        return cls(
            site=cls._site_class.build(
                [(name,len(vector)) for (name,vector) in boundary_vectors_with_names],
                components=components
            ),
            **dict((name + "_boundary_vector",vector) for (name,vector) in boundary_vectors_with_names)
        )
    # }}}

    @classmethod
    def simple(cls,component_value): # {{{
        return cls([1],[1],[1],[1],cls._site_class.simple(component_value))
    # }}}

    @classmethod
    def trivial(cls): # {{{
        return cls([1],[1],[1],[1],cls._site_class.trivial())
    # }}}
# }}}

class OperatorGrid(OperatorLattice,Grid): # {{{
    _site_class = OperatorCenterSite
# }}}

class StateGrid(StateLattice,Grid): # {{{
    _site_class = StateCenterSite
# }}}

# }}}

# Exports {{{
__all__ = [
    "Grid",

    "OperatorGrid",
    "StateGrid",
]
# }}}
