# Imports {{{
from numpy import array, complex128, multiply, ndarray, product, zeros

from .utils import basisVector, crand
# }}}

# Metaclasses {{{ 

class MetaTensor(type): # {{{
    def __new__(cls,class_name,bases,data):
        conjugate_suffix = "_conjugate"
        conjugate_suffix_length = len(conjugate_suffix)
        if "dimension_names" in data:
            dimension_arguments = []
            conjugated_dimension_arguments = []
            for (index,name) in enumerate(data["dimension_names"]):
                index_name = name + "_index"
                if index_name in data:
                    raise ValueError("repeated dimension name '{}'".format(name))
                else:
                    data[index_name] = index
                dimension_arguments.append(name + "_dimension")
                if name.endswith(conjugate_suffix):
                    conjugated_dimension_arguments.append((name[:-conjugate_suffix_length]+"_dimension",name+"_dimension"))
            data["_dimension_arguments"] = dimension_arguments
            data["_conjugated_dimension_arguments"] = conjugated_dimension_arguments
            data["number_of_dimensions"] = len(data["dimension_names"])
        return type.__new__(cls,class_name,bases,data)
# }}}

class MetaSiteTensor(MetaTensor): # {{{
    def __new__(cls,class_name,bases,data):
        if "dimension_names" in data:
            physical_indices = []
            physical_dimensions = []
            bandwidth_indices = []
            bandwidth_dimensions = []
            dimensions = data["dimension_names"]
            for (index,name) in enumerate(dimensions):
                if name.startswith("physical"):
                    physical_indices.append(index)
                    physical_dimensions.append(name)
                else:
                    bandwidth_indices.append(index)
                    bandwidth_dimensions.append(name)
            data["physical_dimension_indices"] = physical_indices
            data["physical_dimension_names"] = physical_dimensions
            data["bandwidth_dimension_indices"] = bandwidth_indices
            data["bandwidth_dimension_names"] = bandwidth_dimensions
            data["number_of_physical_dimensions"] = len(physical_dimensions)
            data["number_of_bandwidth_dimensions"] = len(bandwidth_dimensions)
            data["physical_indices_are_transposed"] = (
                "physical" in dimensions and
                "physical_conjugate" in dimensions and
                dimensions.index("physical") < dimensions.index("physical_conjugate")
            )
        return MetaTensor.__new__(cls,class_name,bases,data)
# }}}

#}}}

# Base Classes {{{

class Tensor:# {{{
    __metaclass__ = MetaTensor
    
    def __init__(self,*args,**keywords): # {{{
        if not ((len(args) == 1) ^ (len(keywords) > 0)):
            raise ValueError("constructor must be given either a reference to the data or the dimensions of the tensor")
        if len(args) == 1:
            self.data = args[0]
            if(self.data.ndim != self.number_of_dimensions):
                raise ValueError("constructor was given a reference to a tensor of rank {}, when a tensor of rank {} was required".format(self.data.ndim,self.number_of_dimensions))
            for (name,dimension) in zip(self._dimension_arguments,self.dimensions()):
                setattr(self,name,dimension)
        else:
            for given_dimension_name in keywords.keys():
                if given_dimension_name not in self._dimension_arguments:
                    if given_dimension_name in self.dimension_names:
                        raise ValueError("you needed to type '{}_dimension' rather than '{}' in the list of keyword arguments to supply the {} dimension".format(*(given_dimension_name,)*3))
                    else:
                        raise ValueError("{} is not a recognized dimension for this tensor".format(given_dimension_name))
            shape = []
            for (name,conjugated_name) in self._conjugated_dimension_arguments:
                if name in keywords:
                    keywords[conjugated_name] = keywords[name]
            for name in self._dimension_arguments:
                try:
                    dimension = keywords[name]
                    shape.append(dimension)
                    setattr(self,name,dimension)
                except KeyError:
                    raise ValueError("missing a value for dimension {}".format(name))
            self.data = ndarray(shape,dtype=complex128)
    # }}}
    
    def dimension(self,index): # {{{
        return self.dimensions()[index]
    # }}}
    
    def dimensions(self): # {{{
        return self.data.shape
    # }}}
    
    @classmethod
    def filled(cls,fill,**keywords): # {{{
        self = cls(**keywords)
        self.data[...] = fill
        return self
    # }}}
    
    @classmethod
    def formFromOuterProduct(cls,**keywords): # {{{
        for name in cls.dimension_names:
            if name.endswith("_conjugate"):
                if name not in keywords:
                    keywords[name] = keywords[name[:-len("_conjugate")]].conj()
        data = array(1,dtype=complex128)
        indices = {}
        for index, (name, vector) in enumerate(keywords.items()):
            data = multiply.outer(data,vector)
            indices[name] = index
        data.transpose([indices[name] for name in cls.dimension_names])
        return cls(data)
    # }}}
    
    @classmethod
    def indexForName(cls,name): # {{{
        return cls.dimension_names.index(name)
    # }}}
    
    @classmethod
    def random(cls,**keywords): # {{{
        self = cls(**keywords)
        self.data[...] = crand(*self.dimensions())
        return self
    # }}}
    
    def size(self): # {{{
        return product([getattr(self,name + "_dimension") for name in self.dimension_names])
    # }}}
    
    @classmethod
    def trivial(cls): # {{{
        return cls.filled(1,**{name: 1 for name in cls._dimension_arguments})
    # }}}

# }}}

class SiteTensor(Tensor): # {{{
    __metaclass__ = MetaSiteTensor

    def bandwidthDimension(self,direction): # {{{
        return self.dimension(self.bandwidthIndex(direction))
    # }}}

    def bandwidthDimensions(self): # {{{
        return [self.dimension(i) for i in self.bandwidthIndices()]
    # }}}

    @classmethod
    def bandwidthIndex(self,direction): # {{{
        return self.bandwidth_dimension_indices[direction]
    # }}}

    @classmethod
    def bandwidthIndexForName(cls,name): # {{{
        return cls.bandwidthIndex(cls.bandwidth_dimension_names.index(name))
    # }}}

    @classmethod
    def bandwidthIndices(self): # {{{
        return self.bandwidth_dimension_indices
    # }}}

    @classmethod
    def build(cls,bandwidth_dimensions,components): # {{{
        first_component_value = components[0][1]
        selector = [None]*cls.number_of_dimensions
        selector_indices = [cls.bandwidthIndexForName(name) for (name,_) in bandwidth_dimensions]
        try:
            physical_dimension = first_component_value.shape[0]
            for i in xrange(cls.number_of_physical_dimensions):
                selector[cls.physicalIndex(i)] = slice(0,physical_dimension)
            self = cls.filled(0,physical_dimension=physical_dimension,**dict((name + "_dimension",dimension) for (name,dimension) in bandwidth_dimensions))
        except AttributeError:
            self = cls.filled(0,**dict((name + "_dimension",dimension) for (name,dimension) in bandwidth_dimensions))
        data = self.data
        for indices, component_value in components:
            for selector_index, index in zip(selector_indices,indices):
                selector[selector_index] = index
            data[tuple(selector)] += component_value if not cls.physical_indices_are_transposed else component_value.transpose()
        return self
    # }}}

    def physicalDimension(self,direction): # {{{
        return self.dimension(self.physicalIndex(direction))
    # }}}

    def physicalDimensions(self): # {{{
        return [self.dimension(i) for i in self.physicalIndices()]
    # }}}

    @classmethod
    def physicalIndex(self,direction): # {{{
        return self.physical_dimension_indices[direction]
    # }}}

    @classmethod
    def physicalIndices(self): # {{{
        return self.physical_dimension_indices
    # }}}

    @classmethod
    def simple(cls,component): # {{{
        return cls.build([(name,1) for name in cls.bandwidth_dimension_names],[((0,) * cls.number_of_bandwidth_dimensions,component)])
    # }}}

# }}}

class StateSiteTensor(SiteTensor): # {{{
    """Hello, world"""
    @classmethod
    def simpleObservation(cls,physical_dimension,observation):
        return cls.simple(basisVector(physical_dimension,observation))
# }}}

# }}}

# Exports {{{
__all__ = [
    "MetaTensor",
    "MetaSiteTensor",

    "Tensor",
    "SiteTensor",
    "StateSiteTensor",
]
# }}}
