#@+leo-ver=5-thin
#@+node:gcross.20111009193003.1158: * @file tensors.py
#@+<< Imports >>
#@+node:gcross.20111009193003.1159: ** << Imports >>
from numpy import complex128, inner, ndarray, tensordot

from .utils import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111016195932.1248: ** Classes
#@+node:gcross.20111103110300.1333: *3* Metaclasses
#@+node:gcross.20111016195932.1249: *4* MetaTensor
class MetaTensor(type):
    #@+others
    #@+node:gcross.20111009193003.1173: *5* __init__
    def __new__(cls,class_name,bases,data):
        conjugate_suffix = "_conjugate"
        conjugate_suffix_length = len(conjugate_suffix)
        if "_dimensions" in data:
            dimension_arguments = []
            conjugated_dimension_arguments = []
            for (index,name) in enumerate(data["_dimensions"]):
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
            data["number_of_dimensions"] = len(data["_dimensions"])
        return type.__new__(cls,class_name,bases,data)
    #@-others
#@+node:gcross.20111103110300.1336: *4* MetaSiteTensor
class MetaSiteTensor(MetaTensor):
    #@+others
    #@+node:gcross.20111103110300.1337: *5* __init__
    def __new__(cls,class_name,bases,data):
        if "_dimensions" in data:
            physical_indices = []
            physical_dimensions = []
            bandwidth_indices = []
            bandwidth_dimensions = []
            for (index,name) in enumerate(data["_dimensions"]):
                if name.startswith("physical"):
                    physical_indices.append(index)
                    physical_dimensions.append(name)
                else:
                    bandwidth_indices.append(index)
                    bandwidth_dimensions.append(name)
            data["_physical_indices"] = physical_indices
            data["_physical_dimensions"] = physical_dimensions
            data["_bandwidth_indices"] = bandwidth_indices
            data["_bandwidth_dimensions"] = bandwidth_dimensions
        return MetaTensor.__new__(cls,class_name,bases,data)
    #@-others
#@+node:gcross.20111103110300.1338: *3* Base classes
#@+node:gcross.20111009193003.1162: *4* Tensor
class Tensor(metaclass=MetaTensor):
    #@+others
    #@+node:gcross.20111009193003.1163: *5* __init__
    def __init__(self,*args,**keywords):
        if not ((len(args) > 0) ^ (len(keywords) > 0)):
            raise ValueError("constructor must be given either a reference to the data or the dimensions of the tensor")
        if len(args) == 1:
            self.data = args[0]
            if(self.data.ndim != self.number_of_dimensions):
                raise ValueError("constructor was given a reference to a tensor of rank {}, when a tensor of rank {} was required".format(self.data.ndim,self.number_of_dimensions))
            for (name,dimension) in zip(self._dimension_arguments,self.data.shape):
                setattr(self,name,dimension)
        else:
            randomize = False
            fill = None
            for given_dimension_name in keywords.keys():
                if given_dimension_name == "randomize":
                    randomize = keywords["randomize"]
                elif given_dimension_name == "fill":
                    fill = keywords["fill"]
                elif given_dimension_name not in self._dimension_arguments:
                    if given_dimension_name in self._dimensions:
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
            if randomize and fill:
                raise ValueError("you asked to fill the tensor both with random data *and* a given constant, which are contradictory requests")
            if randomize:
                self.data = crand(*shape)
            else:
                self.data = ndarray(shape,dtype=complex128)
            if fill:
                self.data[...] = fill
    #@+node:gcross.20111009193003.5259: *5* trivial
    @classmethod
    def trivial(cls):
        keywords = {name: 1 for name in cls._dimension_arguments}
        keywords["fill"] = 1
        return cls(**keywords)
    #@-others
#@+node:gcross.20111103110300.1345: *4* SiteTensor
class SiteTensor(Tensor,metaclass=MetaSiteTensor):
    #@+others
    #@+node:gcross.20111014113710.1232: *5* bandwidthDimension
    def bandwidthDimension(self,direction):
        return self.data.shape[self.bandwidthIndex(direction)]
    #@+node:gcross.20111014113710.1233: *5* bandwidthDimensions
    def bandwidthDimensions(self):
        return [self.data.shape[i] for i in self.bandwidthIndices()]
    #@+node:gcross.20111103110300.1349: *5* bandwidthIndex
    @classmethod
    def bandwidthIndex(self,direction):
        return self._bandwidth_indices[direction]
    #@+node:gcross.20111103110300.1351: *5* bandwidthIndices
    @classmethod
    def bandwidthIndices(self):
        return self._bandwidth_indices
    #@+node:gcross.20111103110300.1346: *5* physicalDimension
    def physicalDimension(self,direction):
        return self.data.shape[self.physicalIndex(direction)]
    #@+node:gcross.20111103110300.1348: *5* physicalDimensions
    def physicalDimensions(self):
        return [self.data.shape[i] for i in self.physicalIndices()]
    #@+node:gcross.20111103110300.1353: *5* physicalIndex
    @classmethod
    def physicalIndex(self,direction):
        return self._physical_indices[direction]
    #@+node:gcross.20111103110300.1355: *5* physicalIndices
    @classmethod
    def physicalIndices(self):
        return self._physical_indices
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111009193003.1171: ** << Exports >>
__all__ = [
    "MetaTensor",
    "MetaSiteTensor",

    "Tensor",
    "SiteTensor",
]
#@-<< Exports >>
#@-leo
