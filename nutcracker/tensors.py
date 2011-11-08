#@+leo-ver=5-thin
#@+node:gcross.20111107131531.1288: * @file tensors.py
#@+<< License >>
#@+node:gcross.20111107131531.1290: ** << License >>
#@+at
# Copyright (c) 2011, Gregory Crosswhite
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#@@c
#@-<< License >>

#@+<< Imports >>
#@+node:gcross.20111107131531.1304: ** << Imports >>
from numpy import complex128, ndarray, product

from .utils import crand
#@-<< Imports >>

#@+others
#@+node:gcross.20111107131531.1291: ** Classes
#@+node:gcross.20111107131531.1292: *3* Metaclasses
#@+node:gcross.20111107131531.1295: *4* MetaTensor
class MetaTensor(type):
    #@+others
    #@+node:gcross.20111107131531.1296: *5* __init__
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
#@+node:gcross.20111107131531.1310: *4* MetaSiteTensor
class MetaSiteTensor(MetaTensor):
    #@+others
    #@+node:gcross.20111107131531.1311: *5* __init__
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
#@+node:gcross.20111107131531.1297: *3* Base classes
#@+node:gcross.20111107131531.1301: *4* Tensor
class Tensor:
    __metaclass__ = MetaTensor
    #@+others
    #@+node:gcross.20111107131531.1302: *5* __init__
    def __init__(self,*args,**keywords):
        if not ((len(args) == 1) ^ (len(keywords) > 0)):
            raise ValueError("constructor must be given either a reference to the data or the dimensions of the tensor")
        if len(args) == 1:
            self.data = args[0]
            if(self.data.ndim != self.number_of_dimensions):
                raise ValueError("constructor was given a reference to a tensor of rank {}, when a tensor of rank {} was required".format(self.data.ndim,self.number_of_dimensions))
            for (name,dimension) in zip(self._dimension_arguments,self.data.shape):
                setattr(self,name,dimension)
        else:
            for given_dimension_name in keywords.keys():
                if given_dimension_name not in self._dimension_arguments:
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
            self.data = ndarray(shape,dtype=complex128)
    #@+node:gcross.20111108100704.1378: *5* filled
    @classmethod
    def filled(cls,fill,**keywords):
        self = cls(**keywords)
        self.data[...] = fill
        return self
    #@+node:gcross.20111108100704.1377: *5* random
    @classmethod
    def random(cls,**keywords):
        self = cls(**keywords)
        self.data[...] = crand(*self.data.shape)
        return self
    #@+node:gcross.20111107131531.1303: *5* trivial
    @classmethod
    def trivial(cls):
        keywords = {name: 1 for name in cls._dimension_arguments}
        keywords["fill"] = 1
        return cls(**keywords)
    #@+node:gcross.20111108100704.1446: *5* size
    def size(self):
        return product([getattr(self,name + "_dimension") for name in self._dimensions])
    #@-others
#@+node:gcross.20111107131531.1321: *4* SiteTensor
class SiteTensor(Tensor):
    __metaclass__ = MetaSiteTensor
    #@+others
    #@+node:gcross.20111107131531.1322: *5* bandwidthDimension
    def bandwidthDimension(self,direction):
        return self.data.shape[self.bandwidthIndex(direction)]
    #@+node:gcross.20111107131531.1323: *5* bandwidthDimensions
    def bandwidthDimensions(self):
        return [self.data.shape[i] for i in self.bandwidthIndices()]
    #@+node:gcross.20111107131531.1324: *5* bandwidthIndex
    @classmethod
    def bandwidthIndex(self,direction):
        return self._bandwidth_indices[direction]
    #@+node:gcross.20111107131531.1325: *5* bandwidthIndices
    @classmethod
    def bandwidthIndices(self):
        return self._bandwidth_indices
    #@+node:gcross.20111107131531.1326: *5* physicalDimension
    def physicalDimension(self,direction):
        return self.data.shape[self.physicalIndex(direction)]
    #@+node:gcross.20111107131531.1327: *5* physicalDimensions
    def physicalDimensions(self):
        return [self.data.shape[i] for i in self.physicalIndices()]
    #@+node:gcross.20111107131531.1328: *5* physicalIndex
    @classmethod
    def physicalIndex(self,direction):
        return self._physical_indices[direction]
    #@+node:gcross.20111107131531.1329: *5* physicalIndices
    @classmethod
    def physicalIndices(self):
        return self._physical_indices
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111107131531.1305: ** << Exports >>
__all__ = [
    "MetaTensor",
    "MetaSiteTensor",

    "Tensor",
    "SiteTensor",
]
#@-<< Exports >>
#@-leo
