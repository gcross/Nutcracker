# Imports {{{
from collections import defaultdict
from copy import copy
from itertools import izip
from numpy import allclose, array, ones, prod, tensordot, zeros
from random import randint

from .utils import applyPermutation, crand, randomComplexSample
# }}}

# Base classes {{{

class Data(object): # {{{
  # Instance methods {{{
    def toNDArrayData(self):
        return NDArrayData(self.toArray())
  # }}}
# }}}

# }}}

# Classes {{{
class NDArrayData(Data): # {{{

  # Class construction methods {{{

    def __init__(self,_arr): # {{{
        self._arr = _arr
    # }}}

    @classmethod # newFilled {{{
    def newFilled(cls,shape,value,dtype=None):
        if not dtype:
            dtype = typeof(value)
        _arr = nd_array(shape,dtype)
        _arr[...] = value
        return NDArrayData(_arr)
    # }}}

    @classmethod # newOuterProduct {{{
    def newOuterProduct(cls,factors):
        return cls(reduce(multiply.outer,factors))
    # }}}

    @classmethod # newRandom {{{
    def newRandom(cls,shape):
        return cls(randomComplexSample(shape))
    # }}}

    @classmethod # newTrivial {{{
    def newTrivial(cls,shape,dtype=int):
        return cls(ones(shape,dtype=dtype))
    # }}}

  # }}}

  # Instance methods {{{

    def __iadd__(self,other): # {{{
        self._arr += other._arr
        return self
    # }}}

    def __getitem__(self,index): # {{{
        return NDArrayData(self._arr[index])
    # }}}

    def __setitem__(self,index,value): # {{{
        self._arr[index] = value._arr
    # }}}

    def toArray(self):  #{{{
        return self._arr
    # }}}

    def allcloseTo(self,other,rtol=1e-05,atol=1e-08): # {{{
        return allclose(self._arr,other._arr,rtol=rtol,atol=atol)
    # }}}

    def conj(self): # {{{
        return self.__class__(self._arr.conj())
    # }}}

    def contractWith(self,other,self_axes,other_axes): # {{{
        return NDArrayData(tensordot(self._arr,other._arr,(self_axes,other_axes)))
    # }}}

    def extractScalar(self): # {{{
        if self.ndim != 0:
            raise ValueError("tensor is not a scalar")
        else:
            return self._arr
    # }}}

    def join(self,groups): #{{{
        _arr = self._arr.transpose([index for group in groups for index in group])
        shape = []
        index = 0
        for group in groups:
            shape.append(prod(_arr.shape[index:index+len(group)]))
            index += len(group)
        return NDArrayData(_arr.reshape(shape))
    # }}}

    def transpose(self,args): # {{{
        return NDArrayData(self._arr.transpose(*args))
    # }}}

  # }}}

  # Properties {{{

    ndim = property(fget = lambda self: self._arr.ndim)
    shape = property(fget = lambda self: self._arr.shape)

  # }}}

# }}}

class ScalarData(Data): # {{{
  # Class construction methods {{{
    def __init__(self,value): # {{{
        self.value = value
    # }}}
    @classmethod # newRandom {{{
    def newRandom(cls):
        return crand()
    # }}}
  # }}}
  # Instance methods {{{
    def __iadd__(self,other): # {{{
        assert isinstance(other,ScalarData)
        self.value += other.value
        return self
    # }}}
    def __repr__(self): # {{{
        return "ScalarData({})".format(self.value)
    # }}}
    def contractWith(self,other,self_sum_axes,other_sum_axes): # {{{
        if isinstance(other,ScalarData):
            assert self_sum_axes == []
            assert other_sum_axes == []
            return ScalarData(self.value*other.value)
        elif isinstance(other,NDArrayData):
            return NDArrayData(self.value*other.toArray())
        else:
            raise TypeError("contraction of ScalarData with {} not supported".format(other.__class__.__name__))
    # }}}
    def extractScalar(self): # {{{
        return self.value
    # }}}
    def join(self,grouping): # {{{
        assert grouping == []
        return copy(self)
    # }}}
    def toArray(self): # {{{
        return array(self.value)
    # }}}
    def transpose(self,transposition): # {{{
        assert transposition == [0]
        return copy(self)
    # }}}
  # }}}
  # Properties {{{
    ndim = property(lambda _: 0)
    shape = property(lambda _: ())
  # }}}
# }}}

class SparseData(Data): # {{{
  # Class construction methoods {{{
    def __init__(self,sparse_shape,index_to_data_map,nested_shape=None,transposition=None): # {{{
        if nested_shape is None:
            if not index_to_data_map:
                raise ValueError("must specify the nested shape in the arguments when index_to_data_map is empty")
            nested_shape = index_to_data_map.iteritems().next()[1].shape
        sparse_ndim = len(sparse_shape)
        nested_ndim = len(nested_shape)
        ndim = sparse_ndim + nested_ndim
        if transposition is None:
            transposition = range(ndim)
        elif len(transposition) != ndim:
            raise ValueError("the length of the transposition is inconsistent with the rank: len({}) = {} != {}".format(transposition,len(transposition),ndim))
        self.index_to_data_map = index_to_data_map
        self.sparse_shape = sparse_shape
        self.sparse_ndim = sparse_ndim
        self.nested_shape = nested_shape
        self.nested_ndim = nested_ndim
        self.ndim = ndim
        self.transposition = transposition
        self.shape = applyPermutation(transposition,sparse_shape + nested_shape)
    # }}}
    @classmethod # newRandom {{{
    def newRandom(cls,NestedClass,sparse_shape,nested_shape,number_of_term_bounds=(0,20)):
        index_to_data_map = {}
        for _ in xrange(randint(*number_of_term_bounds)):
            index = tuple(randint(0,dimension-1) for dimension in sparse_shape)
            data = NestedClass.newRandom(nested_shape)
            if index in index_to_data_map:
                index_to_data_map[index] += data
            else:
                index_to_data_map[index] = data
        return cls(sparse_shape,index_to_data_map,nested_shape)
    # }}}
  # }}}
  # Instance methods {{{
    def __iadd__(self,other): # {{{
        assert isinstance(other,SparseData)
        assert self.shape == other.shape
        assert self.transposition == other.transposition
        index_to_data_map = self.index_to_data_map
        for index, data in other.index_to_data_map.iteritems():
            if index in index_to_data_map:
                index_to_data_map[index] += data
            else:
                index_to_data_map[index] = data
        return self
    # }}}
    def contractWith(self,other,self_sum_axes,other_sum_axes): # {{{
        if isinstance(other,NDArrayData):
            return self.contractWith(SparseData(tuple(),{():other},other.shape),self_sum_axes,other_sum_axes)
        if len(self_sum_axes) != len(other_sum_axes):
            raise ValueError("the lists of axes to sum over must have the same length: len({}) = {} != len({}) = {}".format(self_sum_axes,len(self_sum_axes),other_sum_axes,len(other_sum_axes)))
        assert isinstance(other,SparseData)
      # Cache some instance fields into local variables {{{
        self_split_ndim = self.sparse_ndim
        self_transposition = self.transposition
        other_split_ndim = other.sparse_ndim
        other_transposition = other.transposition
      # }}}
      # Partition the axes {{{
        self_sparse_result_axes = set(xrange(self.sparse_ndim))
        self_nested_result_axes = set(xrange(self.nested_ndim))
        self_sparse_sum_axes = []
        self_nested_sum_axes = []

        other_sparse_result_axes = set(xrange(other.sparse_ndim))
        other_nested_result_axes = set(xrange(other.nested_ndim))
        other_sparse_sum_axes = []
        other_nested_sum_axes = []

        for self_sum_axis, other_sum_axis in izip(self_sum_axes,other_sum_axes):
            self_sum_axis = self_transposition[self_sum_axis]
            other_sum_axis = other_transposition[other_sum_axis]
            if self_sum_axis < self_split_ndim and other_sum_axis < other_split_ndim:
                self_sparse_result_axes.remove(self_sum_axis)
                self_sparse_sum_axes.append(self_sum_axis)
                other_sparse_result_axes.remove(other_sum_axis)
                other_sparse_sum_axes.append(other_sum_axis)
            elif self_sum_axis >= self_split_ndim and other_sum_axis >= other_split_ndim:
                self_sum_axis -= self_split_ndim
                self_nested_result_axes.remove(self_sum_axis)
                self_nested_sum_axes.append(self_sum_axis)
                other_sum_axis -= other_split_ndim
                other_nested_result_axes.remove(other_sum_axis)
                other_nested_sum_axes.append(other_sum_axis)
            elif self_sum_axis > self_split_ndim and other_sum_axis < other_split_ndim:
                self_sum_axis = self_transposition.index(self_sum_axis)
                other_sum_axis = other_transposition.index(other_sum_axis)
                raise ValueError("Attempt to connect nested axis {} to sparse axis {}".format(self_sum_axis,other_sum_axis))
            else: # self_sum_axis < self_split_ndim and other_sum_axis > other_split_ndim:
                self_sum_axis = self_transposition.index(self_sum_axis)
                other_sum_axis = other_transposition.index(other_sum_axis)
                raise ValueError("Attempt to connect sparse axis {} to nested axis {}".format(self_sum_axis,other_sum_axis))
      # }}}
      # Compute the result shapes {{{
        self_sparse_result_axes = list(sorted(self_sparse_result_axes))
        self_sparse_result_shape = tuple(self.sparse_shape[i] for i in self_sparse_result_axes)
        self_nested_result_axes = list(sorted(self_nested_result_axes))
        self_nested_result_shape = tuple(self.nested_shape[i] for i in self_nested_result_axes)
        other_sparse_result_axes = list(sorted(other_sparse_result_axes))
        other_sparse_result_shape = tuple(other.sparse_shape[i] for i in other_sparse_result_axes)
        other_nested_result_axes = list(sorted(other_nested_result_axes))
        other_nested_result_shape = tuple(other.nested_shape[i] for i in other_nested_result_axes)
        other_data_transposition = tuple(other_nested_sum_axes + other_nested_result_axes)
        number_of_sum_axes = len(self_nested_result_axes)
        result_sparse_shape = self_sparse_result_shape + other_sparse_result_shape
        result_nested_shape = self_nested_result_shape + other_nested_result_shape
      # }}}
      # Construct the map of terms which have overlapping sparse indices {{{
        overlapping_terms = defaultdict(lambda: ([],[]))
        for (index_to_data_map,sparse_sum_axes,sparse_result_axes,k) in [
                (self.index_to_data_map,self_sparse_sum_axes,self_sparse_result_axes,0),
                (other.index_to_data_map,other_sparse_sum_axes,other_sparse_result_axes,1),
            ]:
            for (index,data) in index_to_data_map.iteritems():
                sparse_sum_indices = tuple(index[i] for i in sparse_sum_axes)
                sparse_result_indices = tuple(index[i] for i in sparse_result_axes)
                overlapping_terms[sparse_sum_indices][k].append((sparse_result_indices,data))
      # }}}
      # Compute the result data {{{
        result_index_to_data_map = {}
        for (self_terms,other_terms) in overlapping_terms.itervalues():
            for (self_sparse_result_indices,self_data) in self_terms:
                for (other_sparse_result_indices,other_data) in other_terms:
                    result_indices = self_sparse_result_indices + other_sparse_result_indices
                    result_term = self_data.contractWith(other_data,self_nested_sum_axes,other_nested_sum_axes)
                    if result_indices in result_index_to_data_map:
                        result_index_to_data_map[result_indices] += result_term
                    else:
                        result_index_to_data_map[result_indices] = result_term
      # }}}
      # Construct the transposition of the result {{{
        self_result_axis_to_final_axis_map = {}
        other_result_axis_to_final_axis_map = {}
        next_final_axis = 0
        for axis in self_sparse_result_axes:
            self_result_axis_to_final_axis_map[axis] = next_final_axis
            next_final_axis += 1
        for axis in other_sparse_result_axes:
            other_result_axis_to_final_axis_map[axis] = next_final_axis
            next_final_axis += 1
        for axis in self_nested_result_axes:
            self_result_axis_to_final_axis_map[axis+self_split_ndim] = next_final_axis
            next_final_axis += 1
        for axis in other_nested_result_axes:
            other_result_axis_to_final_axis_map[axis+other_split_ndim] = next_final_axis
            next_final_axis += 1
        result_transposition = []
        for ndim, sum_axes, transposition, result_axis_to_final_axis_map in [
                (self.ndim,self_sum_axes,self_transposition,self_result_axis_to_final_axis_map),
                (other.ndim,other_sum_axes,other_transposition,other_result_axis_to_final_axis_map),
            ]:
            for axis in xrange(ndim):
                if axis not in sum_axes:
                    result_transposition.append(result_axis_to_final_axis_map[transposition[axis]])
      # }}}
      # Return the result {{{
        return SparseData(result_sparse_shape,result_index_to_data_map,result_nested_shape,result_transposition)
      # }}}
    # }}}
    def extractScalar(self): # {{{
        assert self.sparse_ndim == 0
        if self.index_to_data_map:
            return self.index_to_data_map[()].extractScalar()
        else:
            return 0
    # }}}
    def join(self,groupings): # {{{
      # Construct the separate groupings and the new shapes for the sparse and nested axes {{{
        sparse_ndim = self.sparse_ndim
        sparse_shape = self.sparse_shape
        nested_shape = self.nested_shape
        transposition = self.transposition
        sparse_groupings = []
        nested_groupings = []
        new_sparse_shape = []
        new_nested_shape = []
        group_classifications = []
        for group in groupings:
            if not group:
                raise ValueError("sparse data does not support empty grouping")
            if transposition[group[0]] < sparse_ndim:
                group_classifications.append(True)
                sparse_grouping = []
                dimension = 1
                for axis in reversed(group):
                    axis = transposition[axis]
                    if axis >= sparse_ndim:
                        raise ValueError("grouping {} mixes sparse and nested axes")
                    sparse_grouping.append((dimension,axis))
                    dimension *= sparse_shape[axis]
                sparse_groupings.append(tuple(sparse_grouping))
                new_sparse_shape.append(dimension)
            else:
                group_classifications.append(False)
                nested_grouping = []
                dimension = 1
                for axis in group:
                    axis = transposition[axis]
                    if axis < sparse_ndim:
                        raise ValueError("grouping {} mixes sparse and nested axes")
                    axis -= sparse_ndim
                    nested_grouping.append(axis)
                    dimension *= nested_shape[axis]
                nested_groupings.append(tuple(nested_grouping))
                new_nested_shape.append(dimension)
        new_sparse_shape = tuple(new_sparse_shape)
        new_sparse_ndim = len(new_sparse_shape)
        new_nested_shape = tuple(new_nested_shape)
        new_nested_ndim = len(new_nested_shape)
      # }}}
      # Perform the join {{{
        new_index_to_data_map = {}
        for (indices,data) in self.index_to_data_map.iteritems():
            new_indices = tuple(sum(multiplier*indices[axis] for (multiplier,axis) in group) for group in sparse_groupings)
            new_data = data.join(nested_groupings)
            if new_indices in new_index_to_data_map:
                new_index_to_data_map[new_indices] += new_data
            else:
                new_index_to_data_map[new_indices] = new_data
      # }}}
      # Construct the new transposition {{{
        new_transposition = []
        current_sparse_axis = 0
        current_nested_axis = new_sparse_ndim
        for grouping_is_sparse in group_classifications:
            if grouping_is_sparse:
                new_transposition.append(current_sparse_axis)
                current_sparse_axis += 1
            else:
                new_transposition.append(current_nested_axis)
                current_nested_axis += 1
      # }}}
      # Return the result {{{
        return SparseData(new_sparse_shape,new_index_to_data_map,new_nested_shape,new_transposition)
      # }}}
    # }}}
    def toArray(self): # {{{
        result_shape = self.sparse_shape + self.nested_shape
        if not self.sparse_shape:
            if self.index_to_data_map:
                result = self.index_to_data_map[()].toArray()
            else:
                result = zeros(result_shape)
        else:
            first = True
            for index, data in self.index_to_data_map.iteritems():
                data = data.toArray()
                if first:
                    result = zeros(result_shape,dtype=data.dtype)
                    first = False
                result[index] += data
            if first:
                result = zeros(result_shape)
        return result.transpose(self.transposition)
    # }}}
    def transpose(self,transposition): # {{{
        if len(transposition) != self.ndim:
            raise ValueError("transposition is incompatible with the rank: len({}) = {} != {}".format(transposition,len(transposition),self.ndim))
        self = copy(self)
        self.transposition = applyPermutation(transposition,self.transposition)
        self.shape = applyPermutation(transposition,self.shape)
        return self
    # }}}
  # }}}
# }}}
# }}}

# Exports {{{
__all__ = [
    # Classes {{{
    "NDArrayData",
    "ScalarData",
    "SparseData",
    # }}}
]
# }}}
