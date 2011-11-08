#@+leo-ver=5-thin
#@+node:gcross.20111107123726.3236: * @file utils.py
#@+<< License >>
#@+node:gcross.20111107123726.3249: ** << License >>
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
#@+node:gcross.20111107123726.3250: ** << Imports >>
from numpy import array, product, tensordot
from numpy.random import rand

from . import core
from .miscellaneous.enum_meta import Enum
#@-<< Imports >>

#@+others
#@+node:gcross.20111108100704.1380: ** Enumerations
#@+node:gcross.20111108100704.1382: *3* Direction
class Direction(Enum):
    left = "left"
    right = "right"
#@+node:gcross.20111108100704.1381: *3* Normalization
class Normalization(Direction):
    middle = "middle"
    none = "none"
#@+node:gcross.20111107123726.3237: ** Functions
#@+node:gcross.20111108100704.1417: *3* appended
def appended(vector,entry):
    copy_of_vector = list(vector)
    if copy_of_vector is vector:
        copy_of_vector = copy(vector)
    copy_of_vector.append(entry)
    return copy_of_vector
#@+node:gcross.20111107131531.1353: *3* crand
def crand(*shape):
    return rand(*shape)*2-1+rand(*shape)*2j-1j
#@+node:gcross.20111107123726.3243: *3* formContractor
def formContractor(order,joins,result_joins):
    observed_tensor_indices = {}

    #@+<< Tabulate all of the observed tensor indices >>
    #@+node:gcross.20111107123726.3244: *4* << Tabulate all of the observed tensor indices >>
    for (tensor_id,index) in sum([list(x) for x in joins] + [list(x) for x in result_joins],[]):
        if index < 0:
            raise ValueError("index {} of tensor {} is negative".format(index,tensor_id))
        try:
            observed_indices = observed_tensor_indices[tensor_id]
        except KeyError:
            observed_indices = set()
            observed_tensor_indices[tensor_id] = observed_indices
        if index in observed_indices:
            raise ValueError("index {} of tensor {} appears more than once in the joins".format(index,tensor_id))
        observed_indices.add(index)
    #@-<< Tabulate all of the observed tensor indices >>

    for tensor_id in observed_tensor_indices:
        if tensor_id not in order:
            raise ValueError("tensor {} does not appear in the list of arguments ({})".format(tensor_id,order))

    tensor_join_ids = {}

    #@+<< Check the observed tensor indices and initialize the map of tensor joins >>
    #@+node:gcross.20111107123726.3245: *4* << Check the observed tensor indices and initialize the map of tensor joins >>
    for (tensor_id,observed_indices) in observed_tensor_indices.items():
        tensor_dimension = max(observed_indices)+1
        expected_indices = set(range(tensor_dimension))
        invalid_indices = observed_indices - expected_indices
        missing_indices = expected_indices - observed_indices
        if len(invalid_indices) > 0:
            raise ValueError('the invalid indices {} have appeared in joins involving tendor {}'.format(invalid_indices,tensor_id))
        if len(missing_indices) > 0:
            raise ValueError('the expected indices {} do not appear in any of the joins for tensor {}'.format(missing_indices,tensor_id))
        if tensor_id not in order:
            raise ValueError('tensor {} does not appear in the list of arguments'.format(tensor_id))
        tensor_join_ids[tensor_id] = [None]*tensor_dimension
    #@-<< Check the observed tensor indices and initialize the map of tensor joins >>

    result_join_ids = []

    #@+<< Label each join with a unique id >>
    #@+node:gcross.20111107123726.3246: *4* << Label each join with a unique id >>
    current_join_id = 0
    for join in joins:
        for (tensor_id,index) in join:
            tensor_join_ids[tensor_id][index] = current_join_id
        current_join_id += 1
    for join in result_joins:
        join_ids = []
        for (tensor_id,index) in join:
            join_ids.append(current_join_id)
            tensor_join_ids[tensor_id][index] = current_join_id
            current_join_id += 1
        result_join_ids.append(join_ids)
    #@-<< Label each join with a unique id >>

    argument_join_ids = [tensor_join_ids[tensor_id] for tensor_id in order]

    #@+<< Form the contractor function >>
    #@+node:gcross.20111107123726.3247: *4* << Form the contractor function >>
    def contract(*arguments):
        if len(arguments) != len(order):
            raise ValueError("wrong number of arguments;  expected {} but received {}".format(len(order),len(arguments)))
        for (i, (tensor_id, argument)) in enumerate(zip(order,arguments)):
            if argument.ndim != len(tensor_join_ids[tensor_id]):
                raise ValueError("argument {} ('{}') has rank {} when it was expected to have rank {}".format(i,order[i],argument.ndim,len(tensor_join_ids[tensor_id])))
        arguments = list(arguments)
        join_ids_index = -1
        current_tensor = arguments.pop()
        current_join_ids = argument_join_ids[join_ids_index]
        while len(arguments) > 0:
            join_ids_index -= 1
            next_tensor = arguments.pop()
            next_join_ids = argument_join_ids[join_ids_index]
            try:
                first_axes = []
                second_axes = []
                first_axis_index = 0
                common_join_ids = set()
                for join_id in current_join_ids:
                    if join_id in next_join_ids:
                        common_join_ids.add(join_id)
                        first_axes.append(first_axis_index)
                        second_axes.append(next_join_ids.index(join_id))
                    first_axis_index += 1
                current_tensor = tensordot(current_tensor,next_tensor,(first_axes,second_axes))
                current_join_ids = [i for i in current_join_ids+next_join_ids if i not in common_join_ids]
            except Exception as e:
                raise ValueError("Error when joining tensor {}: '{}'".format(order[join_ids_index],str(e)))
        current_tensor = current_tensor.transpose([current_join_ids.index(i) for i in sum([list(x) for x in result_join_ids],[])])
        old_shape = current_tensor.shape
        new_shape = []
        index = 0
        for join in result_join_ids:
            dimension = 1
            for _ in join:
                dimension *= old_shape[index]
                index += 1
            new_shape.append(dimension)
        return current_tensor.reshape(new_shape)
    #@-<< Form the contractor function >>

    return contract
#@+node:gcross.20111107131531.3590: *3* mapFunctions
def mapFunctions(functions,data):
    for f, x in zip(functions,data):
        yield f(x)
#@+node:gcross.20111108100704.1384: *3* normalize
def normalize(tensor,index):
    reshaped_tensor, inverseTransformation = transposeAndReshapeAndReturnInverseTransformation(tensor,index)
    reshaped_tensor = array(reshaped_tensor,order='F')
    core.orthogonalize_matrix_in_place(reshaped_tensor)
    return inverseTransformation(reshaped_tensor)
#@+node:gcross.20111108100704.1386: *3* transposeAndReshapeAndReturnInverseTransformation
def transposeAndReshapeAndReturnInverseTransformation(tensor,index):
    new_indices = withoutIndex(range(tensor.ndim),index)
    new_indices.append(index)

    old_shape = withoutIndex(tensor.shape,index)
    new_shape = (product(old_shape),tensor.shape[index])

    inverse_indices = list(range(tensor.ndim-1))
    inverse_indices.insert(index,tensor.ndim-1)

    def inverseTransformer(tensor):
        inverse_shape = appended(old_shape,tensor.shape[1])
        return tensor.reshape(inverse_shape).transpose(inverse_indices)

    return tensor.transpose(new_indices).reshape(new_shape), inverseTransformer
#@+node:gcross.20111108100704.1376: *3* withoutIndex
def withoutIndex(vector,index):
    copy_of_vector = list(vector)
    if copy_of_vector is vector:
        copy_of_vector = copy(vector)
    del copy_of_vector[index]
    return copy_of_vector
#@-others

#@+<< Exports >>
#@+node:gcross.20111107123726.3251: ** << Exports >>
__all__ = [
    "Direction",
    "Normalization",

    "appended",
    "crand",
    "formContractor",
    "normalize",
    "withoutIndex",
]
#@-<< Exports >>
#@-leo
