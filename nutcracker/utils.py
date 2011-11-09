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
from copy import copy
from numpy import array, dot, ndarray, product, sqrt, tensordot
from numpy.linalg import eigh, norm, qr, svd
from numpy.random import rand

from . import core
#@-<< Imports >>

#@+others
#@+node:gcross.20111107123726.3237: ** Functions
#@+node:gcross.20111108100704.1417: *3* appended
def appended(vector,entry):
    copy_of_vector = list(vector)
    if copy_of_vector is vector:
        copy_of_vector = copy(vector)
    copy_of_vector.append(entry)
    return copy_of_vector
#@+node:gcross.20111014183107.1246: *3* CCW
def CCW(i):
    return (i+1)%4
#@+node:gcross.20111022200315.1311: *3* compressConnectionBetween
def compressConnectionBetween(tensor_1,index_1,tensor_2,index_2,keep=None,threshold=None):
    return truncateConnectionBetween(tensor_1,index_1,tensor_2,index_2,constructFilterFrom(keep=keep,threshold=threshold))
#@+node:gcross.20111022200315.1330: *3* compressConnectionBetweenTensors
def compressConnectionBetweenTensors(tensor_1,index_1,tensor_2,index_2,keep=None,threshold=None):
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        compressConnectionBetween(tensor_1.data,index_1,tensor_2.data,index_2,keep,threshold)
    )
#@+node:gcross.20111022200315.1278: *3* compressConnectionToSelf
def compressConnectionToSelf(tensor,index,keep=None,threshold=None):
    return truncateConnectionToSelf(tensor,index,constructFilterFrom(keep=keep,threshold=threshold))
#@+node:gcross.20111022200315.1332: *3* compressConnectionToSelfTensor
def compressConnectionToSelfTensor(tensor,index,keep=None,threshold=None):
    return type(tensor)(compressConnectionToSelf(tensor.data,index,keep,threshold))
#@+node:gcross.20111024143336.1321: *3* compressConnectionUsingFirstTensorOnlyBetween
def compressConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,keep=None,threshold=None):
    return truncateConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,constructFilterFrom(keep=keep,threshold=threshold))
#@+node:gcross.20111024143336.1325: *3* compressConnectionUsingFirstTensorOnlyBetweenTensors
def compressConnectionUsingFirstTensorOnlyBetweenTensors(tensor_1,index_1,tensor_2,index_2,keep=None,threshold=None):
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        compressConnectionUsingFirstTensorOnlyBetween(tensor_1.data,index_1,tensor_2.data,index_2,keep,threshold)
    )
#@+node:gcross.20111107154810.1384: *3* compressHermitianConnectionUsingFirstTensorOnlyBetween
def compressHermitianConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,index_2c,keep=None,threshold=None):
    return truncateHermitianConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,index_2c,constructFilterFrom(keep=keep,threshold=threshold))
#@+node:gcross.20111107154810.1386: *3* compressHermitianConnectionUsingFirstTensorOnlyBetweenTensors
def compressHermitianConnectionUsingFirstTensorOnlyBetweenTensors(tensor_1,index_1,tensor_2,index_2,index_2c,keep=None,threshold=None):
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        compressHermitianConnectionUsingFirstTensorOnlyBetween(tensor_1.data,index_1,tensor_2.data,index_2,index_2c,keep,threshold)
    )
#@+node:gcross.20111022200315.1309: *3* computeFilterFrom
def constructFilterFrom(keep=None,threshold=None):
    if keep is None and threshold is None:
        raise ValueError("either keep or threshold needs to be specified")
    if keep is not None and threshold is not None:
        raise ValueError("both keep and threshold cannot be simultaneously specified")
    if keep is not None:
        return keep
    if threshold is not None:
        if threshold == 0:
            threshold = 1e-10
        return lambda arr: firstIndexBelowMagnitude(arr,threshold)
#@+node:gcross.20111107131531.1353: *3* crand
def crand(*shape):
    return rand(*shape)*2-1+rand(*shape)*2j-1j
#@+node:gcross.20111014183107.1248: *3* CW
def CW(i):
    return (i-1)%4
#@+node:gcross.20111022200315.1274: *3* firstIndexBelowMagnitude
def firstIndexBelowMagnitude(arr,magnitude):
    index = 0
    for x in arr:
        if abs(x) < magnitude:
            break
        index += 1
    return index
#@+node:gcross.20111022200315.1276: *3* firstIndexWithNonZeroMagnitude
def firstIndexWithNonZeroMagnitude(arr):
    return firstIndexBelowMagnitude(arr,1e-13)
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
#@+node:gcross.20111017110141.1258: *3* increaseDimensionNaivelyBetween
def increaseDimensionNaivelyBetween(tensor_1,index_1,tensor_2,index_2,new_dimension):
    if tensor_1.shape[index_1] != tensor_2.shape[index_2]:
        raise ValueError("dimension {} of tensor 1 has different size than dimension {} ({} != {})".format(index_1,index_2,tensor_1.shape[index_1],tensor_2.shape[index_2]))
    old_dimension = tensor_1.shape[index_1]
    if new_dimension < old_dimension:
        raise ValueError("new dimension ({}) must be at least the old dimension ({})".format(new_dimension,old_dimension))
    if new_dimension == old_dimension:
        return tensor_1, tensor_2
    matrix, _ = qr(crand(new_dimension,old_dimension))
    matrix = matrix.transpose()
    return (
        multiplyTensorByMatrixAtIndex(tensor_1,matrix,index_1),
        multiplyTensorByMatrixAtIndex(tensor_2,matrix.conj(),index_2),
    )
#@+node:gcross.20111017110141.1260: *3* increaseDimensionNaivelyBetweenTensors
def increaseDimensionNaivelyBetweenTensors(tensor_1,index_1,tensor_2,index_2,new_dimension):
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        increaseDimensionNaivelyBetween(tensor_1.data,index_1,tensor_2.data,index_2,new_dimension)
    )
#@+node:gcross.20111028110210.1317: *3* increaseDimensionUsingFirstTensorOnlyBetween
def increaseDimensionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,new_dimension):
    if tensor_1.shape[index_1] != tensor_2.shape[index_2]:
        raise ValueError("dimension {} of tensor 1 has different size than dimension {} ({} != {})".format(index_1,index_2,tensor_1.shape[index_1],tensor_2.shape[index_2]))
    old_dimension = tensor_1.shape[index_1]
    if new_dimension < old_dimension:
        raise ValueError("new dimension ({}) must be at least the old dimension ({})".format(new_dimension,old_dimension))
    if new_dimension == old_dimension:
        return tensor_1, tensor_2
    maximum_dimension = product(withoutIndex(tensor_1.shape,index_1))
    if new_dimension > maximum_dimension:
        raise ValueError("new dimension ({}) cannot be greater than the product of the rest of its dimensions ({})".format(new_dimension,maximum_dimension))
    reshaped_tensor_1, inverseTransformation_1 = transposeAndReshapeAndReturnInverseTransformation(tensor_1,index_1)

    q, r = qr(reshaped_tensor_1)

    vlen = q.shape[0]
    new_q = ndarray(shape=(vlen,new_dimension),dtype=q.dtype)
    new_q[:,:old_dimension] = q
    for d in range(old_dimension,new_dimension):
        vn = 0
        while vn < 1e-15:
            v = crand(vlen)
            v -= dot(new_q[:,:d],dot(v,new_q[:,:d].conj()))
            vn = norm(v)
        new_q[:,d] = v / vn

    expander, _ = qr(crand(new_dimension,new_dimension))
    return (
        inverseTransformation_1(dot(new_q,expander[:,:].transpose().conj())),
        multiplyTensorByMatrixAtIndex(tensor_2,dot(r.transpose(),expander[:,:old_dimension].transpose()),index_2),
    )
#@+node:gcross.20111028110210.1334: *3* increaseDimensionUsingFirstTensorOnlyBetweenTensors
def increaseDimensionUsingFirstTensorOnlyBetweenTensors(tensor_1,index_1,tensor_2,index_2,new_dimension):
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        increaseDimensionUsingFirstTensorOnlyBetween(tensor_1.data,index_1,tensor_2.data,index_2,new_dimension)
    )
#@+node:gcross.20111014113710.1239: *3* mapFunctions
def mapFunctions(functions,data):
    for f, x in zip(functions,data):
        yield f(x)
#@+node:gcross.20111013080525.3956: *3* multiplyTensorByMatrixAtIndex
def multiplyTensorByMatrixAtIndex(tensor,matrix,index):
    tensor_new_indices = list(range(tensor.ndim-1))
    tensor_new_indices.insert(index,tensor.ndim-1)
    return tensordot(tensor,matrix,(index,0)).transpose(tensor_new_indices)
#@+node:gcross.20111014183107.1245: *3* normalize
def normalize(tensor,index):
    reshaped_tensor, inverseTransformation = transposeAndReshapeAndReturnInverseTransformation(tensor,index)
    reshaped_tensor = array(reshaped_tensor,order='F')
    core.orthogonalize_matrix_in_place(reshaped_tensor)
    return inverseTransformation(reshaped_tensor)
#@+node:gcross.20111013183808.3917: *3* normalizeAndDenormalize
def normalizeAndDenormalize(tensor_1,index_1,tensor_2,index_2):
    if tensor_1.shape[index_1] != tensor_2.shape[index_2]:
        raise ValueError("The dimension to be normalized in the first tensor (index {}) is not equal to the dimension to be denormalized in the second tensor (index {}). ({} != {})".format(index_1,index_2,tensor_1.shape[index_1],tensor_2.shape[index_2]))
    new_tensor_1, inverse_normalizer = normalizeAndReturnInverseNormalizer(tensor_1,index_1)
    new_tensor_2 = multiplyTensorByMatrixAtIndex(tensor_2,inverse_normalizer,index_2)

    return new_tensor_1, new_tensor_2
#@+node:gcross.20111017110141.1262: *3* normalizeAndDenormalizeTensors
def normalizeAndDenormalizeTensors(tensor_1,index_1,tensor_2,index_2):
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        normalizeAndDenormalize(tensor_1.data,index_1,tensor_2.data,index_2)
    )
#@+node:gcross.20111014182232.1247: *3* normalizeAndReturnInverseNormalizer
def normalizeAndReturnInverseNormalizer(tensor,index):
    new_tensor, inverseTransformation = transposeAndReshapeAndReturnInverseTransformation(tensor,index)
    if new_tensor.shape[1] > new_tensor.shape[0]:
        raise ValueError("The dimension to be normalized is larger than the product of the rest of the dimensions. ({} > {})".format(new_tensor.shape[1],new_tensor.shape[0]))

    q, r = qr(new_tensor)

    return inverseTransformation(q), r.transpose()
#@+node:gcross.20111014183107.1249: *3* OPP
def OPP(i):
    return (i+2)%4
#@+node:gcross.20111101164302.1330: *3* otimes
def otimes(A,B,axes=0):
    return tensordot(A,B,axes).transpose(0,2,1,3).reshape(*[A.shape[i]*B.shape[i] for i in range(2)])
#@+node:gcross.20111022200315.1264: *3* transposeAndReshapeAndReturnInverseTransformation
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
#@+node:gcross.20111107123047.1400: *3* truncateConnectionAndReturnCompressor
def truncateConnectionAndReturnCompressor(tensor,index,filter):
    reshaped_tensor, inverseTransformation = transposeAndReshapeAndReturnInverseTransformation(tensor,index)
    u, s, v = svd(reshaped_tensor,full_matrices=False)
    s[s < 0] = 0
    keep = filter(s/s[0] if s[0] != 0 else s) if callable(filter) else filter
    return (
        inverseTransformation(u[:,:keep]*s[:keep].reshape(1,keep)),
        v[:keep].transpose()
    )
#@+node:gcross.20111022200315.1306: *3* truncateConnectionBetween
def truncateConnectionBetween(tensor_1,index_1,tensor_2,index_2,filter):
    reshaped_tensor_1, inverseTransformation_1 = transposeAndReshapeAndReturnInverseTransformation(tensor_1,index_1)
    reshaped_tensor_2, inverseTransformation_2 = transposeAndReshapeAndReturnInverseTransformation(tensor_2,index_2)
    u, s, v = svd(tensordot(reshaped_tensor_1,reshaped_tensor_2,(1,1)),full_matrices=False)
    s[s < 0] = 0
    keep = filter(s/s[0] if s[0] != 0 else s) if callable(filter) else filter
    s = sqrt(s[:keep]).reshape(1,keep)
    return (inverseTransformation_1(s*u[:,:keep]),inverseTransformation_2(s*(v.transpose()[:,:keep])))
#@+node:gcross.20111022200315.1265: *3* truncateConnectionToSelf
def truncateConnectionToSelf(tensor,index,filter):
    reshaped_tensor, inverseTransformation = transposeAndReshapeAndReturnInverseTransformation(tensor,index)
    evals, evecs = eigh(tensordot(reshaped_tensor,reshaped_tensor.conj(),(1,1)))
    sorted_indices = evals.argsort()[::-1]
    evals = evals[sorted_indices]
    evecs = evecs[:,sorted_indices]
    evals[evals < 0] = 0
    keep = filter(evals/evals[0] if evals[0] != 0 else evals) if callable(filter) else filter
    return inverseTransformation(sqrt(evals[:keep])*evecs[:,:keep])
#@+node:gcross.20111024143336.1315: *3* truncateConnectionUsingFirstTensorOnlyBetween
def truncateConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,filter):
    new_tensor_1, compressor = truncateConnectionAndReturnCompressor(tensor_1,index_1,filter)
    return new_tensor_1, multiplyTensorByMatrixAtIndex(tensor_2,compressor,index_2)
#@+node:gcross.20111107123047.1402: *3* truncateHermitianConnectionUsingFirstTensorOnlyBetween
def truncateHermitianConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,index_2c,filter):
    new_tensor_1, compressor = truncateConnectionAndReturnCompressor(tensor_1,index_1,filter)
    return new_tensor_1, multiplyTensorByMatrixAtIndex(multiplyTensorByMatrixAtIndex(tensor_2,compressor,index_2),compressor.conj(),index_2c)
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
    "appended",
    "CCW",
    "crand",
    "compressConnectionBetween",
    "compressConnectionBetweenTensors",
    "compressConnectionToSelf",
    "compressConnectionToSelfTensor",
    "compressConnectionUsingFirstTensorOnlyBetween",
    "compressConnectionUsingFirstTensorOnlyBetweenTensors",
    "compressHermitianConnectionUsingFirstTensorOnlyBetween",
    "compressHermitianConnectionUsingFirstTensorOnlyBetweenTensors",
    "CW",
    "firstIndexBelowMagnitude",
    "formContractor",
    "increaseDimensionNaivelyBetween",
    "increaseDimensionNaivelyBetweenTensors",
    "increaseDimensionUsingFirstTensorOnlyBetween",
    "increaseDimensionUsingFirstTensorOnlyBetweenTensors",
    "mapFunctions",
    "multiplyTensorByMatrixAtIndex",
    "normalize",
    "normalizeAndDenormalize",
    "normalizeAndDenormalizeTensors",
    "normalizeAndReturnInverseNormalizer",
    "OPP",
    "transposeAndReshapeAndReturnInverseTransformation",
    "truncateConnectionBetween",
    "truncateConnectionToSelf",
    "truncateConnectionUsingFirstTensorOnlyBetween",
    "truncateHermitianConnectionUsingFirstTensorOnlyBetween",
    "withoutIndex",
]
#@-<< Exports >>
#@-leo
