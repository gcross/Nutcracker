# Imports {{{
from collections import namedtuple
from copy import copy
from numpy import array, complex128, dot, ndarray, product, sqrt, tensordot, zeros
from numpy.linalg import eigh, norm, qr, svd
from numpy.random import rand

from . import core
# }}}

# Types {{{
TensorNumberAndIndex = namedtuple("TensorNumberAndIndex",["tensor","index"])
# }}}

# Functions {{{

def appended(vector,entry): # {{{
    copy_of_vector = list(vector)
    if copy_of_vector is vector:
        copy_of_vector = copy(vector)
    copy_of_vector.append(entry)
    return copy_of_vector
# }}}

def basisVector(dimension,index,dtype=complex128): # {{{
    vector = zeros(dimension,dtype=dtype)
    vector[index] = 1
    return vector
# }}}

def CCW(i): # {{{
    return (i+1)%4
# }}}

def compressConnectionBetween(tensor_1,index_1,tensor_2,index_2,keep=None,threshold=None): # {{{
    return truncateConnectionBetween(tensor_1,index_1,tensor_2,index_2,constructFilterFrom(keep=keep,threshold=threshold))
# }}}

def compressConnectionBetweenTensors(tensor_1,index_1,tensor_2,index_2,keep=None,threshold=None): # {{{
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        compressConnectionBetween(tensor_1.data,index_1,tensor_2.data,index_2,keep,threshold)
    )
# }}}

def compressConnectionToSelf(tensor,index,keep=None,threshold=None): # {{{
    return truncateConnectionToSelf(tensor,index,constructFilterFrom(keep=keep,threshold=threshold))
# }}}

def compressConnectionToSelfTensor(tensor,index,keep=None,threshold=None): # {{{
    return type(tensor)(compressConnectionToSelf(tensor.data,index,keep,threshold))
# }}}

def compressConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,keep=None,threshold=None): # {{{
    return truncateConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,constructFilterFrom(keep=keep,threshold=threshold))
# }}}

def compressConnectionUsingFirstTensorOnlyBetweenTensors(tensor_1,index_1,tensor_2,index_2,keep=None,threshold=None): # {{{
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        compressConnectionUsingFirstTensorOnlyBetween(tensor_1.data,index_1,tensor_2.data,index_2,keep,threshold)
    )
# }}}

def compressHermitianConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,index_2c,keep=None,threshold=None): # {{{
    return truncateHermitianConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,index_2c,constructFilterFrom(keep=keep,threshold=threshold))
# }}}

def compressHermitianConnectionUsingFirstTensorOnlyBetweenTensors(tensor_1,index_1,tensor_2,index_2,index_2c,keep=None,threshold=None): # {{{
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        compressHermitianConnectionUsingFirstTensorOnlyBetween(tensor_1.data,index_1,tensor_2.data,index_2,index_2c,keep,threshold)
    )
# }}}

def computePostContractionIndexMap(old_number_of_dimensions,indices_being_contracted,offset=0): # {{{
    indices_being_contracted = frozenset(indices_being_contracted)
    new_number_of_dimensions = old_number_of_dimensions - len(indices_being_contracted)
    current_old_index = 0
    old_to_new_index_map = {}
    for current_new_index in xrange(new_number_of_dimensions):
        while current_old_index in indices_being_contracted:
            current_old_index += 1
        old_to_new_index_map[current_old_index] = current_new_index + offset
        current_old_index += 1
    return old_to_new_index_map
# }}}

def contract(arrays,array_axes): # {{{
    return tensordot(data_1,data_2,(axes_1,axes_2))
# }}}

def contractAndTranspose(arrays,array_axes,new_order): # {{{
    return contractAndTransposeAndJoin(arrays,array_axes,[[x] for x in new_order])
# }}}

def contractAndTransposeAndJoin(arrays,array_axes,new_grouped_order): # {{{
    if array_axes[0] != array_axes[1]:
        raise ValueError("the number of indices to be contracted is inconsistent ({} != {})".format(*array_axes))
    old_to_new_maps = map(computePostContractionIndexMap,array_axes)
    shapes = [array.shape for array in arrays]
    contracted_axes_sets = map(frozenset,array_axes)
    non_contracted_axes_sets = [
        set(xrange(array.ndim)) - contracted_axes_set
        for (array,contracted_axes_set) in zip (arrays,contracted_axes_sets)
    ]
    new_order = []
    new_shape = []
    for group in new_grouped_order:
        dimension = 1
        for tensor_number, index in group:
            if tensor_number not in [0,1]:
                raise ValueError("the tensor index must be either 0 or 1, not {}".format(index))
            if index in contracted_axes[tensor_number]:
                raise ValueError("index {} of tensor {} appears both in the list of indices to contract and in the final order".format(index,tensor_number))
            if index not in non_contracted_axes_sets[tensor_number]:
                raise ValueError("index {} of tensor {} appears twice in the final ordering".format(index,tensor_number))
            non_contracted_axes_sets[tensor_number].remove(index)
            new_order.append(old_to_new_maps[tensor_number][index])
            dimension *= shapes[tensor_number][index]
        new_shape.append(dimension)
    if any(non_contracted_axes_sets):
        raise ValueError("the follow indices of respectively tensor 0 and tensor 1 appear nowhere in either the axes to contract or in the final ordering: {}, {}".format(*non_contracted_axes_sets)
    return tensordot(arrays[0],arrays[1],array_axes).transpose(new_order).reshape(new_shape)
# }}}

def constructFilterFrom(keep=None,threshold=None): # {{{
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
# }}}

def crand(*shape): # {{{
    return rand(*shape)*2-1+rand(*shape)*2j-1j
# }}}

def CW(i): # {{{
    return (i-1)%4
# }}}

def firstIndexBelowMagnitude(arr,magnitude): # {{{
    index = 0
    for x in arr:
        if abs(x) < magnitude:
            break
        index += 1
    return index
# }}}

def firstIndexWithNonZeroMagnitude(arr): # {{{
    return firstIndexBelowMagnitude(arr,1e-13)
# }}}

def formContractor(order,joins,result_joins): # {{{
    observed_tensor_indices = {}

    # Tabulate all of the observed tensor indices {{{
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

    for tensor_id in observed_tensor_indices:
        if tensor_id not in order:
            raise ValueError("tensor {} does not appear in the list of arguments ({})".format(tensor_id,order))
    # }}}

    tensor_join_ids = {}

    # Check the observed tensor indices and initialize the map of tensor joins {{{
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
    # }}}

    result_join_ids = []

    # Label each join with a unique id {{{
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
    # }}}

    argument_join_ids = [tensor_join_ids[tensor_id] for tensor_id in order]

    # Form the contractor function {{{
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
                raise ValueError("Error when joining tensor {}: '{}' (first tensor axes are {} with dimensions {}, second ({}) axes are {} with dimensions {})".format(order[join_ids_index],str(e),first_axes,[current_tensor.shape[i] for i in first_axes],order[join_ids_index],second_axes,[next_tensor.shape[i] for i in second_axes]))
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
    # }}}

    return contract
# }}}

def increaseDimensionNaivelyBetween(tensor_1,index_1,tensor_2,index_2,new_dimension): # {{{
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
# }}}

def increaseDimensionNaivelyBetweenTensors(tensor_1,index_1,tensor_2,index_2,new_dimension): # {{{
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        increaseDimensionNaivelyBetween(tensor_1.data,index_1,tensor_2.data,index_2,new_dimension)
    )
# }}}

def increaseDimensionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,new_dimension): # {{{
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
# }}}

def increaseDimensionUsingFirstTensorOnlyBetweenTensors(tensor_1,index_1,tensor_2,index_2,new_dimension): # {{{
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        increaseDimensionUsingFirstTensorOnlyBetween(tensor_1.data,index_1,tensor_2.data,index_2,new_dimension)
    )
# }}}

def mapFunctions(functions,data): # {{{
    for f, x in zip(functions,data):
        yield f(x)
# }}}

def multiplyTensorByMatrixAtIndex(tensor,matrix,index): # {{{
    tensor_new_indices = list(range(tensor.ndim-1))
    tensor_new_indices.insert(index,tensor.ndim-1)
    return tensordot(tensor,matrix,(index,0)).transpose(tensor_new_indices)
# }}}

def normalize(tensor,index): # {{{
    reshaped_tensor, inverseTransformation = transposeAndReshapeAndReturnInverseTransformation(tensor,index)
    reshaped_tensor = array(reshaped_tensor,order='F')
    core.orthogonalize_matrix_in_place(reshaped_tensor)
    return inverseTransformation(reshaped_tensor)
# }}}

def normalizeAndDenormalize(tensor_1,index_1,tensor_2,index_2): # {{{
    if tensor_1.shape[index_1] != tensor_2.shape[index_2]:
        raise ValueError("The dimension to be normalized in the first tensor (index {}) is not equal to the dimension to be denormalized in the second tensor (index {}). ({} != {})".format(index_1,index_2,tensor_1.shape[index_1],tensor_2.shape[index_2]))
    new_tensor_1, inverse_normalizer = normalizeAndReturnInverseNormalizer(tensor_1,index_1)
    new_tensor_2 = multiplyTensorByMatrixAtIndex(tensor_2,inverse_normalizer,index_2)

    return new_tensor_1, new_tensor_2
# }}}

def normalizeAndDenormalizeTensors(tensor_1,index_1,tensor_2,index_2): # {{{
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        normalizeAndDenormalize(tensor_1.data,index_1,tensor_2.data,index_2)
    )
# }}}

def normalizeAndReturnInverseNormalizer(tensor,index): # {{{
    new_tensor, inverseTransformation = transposeAndReshapeAndReturnInverseTransformation(tensor,index)
    if new_tensor.shape[1] > new_tensor.shape[0]:
        raise ValueError("The dimension to be normalized is larger than the product of the rest of the dimensions. ({} > {})".format(new_tensor.shape[1],new_tensor.shape[0]))

    q, r = qr(new_tensor)

    return inverseTransformation(q), r.transpose()
# }}}

def OPP(i): # {{{
    return (i+2)%4
# }}}

def otimes(A,B,axes=0): # {{{
    return tensordot(A,B,axes).transpose(0,2,1,3).reshape(*[A.shape[i]*B.shape[i] for i in range(2)])
# }}}

def transposeAndReshapeAndReturnInverseTransformation(tensor,index): # {{{
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
# }}}

def truncateConnectionAndReturnCompressor(tensor,index,filter): # {{{
    reshaped_tensor, inverseTransformation = transposeAndReshapeAndReturnInverseTransformation(tensor,index)
    u, s, v = svd(reshaped_tensor,full_matrices=False)
    s[s < 0] = 0
    keep = filter(s/s[0] if s[0] != 0 else s) if callable(filter) else filter
    return (
        inverseTransformation(u[:,:keep]*s[:keep].reshape(1,keep)),
        v[:keep].transpose()
    )
# }}}

def truncateConnectionBetween(tensor_1,index_1,tensor_2,index_2,filter): # {{{
    reshaped_tensor_1, inverseTransformation_1 = transposeAndReshapeAndReturnInverseTransformation(tensor_1,index_1)
    reshaped_tensor_2, inverseTransformation_2 = transposeAndReshapeAndReturnInverseTransformation(tensor_2,index_2)
    u, s, v = svd(tensordot(reshaped_tensor_1,reshaped_tensor_2,(1,1)),full_matrices=False)
    s[s < 0] = 0
    keep = filter(s/s[0] if s[0] != 0 else s) if callable(filter) else filter
    s = sqrt(s[:keep]).reshape(1,keep)
    return (inverseTransformation_1(s*u[:,:keep]),inverseTransformation_2(s*(v.transpose()[:,:keep])))
# }}}

def truncateConnectionToSelf(tensor,index,filter): # {{{
    reshaped_tensor, inverseTransformation = transposeAndReshapeAndReturnInverseTransformation(tensor,index)
    evals, evecs = eigh(tensordot(reshaped_tensor,reshaped_tensor.conj(),(1,1)))
    sorted_indices = evals.argsort()[::-1]
    evals = evals[sorted_indices]
    evecs = evecs[:,sorted_indices]
    evals[evals < 0] = 0
    keep = filter(evals/evals[0] if evals[0] != 0 else evals) if callable(filter) else filter
    return inverseTransformation(sqrt(evals[:keep])*evecs[:,:keep])
# }}}

def truncateConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,filter): # {{{
    new_tensor_1, compressor = truncateConnectionAndReturnCompressor(tensor_1,index_1,filter)
    return new_tensor_1, multiplyTensorByMatrixAtIndex(tensor_2,compressor,index_2)
# }}}

def truncateHermitianConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,index_2c,filter): # {{{
    new_tensor_1, compressor = truncateConnectionAndReturnCompressor(tensor_1,index_1,filter)
    return new_tensor_1, multiplyTensorByMatrixAtIndex(multiplyTensorByMatrixAtIndex(tensor_2,compressor,index_2),compressor.conj(),index_2c)
# }}}

def withoutIndex(vector,index): # {{{
    copy_of_vector = list(vector)
    if copy_of_vector is vector:
        copy_of_vector = copy(vector)
    del copy_of_vector[index]
    return copy_of_vector
# }}}

# }}}

# Exports {{{
__all__ = [
    "TensorNumberAndIndex",

    "appended",
    "basisVector",
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
    "computePostContractionIndexMap",
    "contract",
    "contractAndTranspose",
    "contractAndTransposeAndJoin",
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
# }}}
