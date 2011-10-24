#@+leo-ver=5-thin
#@+node:gcross.20111009135633.1129: * @file utils.py
#@+<< Imports >>
#@+node:gcross.20111009135633.1135: ** << Imports >>
from copy import copy
from numpy import product, sqrt, tensordot
from numpy.linalg import eigh, svd
from numpy.random import rand
#@-<< Imports >>

#@+others
#@+node:gcross.20111009135633.1130: ** Functions
#@+node:gcross.20111022200315.1271: *3* appended
def appended(vector,entry):
    copy_of_vector = list(vector)
    if copy_of_vector is vector:
        copy_of_vector = copy(vector)
    copy_of_vector.append(entry)
    return copy_of_vector
#@+node:gcross.20111014183107.1246: *3* CCW
def CCW(i):
    return (i+1)%4
#@+node:gcross.20111022200315.1278: *3* compressSelfConnectedTensor
def compressSelfConnectedTensor(tensor,index,keep=None,threshold=None):
    return truncateSelfConnectedTensor(tensor,index,constructFilterFrom(keep=keep,threshold=threshold))
#@+node:gcross.20111022200315.1309: *3* computeFilterFrom
def constructFilterFrom(keep=None,threshold=None):
    if keep is None and threshold is None or (keep is not None and threshold is not None):
        raise ValueError("either keep or threshold (but not both) needs to be specified")
    if keep is not None:
        return keep
    if threshold is not None:
        if threshold == 0:
            threshold = 1e-10
        return lambda arr: firstIndexBelowMagnitude(arr,threshold)
#@+node:gcross.20111009193003.3007: *3* crand
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
            return index
        index += 1
#@+node:gcross.20111022200315.1276: *3* firstIndexWithNonZeroMagnitude
def firstIndexWithNonZeroMagnitude(arr):
    return firstIndexBelowMagnitude(arr,1e-13)
#@+node:gcross.20111009135633.1131: *3* formContractor
def formContractor(order,joins,result_joins):
    observed_tensor_indices = {}

    #@+<< Tabulate all of the observed tensor indices >>
    #@+node:gcross.20111009135633.1132: *4* << Tabulate all of the observed tensor indices >>
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
    #@+node:gcross.20111009135633.1133: *4* << Check the observed tensor indices and initialize the map of tensor joins >>
    for (tensor_id,observed_indices) in observed_tensor_indices.items():
        tensor_dimension = max(observed_indices)+1
        expected_indices = set(range(tensor_dimension))
        invalid_indices = observed_indices - expected_indices
        missing_indices = expected_indices - observed_indices
        if len(invalid_indices) > 0:
            raise ValueError('the invalid indices {} have appeared in joins involving tendor {}'.format(invalid_indices,tensor_id))
        if len(missing_indices) > 0:
            raise ValueError('the expected indices {} no not appear in any of the joins for tensor {}'.format(missing_indices,tensor_id))
        if tensor_id not in order:
            raise ValueError('tensor {} does not appear in the list of arguments'.format(tensor_id))
        tensor_join_ids[tensor_id] = [None]*tensor_dimension
    #@-<< Check the observed tensor indices and initialize the map of tensor joins >>

    result_join_ids = []

    #@+<< Label each join with a unique id >>
    #@+node:gcross.20111009135633.1134: *4* << Label each join with a unique id >>
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
    #@+node:gcross.20111009135633.1136: *4* << Form the contractor function >>
    def contract(*arguments):
        if len(arguments) != len(order):
            raise ValueError("wrong number of arguments;  expected {} but received {}",len(order),len(arguments))
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
#@+node:gcross.20111017110141.1258: *3* increaseDimensionBetween
def increaseDimensionBetween(tensor_1,index_1,tensor_2,index_2,new_dimension):
    if tensor_1.shape[index_1] != tensor_2.shape[index_2]:
        raise ValueError("dimension {} of tensor 1 has different size than dimension {} ({} != {})".format(index_1,index_2,tensor_1.shape[index_1],tensor_2.shape[index_2]))
    old_dimension = tensor_1.shape[index_1]
    if new_dimension < old_dimension:
        raise ValueError("new dimension ({}) must be at least the old dimension ({})".format(new_dimension,old_dimension))
    if new_dimension == old_dimension:
        tensor_1, tensor_2
    matrix, _, _ = svd(crand(new_dimension,old_dimension),full_matrices=False)
    matrix = matrix.transpose()
    return (
        multiplyTensorByMatrixAtIndex(tensor_1,matrix,index_1),
        multiplyTensorByMatrixAtIndex(tensor_2,matrix.conj(),index_2),
    )
#@+node:gcross.20111017110141.1260: *3* increaseDimensionBetweenTensors
def increaseDimensionBetweenTensors(tensor_1,index_1,tensor_2,index_2,new_dimension):
    return mapFunctions(
        (type(tensor_1),type(tensor_2)),
        increaseDimensionBetween(tensor_1.data,index_1,tensor_2.data,index_2,new_dimension)
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
    return normalizeAndReturnInverseNormalizer(tensor,index)[0]
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

    u, s, v = svd(new_tensor,full_matrices=False)

    return inverseTransformation(u), s*v.transpose()
#@+node:gcross.20111014183107.1249: *3* OPP
def OPP(i):
    return (i+2)%4
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
#@+node:gcross.20111022200315.1265: *3* truncateSelfConnectedTensor
def truncateSelfConnectedTensor(tensor,index,filter):
    reshaped_tensor, inverseTransformation = transposeAndReshapeAndReturnInverseTransformation(tensor,index)
    evals, evecs = eigh(tensordot(reshaped_tensor,reshaped_tensor.conj(),(1,1)))
    sorted_indices = evals.argsort()[::-1]
    evals = evals[sorted_indices]
    evecs = evecs[:,sorted_indices]
    evals[evals < 0] = 0
    keep = filter(evals/evals[0] if evals[0] != 0 else evals) if callable(filter) else filter
    return inverseTransformation(sqrt(evals[:keep])*evecs[:,:keep])
#@+node:gcross.20111022200315.1270: *3* withoutIndex
def withoutIndex(vector,index):
    copy_of_vector = list(vector)
    if copy_of_vector is vector:
        copy_of_vector = copy(vector)
    del copy_of_vector[index]
    return copy_of_vector
#@-others

#@+<< Exports >>
#@+node:gcross.20111009193003.1160: ** << Exports >>
__all__ = [
    "appended",
    "CCW",
    "crand",
    "compressSelfConnectedTensor",
    "CW",
    "firstIndexBelowMagnitude",
    "formContractor",
    "increaseDimensionBetween",
    "increaseDimensionBetweenTensors",
    "mapFunctions",
    "multiplyTensorByMatrixAtIndex",
    "normalize",
    "normalizeAndDenormalize",
    "normalizeAndDenormalizeTensors",
    "normalizeAndReturnInverseNormalizer",
    "OPP",
    "transposeAndReshapeAndReturnInverseTransformation",
    "truncateSelfConnectedTensor",
    "withoutIndex",
]
#@-<< Exports >>
#@-leo
