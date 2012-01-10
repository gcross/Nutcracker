# Imports {{{
from collections import namedtuple
from numpy import tensordot

from .utils import computePostContractionIndexMap
# }}}

# Types {{{
TensorNumberAndIndex = namedtuple("TensorNumberAndIndex",["tensor","index"])
# }}}

# Functions {{{

def contract(arrays,array_axes): # {{{
    return tensordot(data_1,data_2,(axes_1,axes_2))
# }}}

def contractAndTranspose(arrays,array_axes,new_order): # {{{
    return contractAndTransposeAndJoin(arrays,array_axes,[[x] for x in new_order])
# }}}

def contractAndTransposeAndJoin(arrays,array_axes,new_grouped_order): # {{{
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

# Exports {{{

__all__ = [
    "TensorNumberAndIndex",

    "contract",
    "contractAndTranspose",
    "contractAndTransposeAndJoin",
]

# }}}
