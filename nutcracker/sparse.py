# Imports {{{
from collections import defaultdict, namedtuple
from itertools import izip
from numpy import array, ndarray, prod, zeros
import operator

from .miscellaneous.enum_meta import Enum
from .utils import invertPermutation
# }}}

# Enumerations {{{

class Sparsity(Enum):
    dense = "dense"
    sparse = "sparse"

# }}}

# Classes {{{

class SizedEntryContainer(object): # {{{
    def __init__(self,entries):
        self._entries = tuple(entries)
        self.size = reduce(operator.mul,(entry.size for entry in self),1)
        self.shape = tuple(index.size for index in self)
    def __getitem__(self,index):
        return self._entries[index]
    def __iter__(self):
        return iter(self._entries)
    def __len__(self):
        return len(self._entries)
# }}}

VirtualIndexEntry = namedtuple("VirtualIndexEntry",["index","size","sparsity"])

class VirtualIndex(SizedEntryContainer): # {{{
    @classmethod
    def withSingleEntry(cls,index,size,sparsity):  # {{{
        return cls((VirtualIndexEntry(index,size,sparsity),))
    # }}}
# }}}

class SparseDescriptor(SizedEntryContainer): # {{{
    def __init__(self,indices):
        super(self.__class__,self).__init__(indices)
        self.sparse_shape, self.dense_shape = (
            tuple(
                entry.size
                for index in self
                for entry in index
                if entry.sparsity == sparsity
            )
            for sparsity in (Sparsity.sparse,Sparsity.dense)
        )
        self.number_of_sparse_indices = len(self.sparse_shape)
        self.number_of_dense_indices = len(self.dense_shape)
        dense_index_offset = self.number_of_sparse_indices
        self.dense_form_indices = [
            entry.index + (0 if entry.sparsity == Sparsity.sparse else dense_index_offset)
            for index in self
            for entry in index
        ]
        self.dense_form_shape = [entry.size for index in self for entry in index]
        self.inverse_dense_form_indices = invertPermutation(self.dense_form_indices)
# }}}

class SparseData(SparseDescriptor): # {{{
    def __init__(self,index_table,matrix_table,sparse_descriptor=None,sparse_shape=None):
        if sparse_descriptor is None and sparse_shape is None:
            raise ValueError("Either the shape of the sparse indices or the virtual shape must be specified.")
        elif sparse_descriptor is not None:
            specified_shapes = {Sparsity.dense: matrix_table.shape}
            if sparse_shape is not None:
                specified_shapes[Sparsity.sparse] = sparse_shape
            for virtual_index in sparse_descriptor:
                for entry in virtual_index:
                    if entry.sparsity in specified_shapes:
                        specified_shape = specified_shapes[entry.sparsity]
                        if entry.index >= len(specified_shape):
                            raise ValueError("The virtual shape refers to index {} that does not exist in the specified shape of the {} indicies.".format(entry.index,entry.sparsity))
                        specified_size = specified_shape[size]
                        if entry.size != specified_size:
                            raise ValueError("The virtual shape says that {} index {} should have size {}, but the specified shape says it should have size {}".format(entry.sparsity,entry.index,entry.size,specified_size))
            if sparse_shape is None:
                sparse_shape = [None]*index_table.shape[0]
                for virtual_index in sparse_descriptor:
                    for entry in virtual_index:
                        if entry.sparsity == Sparsity.sparse:
                            if entry.index > len(sparse_shape):
                                raise ValueError("The virtual shape refers to an index that is greater than the number of sparse indices ({} >= {})".format(entry.index,len(sparse_shape)))
                            sparse_shape[entry.index] = entry.size
                for index, size in enumerate(sparse_shape):
                    if size is None:
                        raise ValueError("Index {} of the sparse shape is missing from the virtual shape.".format(index))
        else:
            sparse_descriptor = VirtualShape(
                [VirtualIndex(VirtualIndexEntry(index=index,size=size,sparsity=Sparsity.sparse)) for (index,size) in enumerate(sparse_shape)] +
                [VirtualIndex(VirtualIndexEntry(index=index,size=size,sparsity=Sparsity.dense)) for (index,size) in enumerate(matrix_table.shape)]
            )
        self.sparse_shape = sparse_shape
        self.shape = sparse_shape
        self.index_table = index_table
        self.matrix_table = matrix_table
# }}}

# }}}

# Functions {{{

def applyMatrixJoins(matrix_tables,matrix_join_lists,sparse_to_dense_axis_lists,dense_to_dense_axis_lists,remaining_axis_lists): # {{{
    if len(dense_to_dense_axis_lists[0]) != len(dense_to_dense_axis_lists[1]):
        raise ValueError("The list of dense-to-dense joined indices must have the same lengths. ({} != {})".format(len(x) for x in dense_to_dense_axis_lists))
    number_of_dense_to_dense_axes = len(dense_to_dense_axis_lists[0])
    joined_matrix_table = zeros(
        shape = (len(matrix_joins),) + sum(tuple(matrix_table.shape[i] for i in remaining_axis_list) for matrix_table, remaining_axis_list in izip(matrix_tables,remaining_axis_lists)),
        dtype = matrix_table.dtype,
    )
    transposed_matrix_tables = [
        matrix_tables[0].transpose(sparse_to_dense_axis_lists[0],remaining_axis_lists[0],dense_to_dense_axis_lists[0]),
        matrix_tables[1].transpose(sparse_to_dense_axis_lists[1],dense_to_dense_axis_lists[1],remaining_axis_lists[1]),
    ]
    for joined_matrix_row, matrix_join_list in izip(matrix_table,matrix_join_lists):
        for matrix_join in matrix_join_list:
            tensordot_arguments = []
            for (row_number, indices), transposed_matrix_table in izip(matrix_join):
                tensordot_arguments.append(transposed_matrix_table[row_number,indices,...])
            tensordot_arguments.append(number_of_dense_to_dense_axes)
            joined_matrix_row[...] += tensordot(*tensordot_arguments)
    return joined_matrix_table
# }}}

def computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable(index_tables,sparse_to_sparse_join_axis_lists,sparse_to_dense_join_axis_lists,remaining_axis_lists): # {{{
    SparseJoin = computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable.SparseJoin
    index_table_dtype = index_tables[0].dtype
    # Check that the index tables have the correct ranks {{{
    if [index_table.ndim for index_table in index_tables] != [2,2]:
        raise ValueError("Each index table must be a 2D array where each row (i.e. entry in the first dimension) supplies a list of indices in the sparse dimensions (ndims = ({},{}) != (2,2))".format(*[index_table.ndim for index_table in index_tables]))
    del index_table
    # }}}
    # Make sure that all of the lists are actually lists, since list slicing behaves differently from tuple slicing {{{
    sparse_to_sparse_join_axis_lists = map(list,sparse_to_sparse_join_axis_lists)
    sparse_to_dense_join_axis_lists = map(list,sparse_to_dense_join_axis_lists)
    remaining_axis_lists = map(list,remaining_axis_lists)
    # }}}
    # Compute the set of sparse-to-sparse indices that are shared {{{
    sparse_to_sparse_index_lists = [
        frozenset(
            tuple(row[sparse_to_sparse_join_axis_list])
            for row in index_table
        )
        for (index_table,sparse_to_sparse_join_axis_list) in izip(index_tables,sparse_to_sparse_join_axis_lists)
    ]
    sparse_to_sparse_index_filter = sparse_to_sparse_index_lists[0] & sparse_to_sparse_index_lists[1]
    if not sparse_to_sparse_index_filter:
        return zeros(shape=(0,sum(map(len,remaining_axis_lists))),dtype=index_table_dtype), []
    del sparse_to_sparse_index_lists
    # }}}
    # Create a map with all of the non-zero sparse terms {{{
    sparse_to_sparse_index_map = defaultdict(lambda: ([],[]))
    for tensor_number in xrange(2):
        sparse_to_sparse_join_axis_list = sparse_to_sparse_join_axis_lists[tensor_number]
        sparse_to_dense_join_axis_list = sparse_to_dense_join_axis_lists[tensor_number]
        remaining_axis_list = remaining_axis_lists[tensor_number]
        index_table = index_tables[tensor_number]
        for row_number, row in enumerate(index_table):
            sparse_indices = tuple(row[sparse_to_sparse_join_axis_list])
            if sparse_indices in sparse_to_sparse_index_filter:
                sparse_to_sparse_index_map[sparse_indices][tensor_number].append(
                    SparseJoin(
                        tuple(row[remaining_axis_list]),
                        row_number,
                        tuple(row[sparse_to_dense_join_axis_list])
                    )
                )
    del sparse_to_sparse_index_filter
    # }}}
    # Compute the joined index map {{{
    joined_index_map = defaultdict(lambda: [])
    for joins in sparse_to_sparse_index_map.itervalues():
        for left_join in joins[0]:
            for right_join in joins[1]:
                joined_index_map[left_join.remaining_indices + right_join.remaining_indices].append((
                    ( left_join.row_number, left_join.sparse_to_dense_join_indices),
                    (right_join.row_number,right_join.sparse_to_dense_join_indices),
                ))
    # }}}
    return array(joined_index_map.keys(),dtype=index_table_dtype), joined_index_map.values()
computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable.SparseJoin = namedtuple("SparseJoin",["remaining_indices","row_number","sparse_to_dense_join_indices"])
# }}}

def convertSparseToDense(index_table,matrix_table,sparse_descriptor): # {{{
    matrix = zeros(shape = sparse_descriptor.shape, dtype = matrix_table.dtype)
    transposed_matrix = matrix.reshape(sparse_descriptor.dense_form_shape).transpose(sparse_descriptor.inverse_dense_form_indices)
    for index_table_row, matrix_table_row in izip(index_table,matrix_table):
        transposed_matrix[tuple(index_table_row)] += matrix_table_row
    return matrix
# }}}

def reconcileSparseDescriptors(sparse_descriptor_1,sparse_descriptor_2): # {{{
    if sparse_descriptor_1.size != sparse_descriptor_2.size:
        raise ValueError("unable to reconcile group of size {} with group of size {}".format(sparse_descriptor_1.size,sparse_descriptor_1.size))
    Element = reconcileSparseDescriptors.Element
    groups = [map(Element.fromVirtualIndexEntry,reversed(sparse_descriptor)) for sparse_descriptor in [sparse_descriptor_1,sparse_descriptor_2]]
    reconciled_sparse_descriptors = [[],[]]
    sparse_descriptor_splits = [defaultdict(lambda: []) for _ in xrange(2)]
    while len(groups[0]) > 0:
        elements = [group.pop() for group in groups]
        for a, b in [(0,1),(1,0)]:
            if elements[a].size > elements[b].size:
                if elements[a].size % elements[b].size != 0:
                    raise ValueError("virtual shapes {} and {} cannot be reconciled into a common product of factors ({} % {} != 0)".format(sparse_descriptor_1.sizes(),sparse_descriptor_2.sizes(),elements[a].size,elements[b].size))
                groups[a].append(
                    Element(
                        index=elements[a].index,
                        subindex=elements[a].subindex+1,
                        size=elements[a].size/elements[b].size,
                        sparsity=elements[a].sparsity
                    )
                )
                elements[a].size = elements[b].size
        assert elements[0].size == elements[1].size
        for (element,reconciled_sparse_descriptor,sparse_descriptor_split) in izip(elements,reconciled_sparse_descriptors,sparse_descriptor_splits):
            reconciled_sparse_descriptor.append(VirtualIndexEntry(
                index=(element.index,element.subindex),
                size=element.size,
                sparsity=element.sparsity
            ))
            sparse_descriptor_split[element.index].append(element.size)
    for group in groups:
        assert len(group) == 0
    reconciled_sparse_descriptors = map(VirtualIndex,reconciled_sparse_descriptors)
    assert reconciled_sparse_descriptors[0].size == reconciled_sparse_descriptors[1].size
    return reconciled_sparse_descriptors, sparse_descriptor_splits

class reconcileSparseDescriptors_Element(object):
    def __init__(self,index,subindex,size,sparsity):
        self.index = index
        self.subindex = subindex
        self.size = size
        self.sparsity = sparsity
    @classmethod
    def fromVirtualIndexEntry(cls,sparse_descriptor_entry):
        return cls(
            index = sparse_descriptor_entry.index,
            subindex = 0,
            size = sparse_descriptor_entry.size,
            sparsity = sparse_descriptor_entry.sparsity,
        )
reconcileSparseDescriptors.Element = reconcileSparseDescriptors_Element
del reconcileSparseDescriptors_Element

# }}}

# }}}

# Exports {{{
__all__ = [
    "SparseDescriptor",
    "Sparsity",
    "VirtualIndex",
    "VirtualIndexEntry",

    "computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable",
    "convertSparseToDense",
    "reconcileSparseDescriptors",
]
# }}}
