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
    def __init__(self,indices): # {{{
        SizedEntryContainer.__init__(self,indices)
        number_of_indices_of_kind = defaultdict(lambda: 0)
        for index in self:
            for entry in index:
                number_of_indices_of_kind[entry.sparsity] += 1
        shape_of_kind = {sparsity: [None]*number_of_indices_of_kind[sparsity] for sparsity in Sparsity}
        for index in self:
            for entry in index:
                try:
                    shape_of_kind[entry.sparsity][entry.index] = entry.size
                except IndexError:
                    raise ValueError("raw {} index {} is greater than the number of indices of that kind".format(entry.sparsity,entry.index))
        for sparsity, shape, in shape_of_kind.iteritems():
            missing_indices = set()
            for index, size in enumerate(shape):
                if size is None:
                    missing_indices.add(index)
            if missing_indices:
                raise ValueError("the following {} indices are missing: {}".format(sparsity,missing_indices))
        for sparsity in Sparsity:
            shape_of_kind[sparsity] = tuple(shape_of_kind[sparsity])
        self.number_of_indices_of_kind = number_of_indices_of_kind
        self.shape_of_kind = shape_of_kind
    # }}}

    def computeRawFormIndices(self): # {{{
        dense_index_offset = self.number_of_indices_of_kind[Sparsity.sparse]
        return [
            entry.index + (0 if entry.sparsity == Sparsity.sparse else dense_index_offset)
            for index in self
            for entry in index
        ]
    # }}}

    def transposeAndReshapeDenseToRaw(self,matrix): # {{{
        raw_dense_form_shape = [entry.size for index in self for entry in index]
        inverse_raw_dense_form_indices = invertPermutation(self.computeRawFormIndices())
        return matrix.reshape(raw_dense_form_shape).transpose(inverse_raw_dense_form_indices)
    # }}}

    def reconcileWith(self,other): # {{{
        return reconcileSparseDescriptors(self,other)
    # }}}
# }}}

class SparseData(SparseDescriptor): # {{{
    def __init__(self,index_table,matrix_table,sparse_descriptor): # {{{
        SparseDescriptor.__init__(self,sparse_descriptor)
        self.index_table = index_table
        self.matrix_table = matrix_table
    # }}}

    def convertToDense(self): # {{{
        matrix = zeros(shape = self.shape, dtype = self.matrix_table.dtype)
        transposed_matrix = self.transposeAndReshapeDenseToRaw(matrix)
        for index_table_row, matrix_table_row in izip(self.index_table,self.matrix_table):
            transposed_matrix[tuple(index_table_row)] += matrix_table_row
        return matrix
    # }}}
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

def reconcileSparseDescriptors(sparse_descriptors,axes_to_join): # {{{
    indices = [list(sparse_descriptor) for sparse_descriptor in sparse_descriptors]
    splits = [defaultdict(lambda: defaultdict(lambda: [])) for _ in xrange(2)]
    observed_axes_list = [set() for _ in xrange(2)]
    splits = [{} for _ in xrange(2)]
    for axes in zip(*axes_to_join):
        for i, (axis, observed_axes) in enumerate(izip(axes,observed_axes_list)):
            if axis in observed_axes:
                raise ValueError("the join axis {} appears twice in tensor {}".format(i,axis))
            observed_axes.add(axis)
        (indices[0][axis_0], indices[1][axis_1]), new_splits = reconcileVirtualIndices(indices[0][axis_0],indices[1][axis_1])
        for split, new_split in izip(splits,new_splits):
            split.update(new_split)
    return map(tuple,indices), splits
    # }}}

def reconcileVirtualIndices(virtual_index_1,virtual_index_2): # {{{
    if virtual_index_1.size != virtual_index_2.size:
        raise ValueError("unable to reconcile index of size {} with index of size {}".format(virtual_index_1.size,virtual_index_1.size))
    Element = reconcileVirtualIndices.Element
    groups = [map(Element.fromVirtualIndexEntry,reversed(virtual_index)) for virtual_index in [virtual_index_1,virtual_index_2]]
    reconciled_virtual_indices = [[],[]]
    virtual_index_splits = [defaultdict(lambda: defaultdict(lambda: [])) for _ in xrange(2)]
    while len(groups[0]) > 0:
        elements = [group.pop() for group in groups]
        # Check whether the overlapping virtual index entries need to be split {{{
        for a, b in [(0,1),(1,0)]:
            if elements[a].size > elements[b].size:
                if elements[a].size % elements[b].size != 0:
                    raise ValueError("virtual shapes {} and {} cannot be reconciled into a common product of factors ({} % {} != 0)".format(virtual_index_1.sizes(),virtual_index_2.sizes(),elements[a].size,elements[b].size))
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
        # }}}
        for (element,reconciled_virtual_index,virtual_index_split) in izip(elements,reconciled_virtual_indices,virtual_index_splits):
            reconciled_virtual_index.append(VirtualIndexEntry(
                index=(element.index,element.subindex),
                size=element.size,
                sparsity=element.sparsity
            ))
            virtual_index_split[element.sparsity][element.index].append(element.size)
    for group in groups:
        assert len(group) == 0
    reconciled_virtual_indices = map(VirtualIndex,reconciled_virtual_indices)
    assert reconciled_virtual_indices[0].size == reconciled_virtual_indices[1].size
    return reconciled_virtual_indices, virtual_index_splits

class reconcileVirtualIndices_Element(object):
    def __init__(self,index,subindex,size,sparsity):
        self.index = index
        self.subindex = subindex
        self.size = size
        self.sparsity = sparsity
    @classmethod
    def fromVirtualIndexEntry(cls,virtual_index_entry):
        return cls(
            index = virtual_index_entry.index,
            subindex = 0,
            size = virtual_index_entry.size,
            sparsity = virtual_index_entry.sparsity,
        )
reconcileVirtualIndices.Element = reconcileVirtualIndices_Element
del reconcileVirtualIndices_Element

# }}}

# }}}

# Exports {{{
__all__ = [
    "SparseData",
    "SparseDescriptor",
    "Sparsity",
    "VirtualIndex",
    "VirtualIndexEntry",

    "computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable",
    "reconcileVirtualIndices",
]
# }}}
