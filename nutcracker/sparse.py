# Imports {{{
from collections import defaultdict, namedtuple
from itertools import izip
from numpy import array, ndarray, prod, uint32, zeros
import operator
# }}}

# Classes {{{

class SizedEntryContainer(object): # {{{
    def __init__(self,entries):
        self._entries = tuple(entries)
        self.size = reduce(operator.mul,(entry.size for entry in self),1)
    def __getitem__(self,index):
        return self._entries[index]
    def __iter__(self):
        return iter(self._entries)
    def __len__(self):
        return len(self._entries)
# }}}

VirtualIndexEntry = namedtuple("VirtualIndexEntry",["index","size","sparsity"])

class VirtualIndex(SizedEntryContainer): # {{{
    pass
# }}}

class VirtualShape(SizedEntryContainer): # {{{
    def __init__(self,indices):
        super(SizedEntryContainer,self)(indices)
        self.shape = tuple(index.size for index in self)
# }}}

class SparseData(object): # {{{
    def __init__(self,index_table,matrix_table,virtual_shape=None,sparse_shape=None):
        if virtual_shape is None and sparse_shape is None:
            raise ValueError("Either the shape of the sparse indices or the virtual shape must be specified.")
        elif virtual_shape is not None:
            specified_shapes = {Sparsity.dense: matrix_table.shape}
            if sparse_shape is not None:
                specified_shapes[Sparsity.sparse] = sparse_shape
            for virtual_index in virtual_shape:
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
                for virtual_index in virtual_shape:
                    for entry in virtual_index:
                        if entry.sparsity == Sparsity.sparse:
                            if entry.index > len(sparse_shape):
                                raise ValueError("The virtual shape refers to an index that is greater than the number of sparse indices ({} >= {})".format(entry.index,len(sparse_shape)))
                            sparse_shape[entry.index] = entry.size
                for index, size in enumerate(sparse_shape):
                    if size is None:
                        raise ValueError("Index {} of the sparse shape is missing from the virtual shape.".format(index))
        else:
            virtual_shape = VirtualShape(
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

def computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable(index_tables,sparse_to_sparse_join_axis_lists,sparse_to_dense_join_axis_lists,remaining_axis_lists): # {{{
    SparseJoin = computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable.SparseJoin
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
        for (index_table,sparse_to_sparse_join_axis_list) in zip(index_tables,sparse_to_sparse_join_axis_lists)
    ]
    sparse_to_sparse_index_filter = sparse_to_sparse_index_lists[0] & sparse_to_sparse_index_lists[1]
    if not sparse_to_sparse_index_filter:
        return zeros(shape=(0,sum(map(len,remaining_axis_lists))),dtype=uint32), []
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
    return array(joined_index_map.keys(),dtype=uint32), joined_index_map.values()
computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable.SparseJoin = namedtuple("SparseJoin",["remaining_indices","row_number","sparse_to_dense_join_indices"])
# }}}

def reconcileVirtualIndices(virtual_shape_1,virtual_shape_2): # {{{
    if virtual_shape_1.size != virtual_shape_2.size:
        raise ValueError("unable to reconcile group of size {} with group of size {}".format(virtual_shape_1.size,virtual_shape_1.size))
    groups = [map(reconcileVirtualIndices.Element.fromVirtualIndexEntry,reversed(virtual_shape)) for virtual_shape in [virtual_shape_1,virtual_shape_2]]
    reconciled_virtual_shapes = [[],[]]
    virtual_shape_splits = [defaultdict(lambda: []) for _ in xrange(2)]
    while len(groups[0]) > 0:
        elements = [group.pop() for group in groups]
        for a, b in [(0,1),(1,0)]:
            if elements[a].size > elements[b].size:
                if elements[a].size % elements[b].size != 0:
                    raise ValueError("virtual shapes {} and {} cannot be reconciled into a common product of factors ({} % {} != 0)".format(virtual_shape_1.sizes(),virtual_shape_2.sizes(),elements[a].size,elements[b].size))
                groups[a].append(
                    reconcileVirtualIndices.Element(
                        index=elements[a].index,
                        subindex=elements[a].subindex+1,
                        size=elements[a].size/elements[b].size,
                        sparsity=elements[a].sparsity
                    )
                )
                elements[a].size = elements[b].size
        assert elements[0].size == elements[1].size
        for (element,reconciled_virtual_shape,virtual_shape_split) in izip(elements,reconciled_virtual_shapes,virtual_shape_splits):
            reconciled_virtual_shape.append(VirtualIndexEntry(
                index=(element.index,element.subindex),
                size=element.size,
                sparsity=element.sparsity
            ))
            virtual_shape_split[element.index].append(element.size)
    for group in groups:
        assert len(group) == 0
    reconciled_virtual_shapes = map(VirtualIndex,reconciled_virtual_shapes)
    assert reconciled_virtual_shapes[0].size == reconciled_virtual_shapes[1].size
    return reconciled_virtual_shapes, virtual_shape_splits

class reconcileVirtualIndices_Element(object):
    def __init__(self,index,subindex,size,sparsity):
        self.index = index
        self.subindex = subindex
        self.size = size
        self.sparsity = sparsity
    @classmethod
    def fromVirtualIndexEntry(cls,virtual_shape_entry):
        return cls(
            index = virtual_shape_entry.index,
            subindex = 0,
            size = virtual_shape_entry.size,
            sparsity = virtual_shape_entry.sparsity,
        )
reconcileVirtualIndices.Element = reconcileVirtualIndices_Element
del reconcileVirtualIndices_Element

# }}}

# }}}

# Exports {{{
__all__ = [
    "VirtualIndex",
    "VirtualIndexEntry",

    "computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable",
    "reconcileVirtualIndices",
]
# }}}
