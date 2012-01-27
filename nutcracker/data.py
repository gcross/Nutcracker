# Imports {{{
from itertools import izip
# }}}

# Classes {{{

class Data(object): # {{{
    def __init__(self,index_table,matrix_table,sparse_shape,inverse_transposition=None): # {{{
        self.ndim = matrix_table.ndim-1
        if inverse_transposition is None:
            inverse_transposition = range(self.ndim)
        self.index_table = index_table
        self.matrix_table = matrix_table
        self.sparse_shape = sparse_shape
        self.dense_shape = matrix_table.shape
        self.shape = tuple(x*y for x,y in izip(sparse_shape,matrix_table.shape))
        self.inverse_transposition = inverse_transposition
        self.number_of_terms = index_table.shape[0]
    # }}}
    def join(self,group_sizes): # {{{
        assert sum(group_sizes) == self.ndim
        new_ndim = len(group_sizes)
        dense_shape = self.dense_shape
        sparse_shape = self.sparse_shape
        index_table = self.index_table
        inverse_permutation = self.inverse_permutation
        new_dense_shape = []
        new_spase_shape = []
        current_index = 0
        new_index_table = zeros(shape=(self.number_of_terms,new_ndim),dtype=self.index_table.dtype)
        for new_index, group_size in enumerate(group_sizes):
            sparse_dimension = 1
            dense_dimension = 1
            for old_index in (inverse_permutation[i] for i in xrange(current_index+group_size-1,current_index-1,-1)):
                new_index_table[:,new_index] += sparse_dimension * index_table[:,old_index]
                sparse_dimension *= sparse_shape[old_index]
                dense_dimension *= dense_shape[old_index]
            new_dense_shape.append(dense_dimension)
            new_sparse_shape.append(sparse_dimension)
            current_index += group_size
        self.ndim = new_ndim
        self.dense_shape = tuple(new_dense_shape)
        self.sparse_shape = tuple(new_sparse_shape)
        self.shape = tuple(x*y for x,y in izip(self.dense_shape,self.sparse_shape))
        self.index_table = new_index_table
        self.matrix_table = self.matrix_table.transpose([0] + [1+i for i in inverse_transposition]).reshape([self.number_of_terms] + new_dense_shape)
        self.inverse_permutation = range(new_ndim)
    # }}}
    def transpose(self,transposition): # {{{
        self.inverse_transposition = applyPermutationTo(self.inverse_transposition,invertPermutation(transposition))
    # }}}
    def transposeAndJoin(self,groups): # {{{
        return self.transpose([element for group in groups for element in group]).join([len(group) for group in groups])
    # }}}
    def toArray(self): # {{{
        ndim = self.ndim
        sparse_shape = self.sparse_shape
        dense_shape = self.dense_shape
        pre_join_shape = []
        for i in xrange(ndim):
            pre_join_shape.append(sparse_shape[i])
            pre_join_shape.append(dense_shape[i])
        array = zeros(shape=pre_join_shape,dtype=self.matrix_table.dtype)
        array_view_with_sparse_indices_first = array.transpose([i*2 for i in xrange(ndim)] + [i*2+1 for i in xrange(ndim)])
        for indices, values in izip(self.index_table,self.matrix_table):
            array_view_with_sparse_indices_first[indices] += values
        return array.reshape(self.shape).transpose(invertPermutation(self.inverse_permutation))
    # }}}
# }}}

# }}}

# Functions {{{

def contract(data_objects,axes_to_join): # {{{
    assert axes_to_join[0] == axes_to_join[1]
    number_of_axes_to_join = len(axes_to_join[0])
    # Compute the remaining (non-joined) axes {{{
    remaining_axes = [list(frozenset(xrange(data_objects[i].ndim)) - frozenset(axes_to_join[i])) for i in xrange(2)]
    number_of_remaining_axes = 0
    for axes in remaining_axes:
        axes.sort()
        number_of_remaining_axes += len(axes)
    number_of_first_tensor_remaining_axes = len(remaining_axes[0])
    # }}}
    # Apply an inverse transformation to "undo" the transposition associated with this tensor so that the axes {{{
    # are expressed in terms of the original axis ordering of this tensor.
    original_axes_to_join = axes_to_join
    original_remaining_axes = remaining_axes
    axes_to_join = []
    remaining_axes = []
    for i in xrange(2):
        inverse_transposition = data_objects[i].inverse_transposition
        axes_to_join.append([inverse_transposition[axis] for axis in original_axes_to_join[i]])
        remaining_axes.append([inverse_transposition[axis] for axis in original_remaining_axes[i]])
    # }}}
    # Flatten the sparse indices that are being joined {{{
    flattened_index_tables = []
    for i in xrange(2):
        sparse_tensor = data_objects[i]
        shape = data_objects.sparse_shape
        index_table = data_objects.index_table
        flattened_index_table = zeros(shape=index_table.shape[0],dtype=uint64)
        current_stride = 1
        for axis in axes_to_join[i]:
            flattened_index_table += current_stride * index_table[:,axis]
            current_stride *= shape[axis]
        flattened_index_tables.append(flattened_index_table)
    # }}}
    # Compute the sparse indices that are participating in the sum {{{
    participating_flat_sparse_indices = frozenset(flattened_index_tables[0]) & frozenset(flattened_index_tables[1])
    # }}}
    # Construct a table with all of the terms that share matching sparse indices {{{
    join_table = defaultdict(lambda: tuple(defaultdict(lambda: []) for _ in xrange(2)))
    remaining_axes_indices_maps = []
    for i in xrange(2):
        sparse_tensor = data_objects[i]
        shape = sparse_tensor.sparse_shape
        number_of_sparse_indices = sparse_tensor.number_of_sparse_indices
        index_table = data_objects.index_table
        # Construct a vector that allows us to flatten the remaining sparse indices {{{
        remaining_sparse_axes = []
        remaining_sparse_axes_vector = zeros(index_table.shape(1),dtype=index_table.dtype)
        current_stride = 1
        for axis in remaining_axes[i]:
            remaining_sparse_axes.append(axis)
            remaining_sparse_axes_vector[axis] = current_stride
            current_stride *= shape[axis]
        # }}}
        remaining_axes_indices_map = {}
        # Iterate through the rows to tabulate which are participating in the sum {{{
        for row_number, row_flat_sparse_index in enumerate(flattened_index_tables[i]):
            if row_flat_sparse_index in participating_flat_sparse_indices:
                row = index_table[row_number]
                row_flat_remaining_index = inner(row,remaining_indices_vector)
                if row_flat_remaining_index not in remaining_axes_indices_map:
                    remaining_axes_indices_map[row_flat_remaining_index] = row[remaining_sparse_axes]
                join_table[row_flat_sparse_index][i][flat_remaining_row_index].append(row_number)
        # }}}
        remaining_axes_indices_maps.append(remaining_axes_indices_map)
    # }}}
    # Construct a table mapping flat remaining sparse indices to rows in the result {{{
    flat_index_map = {}
    flat_index_tables = ([],[])
    current_row_number = 0
    for left_terms, right_terms in join_table.itervalues():
        for left_flat_index in left_terms.iterkeys():
            for right_flat_index in right_terms.iterkeys():
                flat_index_tables[0].append(left_flat_index)
                flat_index_tables[1].append(right_flat_index)
                flat_index = (left_flat_index,right_flat_index)
                if flat_index in flat_index_map:
                    flat_index_map[flat_index] = current_row_number
                    current_row_number += 1
    number_of_rows = current_row_number
    # }}}
    # Construct the index table {{{
    index_table = ndarray(shape=(number_of_rows,number_of_remaining_axes),dtype=data_objects[0].index_table.dtype)
    for index_table_section, remaining_axes_indices_map, flat_index_table in izip(hsplit(index_table,[number_of_first_tensor_remaining_axes]),remaining_axes_indices_maps,flat_index_tables):
        for row_number, flat_index in enumerate(flat_index_table):
            index_table_section[row_number] = remaining_axes_indices_map[flat_index]
    # }}}
    # Construct the matrix table {{{
    matrix_table_shape = [number_of_rows]
    for i in xrange(2):
        for axis in remaining_axes:
            matrix_table_shape.append(data_objects[i].dense_shape[axis])
    matrix_table = zeros(shape=matrix_table_shape,dtype=data_objects[0].matrix_table.dtype)
    del matrix_table_shape
    left_input_matrix_table = data_objects[0].matrix_table.transpose([0] + [i+1 for i in remaining_axes[0] + axes_to_join[0]])
    right_input_matrix_table = data_objects[1].matrix_table.transpose([0] + [i+1 for i in axes_to_join[1] + remaining_axes[1]])
    for left_terms, right_terms in join_table.itervalues():
        for left_flat_index, left_input_row_number in left_terms.iteritems():
            left_input_row = left_input_matrix_table[left_input_row_number]
            for right_flat_index, right_input_row in right_terms.iteritems():
                right_input_row = right_input_matrix_table[right_input_row_number]
                output_row_number = flat_index_map[(left_flat_index,right_flat_index)]
                matrix_table[output_row_number] += tensordot(left_input_row,right_input_row,number_of_axes_to_join)
    # }}}
    # Compute the new inverse transposition {{{
    transposition_offset = 0
    inverse_transposition = []
    for axes in remaining_axes:
        axis_map = {old_axis:new_axis+transposition_offset for new_axis,old_axis in enumerate(sorted(axes))}
        for axis in axes:
            inverse_transposition.append(axis_map[axis])
        transposition_offset += len(axis_map)
    # }}}
    # Compute the new sparse shape {{{
    sparse_shape = sum([data_objects[i].sparse_shape[axis] for axis in remaining_axes[i]] for i in xrange(2)
    # }}}
# }}}

def contractAndTranspose(data_objects,axes_to_join,new_order): # {{{
    return contract(data_objects,axes_to_join).transpose(new_order)
# }}}

def contractAndTransposeAndJoin(data_objects,axes_to_join,new_grouped_order): # {{{
    return contract(data_objects,axes_to_join).transposeAndJoin(new_grouped_order)
# }}}

# }}}
