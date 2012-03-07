# Imports {{{
from itertools import izip
from numpy import *
from paycheck import *
from random import *

from . import *
from ..data import *
from ..utils import *
# }}} Imports 

# Tests {{{
class test_SparseData(TestCase): # {{{
    @with_checker # test___iadd__ {{{
    def test___iadd__(self,sparse_ndim=irange(0,5),nested_ndim=irange(0,5)):
        sparse_shape = randomShape(sparse_ndim)
        nested_shape = randomShape(nested_ndim)
        sparse_data_1 = SparseData.newRandom(NDArrayData,sparse_shape,nested_shape)
        sparse_data_2 = SparseData.newRandom(NDArrayData,sparse_shape,nested_shape)
        correct_result = sparse_data_1.toArray() + sparse_data_2.toArray()
        sparse_data_1 += sparse_data_2
        actual_result = sparse_data_1.toArray()
        self.assertAllClose(correct_result,actual_result)
    # }}}
    @with_checker # test_toArray {{{
    def test_toArray(self,sparse_ndim=irange(0,5),nested_ndim=irange(0,5),number_of_terms=irange(0,20)):
        ndim = sparse_ndim + nested_ndim
        sparse_shape = randomShape(sparse_ndim)
        nested_shape = randomShape(nested_ndim)
        index_to_data_map = {}
        correct_result = zeros(shape = sparse_shape + nested_shape, dtype=complex128)
        for _ in xrange(number_of_terms):
            index = tuple(randint(0,dimension-1) for dimension in sparse_shape)
            data = NDArrayData.newRandom(nested_shape)
            correct_result[index] += data.toArray()
            if index in index_to_data_map:
                index_to_data_map[index] += data
            else:
                index_to_data_map[index] = data
        transposition = randomPermutation(ndim)
        correct_result = correct_result.transpose(transposition)
        sparse_result = SparseData(sparse_shape,index_to_data_map,nested_shape = None if number_of_terms > 0 else nested_shape).transpose(transposition).toArray()
        self.assertAllClose(correct_result,sparse_result)
    # }}}
    @with_checker # test_contractWith_no_sparse_axes {{{
    def dont_test_contractWith_no_sparse_axes(self,ndimA=irange(0,5),ndimB=irange(0,5)):
        left_ndim = min(ndimA,ndimB)
        right_ndim = max(ndimA,ndimB)
        number_of_axes_to_sum = randint(0,left_ndim)
        left_transposition = randomPermutation(left_ndim)
        left_sum_axes = [left_transposition.index(axis) for axis in xrange(number_of_axes_to_sum)]
        right_transposition = randomPermutation(right_ndim)
        right_sum_axes = [right_transposition.index(axis) for axis in xrange(number_of_axes_to_sum)]
        right_shape = randomShape(right_ndim)
        left_shape = right_shape[:left_ndim]
        left_ndarray_data = NDArrayData.newRandom(left_shape).transpose(left_transposition)
        right_ndarray_data = NDArrayData.newRandom(right_shape).transpose(right_transposition)
        left_sparse_data = SparseData((),{():left_ndarray_data})
        right_sparse_data = SparseData((),{():right_ndarray_data})
        self.assertAllClose(
            left_sparse_data.contractWith(right_sparse_data,left_sum_axes,right_sum_axes).toArray(),
            left_ndarray_data.contractWith(right_ndarray_data,left_sum_axes,right_sum_axes).toArray(),
        )
    # }}}
    @with_checker # test_contractWith_single_sparse_axis # {{{
    def dont_test_contractWith_single_sparse_axis(self,
        dimension=irange(100,1000),
        number_of_shared_terms=irange(0,20),
        number_of_left_exclusive_terms=irange(0,20),
        number_of_right_exclusive_terms=irange(0,20),
    ):
        left_index_to_data_map = {}
        right_index_to_data_map = {}
        correct_result = 0
        generated_indices = set()
        index = None
        generated_indices.add(indices)
        for _ in xrange(number_of_shared_terms):
            while index in generated_indices:
                index = randint(0,dimension-1)
            left_value = crand()
            left_index_to_data_map[(index,)] = ScalarData(left_value)
            right_value = crand()
            right_index_to_data_map[(index,)] = ScalarData(right_value)
            correct_result += left_value*right_value
            generated_indices.add(index)
        for _ in xrange(number_of_left_exclusive_terms):
            while index in generated_indices:
                index = randint(0,dimension-1)
            left_value = crand()
            left_index_to_data_map[(index,)] = ScalarData(left_value)
            generated_indices.add(index)
        for _ in xrange(number_of_right_exclusive_terms):
            while index in generated_indices:
                index = randint(0,dimension-1)
            right_value = crand()
            right_index_to_data_map[(index,)] = ScalarData(right_value)
            generated_indices.add(index)
        left_sparse_data = SparseData((dimension,),left_index_to_data_map,nested_shape=())
        right_sparse_data = SparseData((dimension,),right_index_to_data_map,nested_shape=())
        self.assertAllClose(
            correct_result,
            left_sparse_data.contractWith(right_sparse_data,[0],[0]).toArray()
        )
    # }}}
    @with_checker # test_contractWith_sparse_only_matrix_multiplication # {{{
    def test_contractWith_sparse_only_matrix_multiplication(self,
        left_dimension=irange(100,1000),
        middle_dimension=irange(100,1000),
        right_dimension=irange(100,1000),
        number_of_shared_terms=irange(0,20),
        number_of_left_exclusive_terms=irange(0,20),
        number_of_right_exclusive_terms=irange(0,20),
        transpose_left=bool,
        transpose_right=bool,
        transpose_result=bool,
    ):
        left_index_to_data_map = {}
        right_index_to_data_map = {}
        correct_result = 0
        generated_middle_indices = set()
        middle_index = None
        generated_middle_indices.add(middle_index)
        correct_result = zeros((left_dimension,right_dimension),dtype=complex128)
        for _ in xrange(number_of_shared_terms):
            while middle_index in generated_middle_indices:
                middle_index = randint(0,middle_dimension-1)
            generated_middle_indices.add(middle_index)
            left_index = randint(0,left_dimension-1)
            left_value = crand()
            left_index_to_data_map[(left_index,middle_index)] = ScalarData(left_value)
            right_index = randint(0,right_dimension-1)
            right_value = crand()
            right_index_to_data_map[(middle_index,right_index)] = ScalarData(right_value)
            correct_result[left_index,right_index] += left_value*right_value
        for _ in xrange(number_of_left_exclusive_terms):
            while middle_index in generated_middle_indices:
                middle_index = randint(0,middle_dimension-1)
            generated_middle_indices.add(middle_index)
            left_index = randint(0,left_dimension-1)
            left_value = crand()
            left_index_to_data_map[(left_index,middle_index)] = ScalarData(left_value)
        for _ in xrange(number_of_right_exclusive_terms):
            while middle_index in generated_middle_indices:
                middle_index = randint(0,middle_dimension-1)
            generated_middle_indices.add(middle_index)
            right_index = randint(0,right_dimension-1)
            right_value = crand()
            right_index_to_data_map[(middle_index,right_index)] = ScalarData(right_value)
        left_sparse_data = SparseData((left_dimension,middle_dimension),left_index_to_data_map,nested_shape=())
        if transpose_left:
            left_sparse_data = left_sparse_data.transpose((1,0))
            left_sum_axis = 0
        else:
            left_sum_axis = 1
        right_sparse_data = SparseData((middle_dimension,right_dimension),right_index_to_data_map,nested_shape=())
        if transpose_right:
            right_sparse_data = right_sparse_data.transpose((1,0))
            right_sum_axis = 1
        else:
            right_sum_axis = 0
        if transpose_result:
            correct_result = correct_result.transpose()
            actual_result = right_sparse_data.contractWith(left_sparse_data,[right_sum_axis],[left_sum_axis]).toArray()
        self.assertAllClose(
            correct_result,
            actual_result,
        )
    # }}}
    @with_checker # test_contractWith_no_nested_axes # {{{
    def dont_test_contractWith_no_nested_axes(self,
        left_ndim=irange(0,5),
        right_ndim=irange(0,5),
        number_of_shared_terms=irange(0,20),
        number_of_left_exclusive_terms=irange(0,20),
        number_of_right_exclusive_terms=irange(0,20),
    ):
        number_of_axes_to_sum = randint(0,min(left_ndim,right_ndim))
        shared_shape = randomShape(number_of_axes_to_sum)
        left_exclusive_ndim = left_ndim-number_of_axes_to_sum
        left_exclusive_shape = randomShape(left_exclusive_ndim)
        left_shape = shared_shape + left_exclusive_shape
        left_index_to_data_map = {}
        right_exclusive_ndim = right_ndim-number_of_axes_to_sum
        right_exclusive_shape = randomShape(right_exclusive_ndim)
        right_shape = shared_shape + right_exclusive_shape
        right_index_to_data_map = {}
        correct_result = zeros(left_exclusive_shape + right_exclusive_shape,dtype=complex128)
        generated_shared_indices = set()
        shared_indices = None
        generated_shared_indices.add(shared_indices)
        for _ in xrange(number_of_shared_terms):
            shared_indices = randomIndices(shared_shape)
            if shared_indices in generated_shared_indices:
                continue
            left_exclusive_indices = randomIndices(left_exclusive_shape)
            left_value = crand()
            left_index_to_data_map[shared_indices + left_exclusive_indices] = ScalarData(left_value)
            right_exclusive_indices = randomIndices(right_exclusive_shape)
            right_value = crand()
            right_index_to_data_map[shared_indices + right_exclusive_indices] = ScalarData(right_value)
            correct_result[left_exclusive_indices + right_exclusive_indices] = left_value*right_value
            generated_shared_indices.add(shared_indices)
        for _ in xrange(number_of_left_exclusive_terms):
            shared_indices = randomIndices(shared_shape)
            if shared_indices in generated_shared_indices:
                continue
            left_exclusive_indices = randomIndices(left_exclusive_shape)
            left_value = crand()
            left_index_to_data_map[shared_indices + left_exclusive_indices] = ScalarData(left_value)
            generated_shared_indices.add(shared_indices)
        for _ in xrange(number_of_right_exclusive_terms):
            shared_indices = randomIndices(shared_shape)
            if shared_indices in generated_shared_indices:
                continue
            right_exclusive_indices = randomIndices(right_exclusive_shape)
            right_value = crand()
            right_index_to_data_map[shared_indices + right_exclusive_indices] = ScalarData(right_value)
            generated_shared_indices.add(shared_indices)
        #left_permutation = randomPermutation(left_ndim)
        #left_index_to_data_map = {tuple(applyPermutation(left_permutation,indices)): data for (indices,data) in left_index_to_data_map.iteritems()}
        #left_inverse_permutation = invertPermutation(left_permutation)
        #left_sum_axes = left_inverse_permutation[:number_of_axes_to_sum]
        #left_shape = tuple(applyPermutation(left_permutation,left_shape))
        #right_permutation = randomPermutation(right_ndim)
        #right_index_to_data_map = {tuple(applyPermutation(right_permutation,indices)): data for (indices,data) in right_index_to_data_map.iteritems()}
        #right_inverse_permutation = invertPermutation(right_permutation)
        #right_sum_axes = right_inverse_permutation[:number_of_axes_to_sum]
        #right_shape = tuple(applyPermutation(right_permutation,right_shape))
        left_sparse_data = SparseData(left_shape,left_index_to_data_map,nested_shape=())
        right_sparse_data = SparseData(right_shape,right_index_to_data_map,nested_shape=())
        left_sum_axes = range(left_exclusive_ndim)
        right_sum_axes = range(right_exclusive_ndim)
        print correct_result
        print left_sparse_data.contractWith(right_sparse_data,left_sum_axes,right_sum_axes).toArray() 
        self.assertAllClose(
            correct_result,
            left_sparse_data.contractWith(right_sparse_data,left_sum_axes,right_sum_axes).toArray()
        )
    # }}}
    @with_checker # test_join_trivial_case {{{
    def test_join_trivial_case(self,sparse_ndim=irange(1,8),nested_ndim=irange(1,8)):
        all_groupings = [[i] for i in xrange(sparse_ndim + nested_ndim)]
        sparse_data = SparseData.newRandom(NDArrayData,randomShape(sparse_ndim,3),randomShape(nested_ndim,3))
        ndarray_data = NDArrayData(sparse_data.toArray())
        self.assertAllClose(
            ndarray_data.join(all_groupings).toArray(),
            sparse_data.join(all_groupings).toArray(),
        )
    # }}}
    @with_checker # test_join_trivial_case_with_shuffled_groupings {{{
    def test_join_trivial_case_with_shuffled_groupings(self,sparse_ndim=irange(1,8),nested_ndim=irange(1,8)):
        all_groupings = [[i] for i in xrange(sparse_ndim + nested_ndim)]
        shuffle(all_groupings)
        sparse_data = SparseData.newRandom(NDArrayData,randomShape(sparse_ndim,3),randomShape(nested_ndim,3))
        ndarray_data = NDArrayData(sparse_data.toArray())
        self.assertAllClose(
            ndarray_data.join(all_groupings).toArray(),
            sparse_data.join(all_groupings).toArray(),
        )
    # }}}
    @with_checker # test_join_trivial_case_with_transposition {{{
    def test_join_trivial_case_with_transposition(self,sparse_ndim=irange(1,8),nested_ndim=irange(1,8)):
        all_groupings = [[i] for i in xrange(sparse_ndim + nested_ndim)]
        transposition = randomPermutation(sparse_ndim + nested_ndim)
        sparse_data = SparseData.newRandom(NDArrayData,randomShape(sparse_ndim,3),randomShape(nested_ndim,3)).transpose(transposition)
        ndarray_data = NDArrayData(sparse_data.toArray())
        self.assertAllClose(
            ndarray_data.join(all_groupings).toArray(),
            sparse_data.join(all_groupings).toArray(),
        )
    # }}}
    @with_checker # test_join_trivial_case {{{
    def test_join_flatten_case(self,sparse_ndim=irange(1,8),nested_ndim=irange(1,8)):
        all_groupings = [range(sparse_ndim),range(sparse_ndim,sparse_ndim+nested_ndim)]
        sparse_data = SparseData.newRandom(NDArrayData,randomShape(sparse_ndim,3),randomShape(nested_ndim,3))
        ndarray_data = NDArrayData(sparse_data.toArray())
        self.assertAllClose(
            ndarray_data.join(all_groupings).toArray(),
            sparse_data.join(all_groupings).toArray(),
        )
    # }}}
    @with_checker # test_join_full_case {{{
    def test_join_full_case(self,sparse_ndim=irange(1,8),nested_ndim=irange(1,8)):
        sparse_axes = range(sparse_ndim)
        shuffle(sparse_axes)
        sparse_groupings = randomPartitioningOf(sparse_axes)
        del sparse_axes
        nested_axes = range(nested_ndim)
        shuffle(nested_axes)
        nested_groupings = randomPartitioningOf(nested_axes)
        del nested_axes
        all_groupings = sparse_groupings + [[sparse_ndim+axis for axis in group] for group in nested_groupings]
        del sparse_groupings, nested_groupings
        transposition = randomPermutation(sparse_ndim + nested_ndim)
        all_groupings = [[transposition.index(axis) for axis in grouping] for grouping in all_groupings if grouping]
        shuffle(all_groupings)
        sparse_data = SparseData.newRandom(NDArrayData,randomShape(sparse_ndim,3),randomShape(nested_ndim,3)).transpose(transposition)
        ndarray_data = NDArrayData(sparse_data.toArray())
        self.assertAllClose(
            ndarray_data.join(all_groupings).toArray(),
            sparse_data.join(all_groupings).toArray(),
        )
    # }}}
    @with_checker # test_transpose {{{
    def test_transpose(self,sparse_ndim=irange(0,5),nested_ndim=irange(0,5)):
        sparse_data = SparseData.newRandom(NDArrayData,randomShape(sparse_ndim),randomShape(nested_ndim))
        transposition = randomPermutation(sparse_ndim+nested_ndim)
        self.assertAllClose(sparse_data.transpose(transposition).toArray(),sparse_data.toArray().transpose(transposition))
    # }}}
# }}}
# }}} Tests
