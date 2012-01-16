# Imports {{{
from collections import defaultdict
from copy import copy
import itertools
from numpy import prod, split, uint32, zeros
from numpy.random import random_integers
import operator
from paycheck import generator
from paycheck import *
from random import randint, shuffle
from uuid import uuid4

from . import *
from ..enumerations import *
from ..sparse import *
# }}}

# Helper functions {{{

def generateIndexTableWithUniqueRows(max,rows,cols): # {{{
    if rows == 0 or cols == 0:
        return zeros(shape=(rows,cols),dtype=uint32)
    index_table = None
    while index_table is None:
        index_table = random_integers(10,max,size=(rows,cols))
        if len(index_table) != len(frozenset(map(tuple,index_table))):
            index_table = None
    assert index_table is not None
    return index_table
# }}}

def primesUpTo(inclusive_upper_bound): # {{{
    primes = []
    current = 2
    while current <= inclusive_upper_bound:
        is_prime = True
        for prime in primes:
            if current % prime == 0:
                is_prime = False
                break
        if is_prime:
            primes.append(current)
        current += 1
    return primes
# }}}

def randomVirtualIndexFromPrimes(primes): # {{{
    primes = copy(primes)
    entries = []
    while len(primes) > 0:
        n = randint(0,len(primes))
        entries.append(VirtualIndexEntry(
            index=uuid4(),
            size=reduce(operator.mul,primes[:n],1),
            sparsity=Sparsity.randomChoice()
        ))
        primes = primes[n:]
    return VirtualIndex(entries)
# }}}

# }}}

# Generators {{{

class PairOfVirtualIndicesGenerator(generator.PayCheckGenerator): # {{{
    def __init__(self,upper_bound_on_primes=20):
        self.prime_list_generator = self.get([choiceof(primesUpTo(upper_bound_on_primes))])
    def __next__(self):
        primes = next(self.prime_list_generator)
        return (randomVirtualIndexFromPrimes(primes),randomVirtualIndexFromPrimes(primes))
pair_of_virtual_shapes = PairOfVirtualIndicesGenerator
# }}}

# }}}

# Tests {{{

class test_computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable(TestCase): # {{{
    @with_checker(number_of_calls=20)
    def test_outer_product(self, nrows = (irange(0,20),)*2, ndims = (irange(1,20),)*2): # {{{
        index_tables = [generateIndexTableWithUniqueRows(1000,nrow,ndim) for (nrow,ndim) in zip(nrows,ndims)]
        remaining_axes_lists = [range(ndim) for ndim in ndims]
        for remaining_axes_list in remaining_axes_lists:
            shuffle(remaining_axes_list)
        del remaining_axes_list
        joined_index_table, matrix_join_lists = computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable(index_tables,([],[]),([],[]),remaining_axes_lists)
        self.assertEqual((nrows[0]*nrows[1],ndims[0]+ndims[1]),joined_index_table.shape)
        for row_number, (row, matrix_join_list) in enumerate(zip(joined_index_table,matrix_join_lists)):
            row_sides = split(row,[ndims[0]])
            for matrix_join in matrix_join_list:
                for side_number, (side_row_number,side_sparse_to_dense_indices) in enumerate(matrix_join):
                    self.assertAllEqual(
                        [index_tables[side_number][side_row_number][i] for i in remaining_axes_lists[side_number]],
                        row_sides[side_number],
                    )
                    self.assertEqual((),side_sparse_to_dense_indices)
        joined_index_rows = frozenset(map(tuple,joined_index_table))
        self.assertEqual(len(joined_index_table),len(joined_index_rows))
        correct_joined_index_rows = frozenset(
            tuple(a[i] for i in remaining_axes_lists[0]) + tuple(b[i] for i in remaining_axes_lists[1])
            for a,b in itertools.product(*index_tables)
        )
        self.assertEqual(correct_joined_index_rows,joined_index_rows)
    # }}}

    @with_checker
    def test_scalar_inner_product(self, nrow = irange(1,20), ndim = irange(1,20)): # {{{
        index_tables = [generateIndexTableWithUniqueRows(100000,nrow,ndim)]
        index_tables.append(list(index_tables[0]))
        shuffle(index_tables[1])
        index_tables[1] = array(index_tables[1])
        sparse_to_sparse_join_axis_lists = [range(ndim)]*2
        shuffle(sparse_to_sparse_join_axis_lists[0])
        joined_index_table, matrix_join_lists = computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable(index_tables,sparse_to_sparse_join_axis_lists,([],[]),([],[]))
        self.assertEqual((1,0),joined_index_table.shape)
        self.assertEqual(1,len(matrix_join_lists))
        matrix_join_list = matrix_join_lists[0]
        self.assertEqual(nrow,len(matrix_join_list))
        for (left_row_number,left_sparse_to_dense_indices), (right_row_number,right_sparse_to_dense_indices) in matrix_join_list:
            self.assertAllEqual(index_tables[0][left_row_number],index_tables[1][right_row_number])
            self.assertEqual((),left_sparse_to_dense_indices)
            self.assertEqual((),right_sparse_to_dense_indices)
    # }}}zo

    @with_checker
    def test_full_inner_outer_product(self, nrows = (irange(0,20),)*2, ndims = (irange(1,20),)*2): # {{{
        # Compute additional size parameters {{{
        minimum_number_of_shared_rows = 0*randint(0,min(*nrows))
        number_of_sparse_to_sparse_axes = randint(0,min(*ndims))
        number_of_sparse_to_dense_axes_list = [randint(0,ndims[i]-number_of_sparse_to_sparse_axes) for i in xrange(2)]
        number_of_remaining_axes_list = [ndims[i] - number_of_sparse_to_sparse_axes - number_of_sparse_to_dense_axes_list[i] for i in xrange(2)]
        for i in xrange(2):
            self.assertEqual(ndims[i],number_of_sparse_to_sparse_axes+number_of_sparse_to_dense_axes_list[i]+number_of_remaining_axes_list[i])
        # }}}
        # Compute axes {{{
        permutations = []
        inverse_permutations = []
        sparse_to_sparse_axes_lists = []
        sparse_to_dense_axes_lists = []
        remaining_axes_lists = []
        for ndim, number_of_sparse_to_dense_axes, number_of_remaining_axes in zip(ndims,number_of_sparse_to_dense_axes_list,number_of_remaining_axes_list):
            axes = range(ndim)
            shuffle(axes)
            sparse_to_sparse_axes, sparse_to_dense_axes, remaining_axes = map(list,split(axes,[number_of_sparse_to_sparse_axes,number_of_sparse_to_sparse_axes+number_of_sparse_to_dense_axes]))
            sparse_to_sparse_axes_lists.append(sparse_to_sparse_axes)
            sparse_to_dense_axes_lists.append(sparse_to_dense_axes)
            remaining_axes_lists.append(remaining_axes)
            inverse_permutations.append(axes)
            permutations.append(invertPermutation(axes))
        # }}}
        # Compute index tables {{{
        shared_rows = [tuple(randint(0,1) for _ in xrange(number_of_sparse_to_sparse_axes)) for _ in xrange(minimum_number_of_shared_rows)]
        index_tables = []
        sparse_to_sparse_indices_sets = []
        for i in xrange(2):
            ndim = ndims[i]
            index_table = map(tuple,generateIndexTableWithUniqueRows(1000000,nrows[i]-minimum_number_of_shared_rows,ndim))
            for row in shared_rows:
                index_table.append(row + tuple(randint(0,1) for _ in xrange(ndim - number_of_sparse_to_sparse_axes)))
            shuffle(index_table)
            sparse_to_sparse_indices_set = set()
            for row in index_table:
                sparse_to_sparse_indices_set.add(row[:number_of_sparse_to_sparse_axes])
            if index_table:
                index_table = array(index_table,dtype=uint32)
            else:
                index_table = zeros((0,ndims[i]),dtype=uint32)
            index_tables.append(index_table)
            sparse_to_sparse_indices_sets.append(sparse_to_sparse_indices_set)
        shared_sparse_to_sparse_indices = sparse_to_sparse_indices_sets[0] & sparse_to_sparse_indices_sets[1]
        del ndim, shared_rows, sparse_to_sparse_indices_set, sparse_to_sparse_indices_sets
        counts_table = defaultdict(lambda: (defaultdict(lambda: 0),defaultdict(lambda: 0)))
        for i, index_table in enumerate(index_tables):
            number_of_remaining_axes = number_of_remaining_axes_list[i]
            for row in index_table:
                sparse_to_sparse_indices = tuple(row[:number_of_sparse_to_sparse_axes])
                if sparse_to_sparse_indices in shared_sparse_to_sparse_indices:
                    if number_of_remaining_axes:
                        remaining_indices = tuple(row[-number_of_remaining_axes_list[i]:])
                    else:
                        remaining_indices = tuple()
                    counts_table[sparse_to_sparse_indices][i][remaining_indices] += 1
            index_tables[i] = index_table[...,permutations[i]]
        del i, index_table, sparse_to_sparse_indices, number_of_remaining_axes
        # }}}
        # Compute expected join counts table {{{
        expected_join_counts = defaultdict(lambda: 0)
        for (left_joins,right_joins) in counts_table.itervalues():
            for left_join_label, left_join_count in left_joins.iteritems():
                for right_join_label, right_join_count in right_joins.iteritems():
                    expected_join_counts[left_join_label + right_join_label] += left_join_count*right_join_count
        # }}}
        joined_index_table, matrix_join_lists = computeJoinedIndexTableAndMatrixJoinTableFromSparseJoinTable(index_tables,sparse_to_sparse_axes_lists,sparse_to_dense_axes_lists,remaining_axes_lists)
        self.assertEqual(sum(number_of_remaining_axes_list),joined_index_table.shape[1])
        if not shared_sparse_to_sparse_indices:
            self.assertEqual(0,joined_index_table.shape[0])
            self.assertEqual(0,len(matrix_join_lists))
            return
        observed_join_counts = defaultdict(lambda: 0)
        observed_joins = set()
        for row, matrix_join_list in zip(joined_index_table,matrix_join_lists):
            remaining_indices_split = map(list,split(row,[number_of_remaining_axes_list[0]]))
            for matrix_join in matrix_join_list:
                join = (matrix_join[0][0],matrix_join[1][0])
                self.assertTrue(join not in observed_joins)
                observed_joins.add(join)
                observed_join_counts[tuple(row)] += 1
                for i, ((row_number, sparse_to_dense_join_indices), remaining_indices) in enumerate(zip(matrix_join,remaining_indices_split)):
                    self.assertAllEqual(index_tables[i][row_number][remaining_axes_lists[i]],remaining_indices)
                    self.assertAllEqual(index_tables[i][row_number][sparse_to_dense_axes_lists[i]],sparse_to_dense_join_indices)
                    self.assertTrue(tuple(index_tables[i][row_number][sparse_to_sparse_axes_lists[i]]) in shared_sparse_to_sparse_indices)
        self.assertEqual(set(expected_join_counts.iterkeys()),set(observed_join_counts.iterkeys()))
        self.assertEqual(expected_join_counts,observed_join_counts)
    # }}}

# }}}

class TestPairOfVirtualIndicesGenerator(TestCase): # {{{
    @with_checker
    def test_both_shapes_have_the_same_size(self,pair_of_virtual_shapes=pair_of_virtual_shapes): # {{{
        self.assertEqual(pair_of_virtual_shapes[0].size,pair_of_virtual_shapes[1].size)
    # }}}
# }}}

class TestReconcileVirtualIndices(TestCase): # {{{
    @with_checker
    def test_correct_split_total(self,pair_of_virtual_shapes=pair_of_virtual_shapes): # {{{
        (_,splits) = reconcileVirtualIndices(*pair_of_virtual_shapes)
        for (virtual_shape, split) in zip(pair_of_virtual_shapes,splits):
            for entry in virtual_shape:
                self.assertEqual(entry.size,reduce(operator.mul,split[entry.index],1))
    # }}}

    @with_checker
    def test_size_unchanged_by_reconciliation(self,pair_of_virtual_shapes=pair_of_virtual_shapes): # {{{
        (pair_of_reconciled_virtual_shapes,_) = reconcileVirtualIndices(*pair_of_virtual_shapes)
        for (virtual_shape,reconciled_virtual_shape) in zip(pair_of_virtual_shapes,pair_of_reconciled_virtual_shapes):
            self.assertEqual(virtual_shape.size,reconciled_virtual_shape.size)
    # }}}

    @with_checker
    def test_entries_matich_after_reconciliation(self,pair_of_virtual_shapes=pair_of_virtual_shapes): # {{{
        (pair_of_reconciled_virtual_shapes,_) = reconcileVirtualIndices(*pair_of_virtual_shapes)
        for (entry_1,entry_2) in zip(*pair_of_reconciled_virtual_shapes):
            self.assertEqual(entry_1.size,entry_2.size)
    # }}}

# }}}

# }}}
