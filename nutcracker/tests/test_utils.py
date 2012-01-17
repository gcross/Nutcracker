# Imports {{{
from copy import copy
from paycheck import *
from random import sample, shuffle

from ..utils import normalize
from . import *
# }}}

class TestNormalize(TestCase): # {{{
    @with_checker
    def test_correctness(self,number_of_dimensions = irange(2,5)):
        tensor, index = randomNormalizableTensorAndIndex(number_of_dimensions)
        self.assertNormalized(normalize(tensor,index),index)
# }}}

class TestCompressConnectionBetween(TestCase): # {{{
    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_keep_all(self, # {{{
        tensor_1_ndim = irange(1,5),
        tensor_2_ndim = irange(1,5),
    ):
        index_1 = randint(0,tensor_1_ndim-1)
        tensor_1_shape = [randint(1,5) for _ in range(tensor_1_ndim)]
        tensor_1 = crand(*tensor_1_shape)

        index_2 = randint(0,tensor_2_ndim-1)
        tensor_2_shape = [randint(1,5) for _ in range(tensor_2_ndim)]
        tensor_2_shape[index_2] = tensor_1_shape[index_1]
        tensor_2 = crand(*tensor_2_shape)

        compressed_tensor_1, compressed_tensor_2 = \
            compressConnectionBetween(tensor_1,index_1,tensor_2,index_2,keep=len)

        self.assertEqual(
            compressed_tensor_1.shape[index_1],
            compressed_tensor_2.shape[index_2],
        )
        self.assertEqual(
            withoutIndex(compressed_tensor_1.shape,index_1),
            withoutIndex(tensor_1.shape,index_1),
        )
        self.assertEqual(
            withoutIndex(compressed_tensor_2.shape,index_2),
            withoutIndex(tensor_2.shape,index_2),
        )
        self.assertAllClose(
            tensordot(tensor_1,tensor_2,(index_1,index_2)),
            tensordot(compressed_tensor_1,compressed_tensor_2,(index_1,index_2)),
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_threshold_zero(self, # {{{
        tensor_1_ndim = irange(1,5),
        tensor_2_ndim = irange(1,5),
    ):
        index_1 = randint(0,tensor_1_ndim-1)
        tensor_1_shape = [randint(1,5) for _ in range(tensor_1_ndim)]
        tensor_1 = crand(*tensor_1_shape)

        index_2 = randint(0,tensor_2_ndim-1)
        tensor_2_shape = [randint(1,5) for _ in range(tensor_2_ndim)]
        tensor_2_shape[index_2] = tensor_1_shape[index_1]
        tensor_2 = crand(*tensor_2_shape)

        compressed_tensor_1, compressed_tensor_2 = \
            compressConnectionBetween(tensor_1,index_1,tensor_2,index_2,threshold=0)

        self.assertEqual(
            compressed_tensor_1.shape[index_1],
            compressed_tensor_2.shape[index_2],
        )
        self.assertLessEqual(
            withoutIndex(compressed_tensor_1.shape,index_1),
            withoutIndex(tensor_1.shape,index_1),
        )
        self.assertLessEqual(
            withoutIndex(compressed_tensor_2.shape,index_2),
            withoutIndex(tensor_2.shape,index_2),
        )
        self.assertAllClose(
            tensordot(tensor_1,tensor_2,(index_1,index_2)),
            tensordot(compressed_tensor_1,compressed_tensor_2,(index_1,index_2)),
        )
    # }}}
# }}}

class TestCompressConnectionToSelf(TestCase): # {{{
    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_keep_all(self, # {{{
        tensor_ndim = irange(2,5),
    ):
        index = randint(0,tensor_ndim-1)
        tensor_shape = [randint(1,5) for _ in range(tensor_ndim)]
        tensor = crand(*tensor_shape)

        compressed_tensor = compressConnectionToSelf(tensor,index,keep=len)

        self.assertEqual(
            withoutIndex(tensor.shape,index),
            withoutIndex(compressed_tensor.shape,index)
        )
        self.assertAllClose(
            tensordot(tensor,tensor.conj(),(index,index)),
            tensordot(compressed_tensor,compressed_tensor.conj(),(index,index))
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_threshold_zero(self, #{{{
        tensor_ndim = irange(2,5),
    ):
        index = randint(0,tensor_ndim-1)
        tensor_shape = [randint(1,5) for _ in range(tensor_ndim)]
        tensor = crand(*tensor_shape)

        compressed_tensor = compressConnectionToSelf(tensor,index,threshold=0)

        self.assertTrue(compressed_tensor.shape[index] <= tensor.shape[index])
        self.assertEqual(
            withoutIndex(tensor.shape,index),
            withoutIndex(compressed_tensor.shape,index)
        )
        self.assertAllClose(
            tensordot(tensor,tensor.conj(),(index,index)),
            tensordot(compressed_tensor,compressed_tensor.conj(),(index,index))
        )
    # }}}
# }}}

class TestCompressConnectionUsingFirstTensorBetween(TestCase): # {{{
    @with_checker(number_of_calls=10)
    def test_join_invariant_under_keep_all(self, # {{{
        tensor_1_ndim = irange(1,5),
        tensor_2_ndim = irange(1,5),
    ):
        index_1 = randint(0,tensor_1_ndim-1)
        tensor_1_shape = [randint(1,5) for _ in range(tensor_1_ndim)]
        tensor_1 = crand(*tensor_1_shape)

        index_2 = randint(0,tensor_2_ndim-1)
        tensor_2_shape = [randint(1,5) for _ in range(tensor_2_ndim)]
        tensor_2_shape[index_2] = tensor_1_shape[index_1]
        tensor_2 = crand(*tensor_2_shape)

        compressed_tensor_1, compressed_tensor_2 = \
            compressConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,keep=len)

        self.assertEqual(
            compressed_tensor_1.shape[index_1],
            compressed_tensor_2.shape[index_2],
        )
        self.assertEqual(
            withoutIndex(compressed_tensor_1.shape,index_1),
            withoutIndex(tensor_1.shape,index_1),
        )
        self.assertEqual(
            withoutIndex(compressed_tensor_2.shape,index_2),
            withoutIndex(tensor_2.shape,index_2),
        )
        self.assertAllClose(
            tensordot(tensor_1,tensor_2,(index_1,index_2)),
            tensordot(compressed_tensor_1,compressed_tensor_2,(index_1,index_2)),
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_join_invariant_under_threshold_zero(self, # {{{
        tensor_1_ndim = irange(1,5),
        tensor_2_ndim = irange(1,5),
    ):
        index_1 = randint(0,tensor_1_ndim-1)
        tensor_1_shape = [randint(1,5) for _ in range(tensor_1_ndim)]
        tensor_1 = crand(*tensor_1_shape)

        index_2 = randint(0,tensor_2_ndim-1)
        tensor_2_shape = [randint(1,5) for _ in range(tensor_2_ndim)]
        tensor_2_shape[index_2] = tensor_1_shape[index_1]
        tensor_2 = crand(*tensor_2_shape)

        compressed_tensor_1, compressed_tensor_2 = \
            compressConnectionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,threshold=0)

        self.assertEqual(
            compressed_tensor_1.shape[index_1],
            compressed_tensor_2.shape[index_2],
        )
        self.assertLessEqual(
            withoutIndex(compressed_tensor_1.shape,index_1),
            withoutIndex(tensor_1.shape,index_1),
        )
        self.assertLessEqual(
            withoutIndex(compressed_tensor_2.shape,index_2),
            withoutIndex(tensor_2.shape,index_2),
        )
        self.assertAllClose(
            tensordot(tensor_1,tensor_2,(index_1,index_2)),
            tensordot(compressed_tensor_1,compressed_tensor_2,(index_1,index_2)),
        )
    # }}}
# }}}

class TestContractAndTransposeAndJoin(TestCase): # {{{
    @with_checker
    def test_correctness(self,array_ranks=(irange(1,8),)*2): # {{{
        array_shapes = [[randint(1,3) for _ in xrange(rank)] for rank in array_ranks]
        number_of_dimensions_to_join = randint(0,min(array_ranks))
        array_axes = [sample(range(rank),number_of_dimensions_to_join) for rank in array_ranks]
        for axis_0, axis_1 in zip(*array_axes):
            array_shapes[1][axis_1] = array_shapes[0][axis_0]
        arrays = [crand(*shape) for shape in array_shapes]
        indices_being_contracted_for = map(frozenset,array_axes)
        remaining_indices = []
        for tensor_number, rank in enumerate(array_ranks):
            for index in xrange(rank):
                if index not in indices_being_contracted_for[tensor_number]:
                    remaining_indices.append(TensorNumberAndIndex(tensor_number,index))
        shuffle(remaining_indices)
        number_of_remaining_indices = len(remaining_indices)
        number_of_partitions = randint(0,number_of_remaining_indices)
        partition_indices = [randint(0,number_of_remaining_indices) for _ in xrange(number_of_partitions)]
        partition_indices.sort()
        partition_indices = [0] + partition_indices + [number_of_remaining_indices]
        new_grouped_order = [remaining_indices[partition_indices[i]:partition_indices[i+1]] for i in xrange(number_of_partitions+1)]
        contractor = \
            formContractor(
                [0,1],
                zip(*[[(i,index) for index in axes] for (i,axes) in enumerate(array_axes)]),
                new_grouped_order,
            )
        self.assertAllClose(
            contractor(*arrays),
            contractAndTransposeAndJoin(arrays,array_axes,new_grouped_order),
        )
    # }}}
# }}}

class TestComputePostContractionIndexMap(TestCase): # {{{
    @with_checker
    def test_correct_number_of_entries(self, # {{{
        number_of_dimensions_increment=irange(1,10),
        indices_being_contracted=[irange(0,20)]
    ):
        old_number_of_dimensions = (
            max(indices_being_contracted) if len(indices_being_contracted) > 0 else 0
        ) + number_of_dimensions_increment
        self.assertEqual(
            old_number_of_dimensions-len(set(indices_being_contracted)),
            len(computePostContractionIndexMap(old_number_of_dimensions,indices_being_contracted))
        )
    # }}}

    @with_checker
    def test_no_contracted_old_indices(self, # {{{
        number_of_dimensions_increment=irange(1,10),
        indices_being_contracted=[irange(0,20)]
    ):
        old_number_of_dimensions = (
            max(indices_being_contracted) if len(indices_being_contracted) > 0 else 0
        ) + number_of_dimensions_increment
        old_to_new_index_map = computePostContractionIndexMap(old_number_of_dimensions,indices_being_contracted)
        self.assertEqual(
            frozenset(xrange(old_number_of_dimensions)),
            frozenset(indices_being_contracted) ^ old_to_new_index_map.viewkeys(),
        )
    # }}}

# }}}

class TestFormContractor(TestCase): # {{{
    @with_checker(number_of_calls=10)
    def test_trivial_case_1D(self, # {{{
        d = irange(1,10),
    ):
        x = crand(d)
        self.assertAllEqual(x,formContractor(['A'],[],[[('A',0)]])(x))
    # }}}

    @with_checker(number_of_calls=100)
    def test_trivial_case_ND(self, # {{{
        n = irange(1,8),
    ):
        x = crand(*[randint(1,3) for _ in range(n)])
        self.assertAllEqual(x,formContractor(['A'],[],[[('A',i)] for i in range(n)])(x))
    # }}}

    @with_checker(number_of_calls=100)
    def test_trivial_case_ND_flattened(self, # {{{
        n = irange(1,8),
    ):
        x = crand(*[randint(1,3) for _ in range(n)])
        self.assertAllEqual(x.ravel(),formContractor(['A'],[],[[('A',i) for i in range(n)]])(x))
    # }}}

    @with_checker(number_of_calls=10)
    def test_matvec(self, # {{{
        m = irange(1,10),
        n = irange(1,10),
    ):
        M = crand(m,n)
        v = crand(n)
        self.assertAllEqual(
            dot(M,v),
            formContractor(
                ['M','v'],
                [(('M',1),('v',0))],
                [[('M',0)]]
            )(M,v)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_matvec_transposed(self, # {{{
        m = irange(1,10),
        n = irange(1,10)
    ):
        M = crand(n,m)
        v = crand(n)
        self.assertAllEqual(
            dot(v,M),
            formContractor(
                ['v','M'],
                [(('M',0),('v',0))],
                [[('M',1)]]
            )(v,M)
        )
    # }}}
    
    @with_checker(number_of_calls=10)
    def test_matmat(self, # {{{
        m = irange(1,10),
        n = irange(1,10),
        o = irange(1,10),
    ):
        A = crand(m,n)
        B = crand(n,o)
        self.assertAllClose(
            dot(A,B),
            formContractor(
                ['A','B'],
                [(('A',1),('B',0))],
                [[('A',0)],[('B',1)]]
            )(A,B)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_matmat_flattened(self, # {{{
        m = irange(1,10),
        n = irange(1,10),
        o = irange(1,10),
    ):
        A = crand(m,n)
        B = crand(n,o)
        self.assertAllClose(
            dot(A,B).ravel(),
            formContractor(
                ['A','B'],
                [(('A',1),('B',0))],
                [[('A',0),('B',1)]]
            )(A,B)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_matmat_transposed(self, # {{{
        m = irange(1,10),
        n = irange(1,10),
        o = irange(1,10),
    ):
        A = crand(m,n)
        B = crand(n,o)
        self.assertAllClose(
            dot(A,B).transpose(),
            formContractor(
                ['A','B'],
                [(('A',1),('B',0))],
                [[('B',1)],[('A',0)]]
            )(A,B)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_matmat_swapped_and_transposed(self, # {{{
        m = irange(1,10),
        n = irange(1,10),
        o = irange(1,10),
    ):
        A = crand(n,m)
        B = crand(o,n)
        self.assertAllClose(
            dot(B,A).transpose(),
            formContractor(
                ['A','B'],
                [(('A',0),('B',1))],
                [[('A',1)],[('B',0)]]
            )(A,B)
        )
    # }}}
    
    @with_checker(number_of_calls=10)
    def test_r3r3(self, # {{{
        a = irange(1,10),
        b = irange(1,10),
        c = irange(1,10),
        d = irange(1,10),
        e = irange(1,10),
    ):
        A = crand(a,b,c)
        B = crand(c,d,e)
        self.assertAllClose(
            dot(A.reshape(a*b,c),B.reshape(c,d*e)).reshape(a,b,d,e),
            formContractor(
                ['A','B'],
                [(('A',2),('B',0))],
                [[('A',0)],[('A',1)],[('B',1)],[('B',2)]]
            )(A,B)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_r3r3_semi_flattened(self, # {{{
        a = irange(1,10),
        b = irange(1,10),
        c = irange(1,10),
        d = irange(1,10),
        e = irange(1,10),
    ):
        A = crand(a,b,c)
        B = crand(c,d,e)
        self.assertAllClose(
            dot(A.reshape(a*b,c),B.reshape(c,d*e)),
            formContractor(
                ['A','B'],
                [(('A',2),('B',0))],
                [[('A',0),('A',1)],[('B',1),('B',2)]]
            )(A,B)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_r3r3_semi_transposed_and_flattened(self, # {{{
        a = irange(1,10),
        b = irange(1,10),
        c = irange(1,10),
        d = irange(1,10),
        e = irange(1,10),
    ):
        A = crand(a,b,c)
        B = crand(c,d,e)
        self.assertAllClose(
            dot(A.reshape(a*b,c),B.reshape(c,d*e)).transpose().reshape(d,e,a,b).transpose(1,0,2,3).reshape(e*d,a,b),
            formContractor(
                ['A','B'],
                [(('A',2),('B',0))],
                [[('B',2),('B',1)],[('A',0)],[('A',1)]]
            )(A,B)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_r3r2(self, # {{{
        a = irange(1,10),
        b = irange(1,10),
        c = irange(1,10),
        d = irange(1,10),
    ):
        A = crand(a,b,c)
        B = crand(c,d)
        self.assertAllClose(
            dot(A.reshape(a*b,c),B.reshape(c,d)).reshape(a,b,d),
            formContractor(
                ['A','B'],
                [(('A',2),('B',0))],
                [[('A',0)],[('A',1)],[('B',1)]]
            )(A,B)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_r3r3_semi_flattened(self, # {{{
        a = irange(1,10),
        b = irange(1,10),
        c = irange(1,10),
        d = irange(1,10),
    ):
        A = crand(a,b,c)
        B = crand(c,d)
        self.assertAllClose(
            dot(A.reshape(a*b,c),B.reshape(c,d)).reshape(a,b*d),
            formContractor(
                ['A','B'],
                [(('A',2),('B',0))],
                [[('A',0)],[('A',1),('B',1)]]
            )(A,B)
        )
    # }}}
    
    @with_checker(number_of_calls=10)
    def test_matmatmat(self, # {{{
        a = irange(1,10),
        b = irange(1,10),
        c = irange(1,10),
        d = irange(1,10),
    ):
        A = crand(a,b)
        B = crand(b,c)
        C = crand(c,d)
        self.assertAllClose(
            dot(dot(A,B),C),
            formContractor(
                ['A','B','C'],
                [
                    (('A',1),('B',0)),
                    (('B',1),('C',0)),
                ],
                [[('A',0)],[('C',1)]]
            )(A,B,C)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_triangle(self, # {{{
        a = irange(1,10),
        b = irange(1,10),
        c = irange(1,10),
        d = irange(1,10),
        e = irange(1,10),
        f = irange(1,10),
    ):
        A = crand(a,e,b)
        B = crand(c,b,f)
        C = crand(d,a,c)
        AB = tensordot(A,B,(2,1))
        ABC = tensordot(AB,C,((0,2),(1,2)))
        self.assertAllClose(
            ABC,
            formContractor(
                ['A','B','C'],
                [
                    (('A',0),('C',1)),
                    (('A',2),('B',1)),
                    (('B',0),('C',2)),
                ],
                [[('A',1)],[('B',2)],[('C',0)]]
            )(A,B,C)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_1(self, # {{{
        a = irange(1,10),
        b = irange(1,10),
        c = irange(1,10),
        d = irange(1,10),
        e = irange(1,10),
        f = irange(1,10),
    ):
        A = crand(a,b,c)
        B = crand(d,e,c,f)
        AB = tensordot(A,B,(2,2)).transpose(0,2,1,3,4).reshape(a*d,b*e,f)
        self.assertAllClose(
            AB,
            formContractor(
                ['A','B'],
                [
                    (('A',2),('B',2)),
                ],
                [
                    [('A',0),('B',0)],
                    [('A',1),('B',1)],
                    [('B',3)],
                ]
            )(A,B)
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_2(self, # {{{
        a = irange(1,5),
        b = irange(1,5),
        c = irange(1,5),
        d = irange(1,5),
        e = irange(1,5),
        f = irange(1,5),
        g = irange(1,5),
        h = irange(1,5),
        i = irange(1,5),
    ):
        A = crand(a,b,c,d)
        B = crand(e,f,d,h,i)
        AB = tensordot(A,B,(3,2)).transpose(0,3,1,4,2,5,6).reshape(a*e,b*f,c*h,i)
        self.assertAllClose(
            AB,
            formContractor(
                ['A','B'],
                [
                    (('A',3),('B',2)),
                ],
                [
                    [('A',0),('B',0)],
                    [('A',1),('B',1)],
                    [('A',2),('B',3)],
                    [('B',4)],
                ]
            )(A,B)
        )
    # }}}
# }}}

class TestIncreaseDimensionUsingFirstTensorOnlyBetween(TestCase): # {{{
    @with_checker
    def test_invariant_when_new_dimension_same_as_old(self, # {{{
        m=irange(2,5),
        n=irange(1,5),
    ):
        tensor_1, index_1 = randomTensorAndIndex(m)
        old_dimension = tensor_1.shape[index_1]
        tensor_2, index_2 = randomTensorAndIndexAgreeingWith(n,old_dimension)
        new_tensor_1, new_tensor_2 = increaseDimensionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,old_dimension)
        self.assertAllClose(
            tensordot(tensor_1,tensor_2,(index_1,index_2)),
            tensordot(new_tensor_1,new_tensor_2,(index_1,index_2)),
        )
    # }}}
    
    @with_checker
    def test_invariant_when_new_dimension_more_than_old(self, # {{{
        m=irange(2,5),
        n=irange(1,5),
    ):
        index_1 = randomIndex(m)
        tensor_1_shape = randomShape(m)
        tensor_1_shape[index_1] = randint(1,product(withoutIndex(tensor_1_shape,index_1)))
        tensor_1 = crand(*tensor_1_shape)
        old_dimension = tensor_1.shape[index_1]
        tensor_2, index_2 = randomTensorAndIndexAgreeingWith(n,old_dimension)
        new_dimension = randint(old_dimension,product(withoutIndex(tensor_1.shape,index_1)))
        new_tensor_1, new_tensor_2 = increaseDimensionUsingFirstTensorOnlyBetween(tensor_1,index_1,tensor_2,index_2,new_dimension)
        self.assertEqual(new_tensor_1.shape[index_1],new_dimension)
        self.assertEqual(new_tensor_2.shape[index_2],new_dimension)
        self.assertAllClose(
            tensordot(tensor_1,tensor_2,(index_1,index_2)),
            tensordot(new_tensor_1,new_tensor_2,(index_1,index_2)),
        )
    # }}}
# }}}

class test_invertPermutation(TestCase): # {{{
    @with_checker
    def test_identity_on_range(self,number_of_elements=irange(0,20)): # {{{
        self.assertEqual(range(number_of_elements),invertPermutation(range(number_of_elements)))
    # }}}

    @with_checker
    def test_self_inverse(self,number_of_elements=irange(0,20)): # {{{
        permutation = range(number_of_elements)
        shuffle(permutation)
        self.assertEqual(permutation,invertPermutation(invertPermutation(permutation)))
    # }}}

    @with_checker
    def test_correct_when_applied_to_data(self,number_of_elements=irange(0,20)): # {{{
        data = [randint(0,100) for _ in xrange(number_of_elements)]
        permutation = range(number_of_elements)
        shuffle(permutation)
        permuted_data = [data[i] for i in permutation]
        self.assertEqual(data,[permuted_data[i] for i in invertPermutation(permutation)])
    # }}}
# }}}

class TestNormalizeAndDenormalize(TestCase): # {{{
    @with_checker
    def test_random(self, # {{{
        tensor_1_ndim = irange(1,5),
        tensor_2_ndim = irange(1,5),
    ):
        index_1 = randint(0,tensor_1_ndim-1)
        tensor_1_shape = [randint(1,5) for _ in range(tensor_1_ndim)]
        tensor_1_shape_without_index = copy(tensor_1_shape)
        del tensor_1_shape_without_index[index_1]
        tensor_1_degrees_of_freedom = product(tensor_1_shape_without_index)
        tensor_1_shape[index_1] = min(tensor_1_shape[index_1],tensor_1_degrees_of_freedom)
        tensor_1 = crand(*tensor_1_shape)

        index_2 = randint(0,tensor_2_ndim-1)
        tensor_2_shape = [randint(1,5) for _ in range(tensor_2_ndim)]
        tensor_2_shape[index_2] = tensor_1_shape[index_1]
        tensor_2 = crand(*tensor_2_shape)

        new_tensor_1, new_tensor_2 = normalizeAndDenormalize(tensor_1,index_1,tensor_2,index_2)
        self.assertEqual(tensor_1.shape,new_tensor_1.shape)
        self.assertEqual(tensor_2.shape,new_tensor_2.shape)

        self.assertNormalized(new_tensor_1,index_1)
        self.assertAllClose(
            tensordot(tensor_1,tensor_2,(index_1,index_2)),
            tensordot(new_tensor_1,new_tensor_2,(index_1,index_2))
        )
    # }}}
# }}}

class TestTruncateConnectionToSelf(TestCase): # {{{
    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_unfiltered(self, # {{{
        tensor_ndim = irange(2,5),
    ):
        index = randint(0,tensor_ndim-1)
        tensor_shape = [randint(1,5) for _ in range(tensor_ndim)]
        tensor = crand(*tensor_shape)

        truncated_tensor = truncateConnectionToSelf(tensor,index,len)

        self.assertEqual(
            withoutIndex(tensor.shape,index),
            withoutIndex(truncated_tensor.shape,index)
        )
        self.assertAllClose(
            tensordot(tensor,tensor.conj(),(index,index)),
            tensordot(truncated_tensor,truncated_tensor.conj(),(index,index))
        )
    # }}}

    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_zero_filtered(self, # {{{
        tensor_ndim = irange(2,5),
    ):
        index = randint(0,tensor_ndim-1)
        tensor_shape = [randint(1,5) for _ in range(tensor_ndim)]
        tensor = crand(*tensor_shape)

        truncated_tensor = truncateConnectionToSelf(tensor,index,lambda arr: firstIndexBelowMagnitude(arr,1e-10))

        self.assertTrue(truncated_tensor.shape[index] <= tensor.shape[index])
        self.assertEqual(
            withoutIndex(tensor.shape,index),
            withoutIndex(truncated_tensor.shape,index)
        )
        self.assertAllClose(
            tensordot(tensor,tensor.conj(),(index,index)),
            tensordot(truncated_tensor,truncated_tensor.conj(),(index,index))
        )
    # }}}
# }}}
