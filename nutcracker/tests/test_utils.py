#@+leo-ver=5-thin
#@+node:gcross.20111108100704.1389: * @file test_utils.py
#@+<< Imports >>
#@+node:gcross.20111108100704.1390: ** << Imports >>
from paycheck import *

from ..utils import normalize
from . import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111108100704.1391: ** Tests
#@+node:gcross.20111108100704.1392: *3* normalize
class TestNormalize(TestCase):
    @with_checker
    def test_correctness(self,number_of_dimensions = irange(2,5)):
        tensor, index = randomNormalizableTensorAndIndex(number_of_dimensions)
        self.assertNormalized(normalize(tensor,index),index)
#@+node:gcross.20111022200315.1323: *3* compressConnectionBetween
class TestCompressConnectionBetween(TestCase):
    #@+others
    #@+node:gcross.20111022200315.1324: *4* test_matrix_invariant_under_keep_all
    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_keep_all(self,
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
    #@+node:gcross.20111022200315.1327: *4* test_matrix_invariant_under_threshold_zero
    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_threshold_zero(self,
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
    #@-others
#@+node:gcross.20111022200315.1282: *3* compressConnectionToSelf
class TestCompressConnectionToSelf(TestCase):
    #@+others
    #@+node:gcross.20111022200315.1283: *4* test_matrix_invariant_under_keep_all
    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_keep_all(self,
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
    #@+node:gcross.20111022200315.1284: *4* test_matrix_invariant_under_threshold_zero
    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_threshold_zero(self,
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
    #@-others
#@+node:gcross.20111024143336.1329: *3* compressConnectionUsingFirstTensorBetween
class TestCompressConnectionUsingFirstTensorBetween(TestCase):
    #@+others
    #@+node:gcross.20111024143336.1330: *4* test_join_invariant_under_keep_all
    @with_checker(number_of_calls=10)
    def test_join_invariant_under_keep_all(self,
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
    #@+node:gcross.20111024143336.1331: *4* test_join_invariant_under_threshold_zero
    @with_checker(number_of_calls=10)
    def test_join_invariant_under_threshold_zero(self,
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
    #@-others
#@+node:gcross.20111009135633.2983: *3* formContractor
class TestFormContractor(TestCase):
    #@+others
    #@+node:gcross.20111009135633.2984: *4* test_trivial_case_xD
    @with_checker(number_of_calls=10)
    def test_trivial_case_1D(self,
        d = irange(1,10),
    ):
        x = crand(d)
        self.assertAllEqual(x,formContractor(['A'],[],[[('A',0)]])(x))

    @with_checker(number_of_calls=100)
    def test_trivial_case_ND(self,
        n = irange(1,8),
    ):
        x = crand(*[randint(1,3) for _ in range(n)])
        self.assertAllEqual(x,formContractor(['A'],[],[[('A',i)] for i in range(n)])(x))


    @with_checker(number_of_calls=100)
    def test_trivial_case_ND_flattened(self,
        n = irange(1,8),
    ):
        x = crand(*[randint(1,3) for _ in range(n)])
        self.assertAllEqual(x.ravel(),formContractor(['A'],[],[[('A',i) for i in range(n)]])(x))
    #@+node:gcross.20111009135633.2988: *4* test_matvec
    @with_checker(number_of_calls=10)
    def test_matvec(self,
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

    @with_checker(number_of_calls=10)
    def test_matvec_transposed(self,
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
    #@+node:gcross.20111009135633.2992: *4* test_matmat
    @with_checker(number_of_calls=10)
    def test_matmat(self,
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

    @with_checker(number_of_calls=10)
    def test_matmat_flattened(self,
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

    @with_checker(number_of_calls=10)
    def test_matmat_transposed(self,
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

    @with_checker(number_of_calls=10)
    def test_matmat_swapped_and_transposed(self,
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
    #@+node:gcross.20111009135633.2993: *4* test_r3r3
    @with_checker(number_of_calls=10)
    def test_r3r3(self,
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

    @with_checker(number_of_calls=10)
    def test_r3r3_semi_flattened(self,
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

    @with_checker(number_of_calls=10)
    def test_r3r3_semi_transposed_and_flattened(self,
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
    #@+node:gcross.20111009135633.2997: *4* test_r3r2
    @with_checker(number_of_calls=10)
    def test_r3r2(self,
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

    @with_checker(number_of_calls=10)
    def test_r3r3_semi_flattened(self,
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
    #@+node:gcross.20111009135633.2999: *4* test_matmatmat
    @with_checker(number_of_calls=10)
    def test_matmatmat(self,
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
    #@+node:gcross.20111009135633.3000: *4* test_triangle
    @with_checker(number_of_calls=10)
    def test_triangle(self,
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
    #@+node:gcross.20111013080525.1215: *4* test_1
    @with_checker(number_of_calls=10)
    def test_1(self,
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
    #@+node:gcross.20111013080525.1217: *4* test_2
    @with_checker(number_of_calls=10)
    def test_2(self,
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
    #@-others
#@+node:gcross.20111028110210.1318: *3* increaseDimensionUsingFirstTensorOnlyBetween
class TestIncreaseDimensionUsingFirstTensorOnlyBetween(TestCase):
    #@+others
    #@+node:gcross.20111028110210.1319: *4* test_invariant_when_new_dimension_same_as_old
    @with_checker
    def test_invariant_when_new_dimension_same_as_old(self,
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
    #@+node:gcross.20111028110210.1332: *4* test_invariant_when_new_dimension_more_than_old
    @with_checker
    def test_invariant_when_new_dimension_more_than_old(self,
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
    #@-others
#@+node:gcross.20111013183808.3919: *3* normalizeAndDenormalize
class TestNormalizeAndDenormalize(TestCase):
    #@+others
    #@+node:gcross.20111013183808.3920: *4* test_random
    @with_checker
    def test_random(self,
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
    #@-others
#@+node:gcross.20111022200315.1268: *3* truncateConnectionToSelf
class TestTruncateConnectionToSelf(TestCase):
    #@+others
    #@+node:gcross.20111022200315.1269: *4* test_matrix_invariant_under_unfiltered
    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_unfiltered(self,
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
    #@+node:gcross.20111022200315.1273: *4* test_matrix_invariant_under_zero_filtered
    @with_checker(number_of_calls=10)
    def test_matrix_invariant_under_zero_filtered(self,
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
    #@-others
#@-others
#@-leo
