#@+leo-ver=5-thin
#@+node:gcross.20111009135633.1137: * @file test.py
#@+<< Imports >>
#@+node:gcross.20111009135633.1138: ** << Imports >>
from copy import copy
import numpy
from numpy import all, allclose, array, dot, identity, product, tensordot
from numpy.random import rand
from paycheck import *
from random import randint
import unittest

from flatland.grid import *
from flatland.tensors import *
from flatland.utils import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111009135633.2985: ** Functions
#@+node:gcross.20111009135633.2987: *3* crand
def crand(*shape):
    return rand(*shape)*2-1+rand(*shape)*2j-1j
#@+node:gcross.20111028110210.1335: *3* ensurePhysicalDimensionSufficientlyLarge
def ensurePhysicalDimensionSufficientlyLarge(tensor,index,dimension):
    if dimension > product(withoutIndex(tensor.data.shape,index)):
        new_shape = list(tensor.data.shape)
        new_shape[0] = dimension
        return type(tensor)(crand(*new_shape))
    else:
        return tensor
#@+node:gcross.20111028110210.1337: *3* ensurePhysicalDimensionSufficientlyLargeToNormalize
def ensurePhysicalDimensionSufficientlyLargeToNormalize(tensor,index):
    return ensurePhysicalDimensionSufficientlyLarge(tensor,index,tensor.data.shape[index])
#@+node:gcross.20111028110210.1324: *3* randomIndex
def randomIndex(ndim):
    return randint(0,ndim-1)
#@+node:gcross.20111028110210.1323: *3* randomShape
def randomShape(ndim):
    return [randint(1,5) for _ in range(ndim)]
#@+node:gcross.20111028110210.1328: *3* randomShapeAgreeingWith
def randomShapeAgreeingWith(ndim,index,other_dimension):
    shape = randomShape(ndim)
    shape[index] = other_dimension
    return shape
#@+node:gcross.20111028110210.1320: *3* randomTensor
def randomTensor(ndim):
    return crand(*randomShape(ndim))
#@+node:gcross.20111028110210.1330: *3* randomTensorAgreeingWith
def randomTensorAgreeingWith(ndim,index,other_dimension):
    return crand(*randomShapeAgreeingWith(ndim,index,other_dimension))
#@+node:gcross.20111028110210.1322: *3* randomTensorAndIndex
def randomTensorAndIndex(ndim):
    return randomTensor(ndim), randomIndex(ndim)
#@+node:gcross.20111028110210.1326: *3* randomTensorAndIndexAgreeingWith
def randomTensorAndIndexAgreeingWith(ndim,other_dimension):
    index = randomIndex(ndim)
    return randomTensorAgreeingWith(ndim,index,other_dimension), index
#@+node:gcross.20111009135633.2977: ** Classes
#@+node:gcross.20111009135633.2978: *3* TestCase
class TestCase(unittest.TestCase):
    #@+others
    #@+node:gcross.20111009135633.2979: *4* assertAllClose
    def assertAllClose(self,v1,v2):
        v1 = array(v1)
        v2 = array(v2)
        self.assertEqual(v1.shape,v2.shape)
        self.assertTrue(allclose(v1,v2))
    #@+node:gcross.20111009135633.2980: *4* assertAllEqual
    def assertAllEqual(self,v1,v2):
        v1 = array(v1)
        v2 = array(v2)
        self.assertEqual(v1.shape,v2.shape)
        self.assertTrue(all(v1 == v2))
    #@+node:gcross.20111017110141.1255: *4* assertIsNormalized
    def assertIsNormalized(self,tensor,index):
        indices = list(range(tensor.ndim))
        del indices[index]
        self.assertAllClose(tensordot(tensor,tensor.conj(),(indices,indices)),identity(tensor.shape[index]))
    #@+node:gcross.20111009135633.2981: *4* assertVanishing
    def assertVanishing(self,v):
        self.assertAlmostEqual(norm(v),0)
    #@-others
#@+node:gcross.20111009135633.2982: ** Tests
#@+others
#@+node:gcross.20111013183808.3918: *3* Functions
#@+node:gcross.20111022200315.1323: *4* compressConnectionBetween
class TestCompressConnectionBetween(TestCase):
    #@+others
    #@+node:gcross.20111022200315.1324: *5* test_matrix_invariant_under_keep_all
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
    #@+node:gcross.20111022200315.1327: *5* test_matrix_invariant_under_threshold_zero
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
#@+node:gcross.20111022200315.1282: *4* compressConnectionToSelf
class TestCompressConnectionToSelf(TestCase):
    #@+others
    #@+node:gcross.20111022200315.1283: *5* test_matrix_invariant_under_keep_all
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
    #@+node:gcross.20111022200315.1284: *5* test_matrix_invariant_under_threshold_zero
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
#@+node:gcross.20111024143336.1329: *4* compressConnectionUsingFirstTensorBetween
class TestCompressConnectionUsingFirstTensorBetween(TestCase):
    #@+others
    #@+node:gcross.20111024143336.1330: *5* test_join_invariant_under_keep_all
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
    #@+node:gcross.20111024143336.1331: *5* test_join_invariant_under_threshold_zero
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
#@+node:gcross.20111009135633.2983: *4* formContractor
class TestFormContractor(TestCase):
    #@+others
    #@+node:gcross.20111009135633.2984: *5* test_trivial_case_xD
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
    #@+node:gcross.20111009135633.2988: *5* test_matvec
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
    #@+node:gcross.20111009135633.2992: *5* test_matmat
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
    #@+node:gcross.20111009135633.2993: *5* test_r3r3
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
    #@+node:gcross.20111009135633.2997: *5* test_r3r2
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
    #@+node:gcross.20111009135633.2999: *5* test_matmatmat
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
    #@+node:gcross.20111009135633.3000: *5* test_triangle
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
    #@+node:gcross.20111013080525.1215: *5* test_1
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
    #@+node:gcross.20111013080525.1217: *5* test_2
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
#@+node:gcross.20111028110210.1318: *4* increaseDimensionUsingFirstTensorOnlyBetween
class TestIncreaseDimensionUsingFirstTensorOnlyBetween(TestCase):
    #@+others
    #@+node:gcross.20111028110210.1319: *5* test_invariant_when_new_dimension_same_as_old
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
    #@+node:gcross.20111028110210.1332: *5* test_invariant_when_new_dimension_more_than_old
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
#@+node:gcross.20111013183808.3919: *4* normalizeAndDenormalize
class TestNormalizeAndDenormalize(TestCase):
    #@+others
    #@+node:gcross.20111013183808.3920: *5* test_random
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

        self.assertIsNormalized(new_tensor_1,index_1)
        self.assertAllClose(
            tensordot(tensor_1,tensor_2,(index_1,index_2)),
            tensordot(new_tensor_1,new_tensor_2,(index_1,index_2))
        )
    #@-others
#@+node:gcross.20111022200315.1268: *4* truncateConnectionToSelf
class TestTruncateConnectionToSelf(TestCase):
    #@+others
    #@+node:gcross.20111022200315.1269: *5* test_matrix_invariant_under_unfiltered
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
    #@+node:gcross.20111022200315.1273: *5* test_matrix_invariant_under_zero_filtered
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
#@+node:gcross.20111009193003.1168: *3* Tensors
#@+node:gcross.20111009193003.5240: *4* StateCornerSite
class TestStateCornerSite(TestCase):
    #@+others
    #@+node:gcross.20111013080525.1205: *5* absorbSideSiteAtCounterClockwise
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtCounterClockwise(self):
        corner = \
            StateCornerSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                randomize = True,
            )
        site = \
            StateSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = corner.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbSideSiteAtCounterClockwise.contract(corner.data,site.data),
            corner.absorbSideSiteAtCounterClockwise(site).data
        )

    test_absorbSideSiteAtCounterClockwise.contract = \
        formContractor(
            ['C','S'],
            [
                (('C',2),('S',1)),
            ],
            [
                [('C',0),('S',0)],
                [('C',1),('S',3)],
                [('S',2)],
            ]
        )
    #@+node:gcross.20111013080525.1209: *5* absorbSideSiteAtClockwise
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtClockwise(self):
        corner = \
            StateCornerSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                randomize = True,
            )
        site = \
            StateSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = corner.clockwise_dimension,
                inward_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbSideSiteAtClockwise.contract(corner.data,site.data),
            corner.absorbSideSiteAtClockwise(site).data
        )

    test_absorbSideSiteAtClockwise.contract = \
        formContractor(
            ['C','S'],
            [
                (('C',1),('S',2)),
            ],
            [
                [('C',0),('S',0)],
                [('S',1)],
                [('C',2),('S',3)],
            ]
        )
    #@+node:gcross.20111009193003.5241: *5* formBoundary
    @with_checker(number_of_calls=10)
    def test_formBoundary(self):
        site = \
            StateCornerSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                physical_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_formBoundary.contract(site.data,site.data.conj()),
            site.formBoundary().data
        )

    test_formBoundary.contract = \
        formContractor(
            ['T','T*'],
            [
                (('T',0),('T*',0)),
            ],
            [
                [('T',1),('T*',1)],
                [('T',2),('T*',2)],
            ]
        )
    #@-others
#@+node:gcross.20111009193003.5248: *4* StateSideBoundary
class TestStateSideBoundary(TestCase):
    #@+others
    #@+node:gcross.20111009193003.5250: *5* absorbCounterClockwiseCornerBoundary
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseCornerBoundary(self):
        side = \
            StateSideBoundary(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        corner = \
            StateCornerBoundary(
                clockwise_dimension = side.counterclockwise_dimension,
                counterclockwise_dimension = side.counterclockwise_dimension,
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbCounterClockwiseCornerBoundary.contract(side.data,corner.data),
            side.absorbCounterClockwiseCornerBoundary(corner).data
        )

    test_absorbCounterClockwiseCornerBoundary.contract = \
        formContractor(
            ['S','C'],
            [
                (('C',0),('S',1)),
            ],
            [
                [('S',0)],
                [('C',1)],
                [('S',2)],
                [('S',3)],
            ]
        )
    #@+node:gcross.20111009193003.5264: *5* absorbCounterClockwiseSideBoundary
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseSideBoundary(self):
        side1 = \
            StateSideBoundary(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        side2 = \
            StateSideBoundary(
                clockwise_dimension = side1.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_absorbCounterClockwiseSideBoundary.contract(side1.data,side2.data),
            side1.absorbCounterClockwiseSideBoundary(side2).data
        )

    test_absorbCounterClockwiseSideBoundary.contract = \
        formContractor(
            ['A','B'],
            [
                (('B',0),('A',1)),
            ],
            [
                [('A',0)],
                [('B',1)],
                [('A',2),('B',2)],
                [('A',3),('B',3)],
            ]
        )
    #@-others
#@+node:gcross.20111009193003.1169: *4* StateSideSite
class TestStateSideSite(TestCase):
    #@+others
    #@+node:gcross.20111013080525.1243: *5* absorbCenterSite_downward
    @with_checker(number_of_calls=10)
    def test_absorbCenterSite_upward(self):
        site = \
            StateSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        center = \
            StateCenterSite(
                physical_dimension = randint(1,5),
                rightward_dimension = randint(1,5),
                upward_dimension = randint(1,5),
                leftward_dimension = randint(1,5),
                downward_dimension = site.inward_dimension,
                randomize = True,
            )

        self.assertAllClose(
             tensordot(site.data,center.data,(3,4))
            .transpose(0,3,1,6,2,4,5)
            .reshape(
                site.physical_dimension*center.physical_dimension,
                site.clockwise_dimension*center.leftward_dimension,
                site.counterclockwise_dimension*center.rightward_dimension,
                center.upward_dimension,
             )

            ,site.absorbCenterSite(center,3).data
        )
    #@+node:gcross.20111013080525.1240: *5* absorbCenterSite_leftward
    @with_checker(number_of_calls=10)
    def test_absorbCenterSite_leftward(self):
        site = \
            StateSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        center = \
            StateCenterSite(
                physical_dimension = randint(1,5),
                rightward_dimension = randint(1,5),
                upward_dimension = randint(1,5),
                leftward_dimension = site.inward_dimension,
                downward_dimension = randint(1,5),
                randomize = True,
            )

        self.assertAllClose(
             tensordot(site.data,center.data,(3,3))
            .transpose(0,3,1,5,2,6,4)
            .reshape(
                site.physical_dimension*center.physical_dimension,
                site.clockwise_dimension*center.upward_dimension,
                site.counterclockwise_dimension*center.downward_dimension,
                center.rightward_dimension,
             )

            ,site.absorbCenterSite(center,2).data
        )
    #@+node:gcross.20111013080525.1236: *5* absorbCenterSite_rightward
    @with_checker(number_of_calls=10)
    def test_absorbCenterSite_rightward(self):
        site = \
            StateSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        center = \
            StateCenterSite(
                physical_dimension = randint(1,5),
                rightward_dimension = randint(1,5),
                upward_dimension = site.inward_dimension,
                leftward_dimension = randint(1,5),
                downward_dimension = randint(1,5),
                randomize = True,
            )

        self.assertAllClose(
             tensordot(site.data,center.data,(3,2))
            .transpose(0,3,1,4,2,5,6)
            .reshape(
                site.physical_dimension*center.physical_dimension,
                site.clockwise_dimension*center.rightward_dimension,
                site.counterclockwise_dimension*center.leftward_dimension,
                center.downward_dimension,
             )

            ,site.absorbCenterSite(center,1).data
        )
    #@+node:gcross.20111013080525.1238: *5* absorbCenterSite_upward
    @with_checker(number_of_calls=10)
    def test_absorbCenterSite_upward(self):
        site = \
            StateSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        center = \
            StateCenterSite(
                physical_dimension = randint(1,5),
                rightward_dimension = site.inward_dimension,
                upward_dimension = randint(1,5),
                leftward_dimension = randint(1,5),
                downward_dimension = randint(1,5),
                randomize = True,
            )

        self.assertAllClose(
             tensordot(site.data,center.data,(3,1))
            .transpose(0,3,1,6,2,4,5)
            .reshape(
                site.physical_dimension*center.physical_dimension,
                site.clockwise_dimension*center.downward_dimension,
                site.counterclockwise_dimension*center.upward_dimension,
                center.leftward_dimension,
             )

            ,site.absorbCenterSite(center,0).data
        )
    #@+node:gcross.20111009193003.1170: *5* formBoundary
    @with_checker(number_of_calls=10)
    def test_formBoundary(self):
        site = \
            StateSideSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                physical_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_formBoundary.contract(site.data,site.data.conj()),
            site.formBoundary().data
        )

    test_formBoundary.contract = \
        formContractor(
            ['T','T*'],
            [
                (('T',0),('T*',0)),
            ],
            [
                [('T',1),('T*',1)],
                [('T',2),('T*',2)],
                [('T',3)],
                [('T*',3)],
            ]
        )
    #@-others
#@+node:gcross.20111009193003.5265: *3* Grid
class TestGrid(TestCase):
    #@+others
    #@+node:gcross.20111013080525.1249: *4* randomGrid
    @staticmethod
    def randomGrid():
        grid = Grid(1)
        grid.sides = []
        for _ in range(4):
            clockwise_dimension = randint(1,3)
            counterclockwise_dimension = randint(1,3)
            physical_dimension = randint(1,3)
            inward_dimension = randint(1,3)
            grid.sides.append(StateSideSite(
                clockwise_dimension = clockwise_dimension,
                counterclockwise_dimension = counterclockwise_dimension,
                inward_dimension = inward_dimension,
                physical_dimension = physical_dimension,
                randomize = True
            ))
        grid.corners = [
            StateCornerSite(
                clockwise_dimension = grid.sides[i].counterclockwise_dimension,
                counterclockwise_dimension = grid.sides[CCW(i)].clockwise_dimension,
                physical_dimension = randint(1,3),
                randomize = True
            )
            for i in range(4)
        ]
        grid.center = \
            StateCenterSite(
                physical_dimension=randint(1,3),
                rightward_dimension=grid.sides[0].inward_dimension,
                upward_dimension=grid.sides[1].inward_dimension,
                leftward_dimension=grid.sides[2].inward_dimension,
                downward_dimension=grid.sides[3].inward_dimension,
                randomize=True,
            )
        return grid
    #@+node:gcross.20111024143336.1335: *4* test_compressConnectionBetweenSideAndCenter_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_connection_dimension = grid.sides[direction].inward_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCenter(direction,keep=len)
        self.assertLessEqual(grid.sides[direction].inward_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1337: *4* test_compressConnectionBetweenSideAndCenter_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        number_to_keep = randint(1,min(
            product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.inward_index)),
            grid.sides[direction].inward_dimension
        ))
        grid.compressConnectionBetweenSideAndCenter(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.sides[direction].inward_dimension)
        self.assertEqual(number_to_keep,grid.center.bandwidthDimension(direction))
        grid.computeNormalization()
    #@+node:gcross.20111024143336.1339: *4* test_compressConnectionBetweenSideAndCenter_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_connection_dimension = grid.sides[direction].inward_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCenter(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].inward_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1299: *4* test_compressConnectionBetweenSideAndClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_connection_dimension = grid.sides[direction].clockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=len)
        self.assertLessEqual(grid.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1305: *4* test_compressConnectionBetweenSideAndClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.clockwise_index)),
                    product(withoutIndex(grid.corners[CW(direction)].data.shape,StateCornerSite.counterclockwise_index)),
                )
            )
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,grid.corners[CW(direction)].counterclockwise_dimension)
        grid.computeNormalization()
    #@+node:gcross.20111024143336.1303: *4* test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_connection_dimension = grid.sides[direction].clockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1342: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_connection_dimension = grid.sides[direction].counterclockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=len)
        self.assertLessEqual(grid.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1307: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.counterclockwise_index)),
                    product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.clockwise_index)),
                )
            )
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,grid.corners[direction].clockwise_dimension)
        grid.computeNormalization()
    #@+node:gcross.20111022200315.1340: *4* test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_connection_dimension = grid.sides[direction].counterclockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1288: *4* test_compressCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_all(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_normalization = grid.computeNormalization()
        number_to_keep = product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.physical_index))
        grid.compressCorner(direction,keep=number_to_keep)
        self.assertEqual(grid.corners[direction].physical_dimension,number_to_keep)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1290: *4* test_compressCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_normalization = grid.computeNormalization()
        number_to_keep = randint(1,product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.physical_index)))
        grid.compressCorner(direction,keep=number_to_keep)
        self.assertEqual(grid.corners[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111022200315.1286: *4* test_compressCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_physical_dimension = grid.corners[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        grid.compressCorner(direction,threshold=0)
        self.assertLessEqual(grid.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1298: *4* test_compressSide_keep_all
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_all(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_normalization = grid.computeNormalization()
        number_to_keep = product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.physical_index))
        grid.compressSide(direction,keep=number_to_keep)
        self.assertEqual(grid.sides[direction].physical_dimension,number_to_keep)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1300: *4* test_compressSide_keep_some
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_normalization = grid.computeNormalization()
        number_to_keep = randint(1,product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.physical_index)))
        grid.compressSide(direction,keep=number_to_keep)
        self.assertEqual(grid.sides[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111022200315.1294: *4* test_compressSide_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressSide_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        old_physical_dimension = grid.sides[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        grid.compressSide(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111013165152.1231: *4* test_computeNormalization_random
    @with_checker(number_of_calls=10)
    def test_computeNormalization_random(self):
        grid = self.randomGrid()
        self.assertAlmostEqual(
            self.test_computeNormalization_random.contract(*([grid.center.data,grid.center.data.conj()] + [x.formBoundary().data for x in grid.sides + grid.corners])),
            grid.computeNormalization(),
        )

    test_computeNormalization_random.contract = \
        formContractor(
            (['O','O*'] + ['S{}'.format(i) for i in range(4)] + ['C{}'.format(i) for i in range(4)]),
            ([(('S{}'.format(i),1),('C{}'.format(i),0)) for i in range(4)]
            +[(('C{}'.format(i),1),('S{}'.format(CCW(i)),0)) for i in range(4)]
            +[(('S{}'.format(i),2),('O',1+i)) for i in range(4)]
            +[(('S{}'.format(i),3),('O*',1+i)) for i in range(4)]
            +[(('O',0),('O*',0))]
            ),
            []
        )
    #@+node:gcross.20111013165152.1227: *4* test_computeNormalization_trivial
    def test_computeNormalization_trivial(self):
        for physical_dimension in range(1,5):
            self.assertAlmostEqual(Grid(physical_dimension).computeNormalization(),1)
    #@+node:gcross.20111013080525.1263: *4* test_computeNormalizationConditionNumber_post_contract
    @with_checker(number_of_calls=10)
    def test_computeNormalizationConditionNumber_post_contract(self,
        physical_dimension = irange(1,5),
        number_of_contractions = irange(0,5),
    ):
        grid = Grid(physical_dimension)
        for _ in range(number_of_contractions):
            grid.contract(randint(0,3))
        self.assertAlmostEqual(grid.computeNormalizationConditionNumber(),1)
    #@+node:gcross.20111013080525.1261: *4* test_computeNormalizationConditionNumber_trivial
    def test_computeNormalizationConditionNumber_trivial(self):
        for physical_dimension in range(1,5):
            self.assertAlmostEqual(Grid(physical_dimension).computeNormalizationConditionNumber(),1)
    #@+node:gcross.20111010182600.1199: *4* test_computeNormalizationMatrix_random
    @with_checker(number_of_calls=10)
    def test_computeNormalizationMatrix_random(self):
        grid = self.randomGrid()
        self.assertAllClose(
            self.test_computeNormalizationMatrix_random.contract(*([identity(grid.physical_dimension)] + [x.formBoundary().data for x in grid.sides + grid.corners])),
            grid.computeNormalizationMatrix(),
        )

    test_computeNormalizationMatrix_random.contract = \
        formContractor(
            (['I'] + ['S{}'.format(i) for i in range(4)] + ['C{}'.format(i) for i in range(4)]),
            ([(('S{}'.format(i),1),('C{}'.format(i),0)) for i in range(4)]
            +[(('C{}'.format(i),1),('S{}'.format(CCW(i)),0)) for i in range(4)]
            ),
            [
                [('I',0)] + [('S{}'.format(i),2) for i in range(4)],
                [('I',1)] + [('S{}'.format(i),3) for i in range(4)],
            ]
        )
    #@+node:gcross.20111010182517.1196: *4* test_computeNormalizationMatrix_trivial
    def test_computeNormalizationMatrix_trivial(self):
        for physical_dimension in range(1,5):
            self.assertAllClose(Grid(physical_dimension).computeNormalizationMatrix(),identity(physical_dimension))
    #@+node:gcross.20111013080525.1257: *4* test_contract_downward
    @with_checker(number_of_calls=10)
    def test_contract_downward(self):
        grid = self.randomGrid()
        for forward_direction in [1,3]:
            reverse_direction = OPP(forward_direction)
            forward_dimension = grid.bandwidthDimension(forward_direction)
            reverse_dimension = grid.bandwidthDimension(reverse_direction)
            if forward_dimension < reverse_dimension:
                grid.sides[forward_direction] = \
                    ensurePhysicalDimensionSufficientlyLarge(
                        grid.sides[forward_direction],
                        StateSideSite.inward_index,
                        reverse_dimension
                    )
                grid.increaseSingleDirectionBandwidthDimensionTo(reverse_dimension,forward_direction)
        sides = copy(grid.sides)
        corners = copy(grid.corners)
        center = grid.center
        corners[3] = corners[3].absorbSideSiteAtCounterClockwise(sides[0])
        corners[2] = corners[2].absorbSideSiteAtClockwise(sides[2])
        sides[3] = sides[3].absorbCenterSite(center,3)
        grid.contract(3)
        for correct_side, actual_side in zip(sides,grid.sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(corners,grid.corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
    #@+node:gcross.20111013080525.1253: *4* test_contract_leftward
    @with_checker(number_of_calls=10)
    def test_contract_leftward(self):
        grid = self.randomGrid()
        for forward_direction in [0,2]:
            reverse_direction = OPP(forward_direction)
            forward_dimension = grid.bandwidthDimension(forward_direction)
            reverse_dimension = grid.bandwidthDimension(reverse_direction)
            if forward_dimension < reverse_dimension:
                grid.sides[forward_direction] = \
                    ensurePhysicalDimensionSufficientlyLarge(
                        grid.sides[forward_direction],
                        StateSideSite.inward_index,
                        reverse_dimension
                    )
                grid.increaseSingleDirectionBandwidthDimensionTo(reverse_dimension,forward_direction)
        sides = copy(grid.sides)
        corners = copy(grid.corners)
        center = grid.center
        corners[2] = corners[2].absorbSideSiteAtCounterClockwise(sides[3])
        corners[1] = corners[1].absorbSideSiteAtClockwise(sides[1])
        sides[2] = sides[2].absorbCenterSite(center,2)
        grid.contract(2)
        for correct_side, actual_side in zip(sides,grid.sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(corners,grid.corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
    #@+node:gcross.20111013080525.1248: *4* test_contract_rightward
    @with_checker(number_of_calls=10)
    def test_contract_rightward(self):
        grid = self.randomGrid()
        for forward_direction in [0,2]:
            reverse_direction = OPP(forward_direction)
            forward_dimension = grid.bandwidthDimension(forward_direction)
            reverse_dimension = grid.bandwidthDimension(reverse_direction)
            if forward_dimension < reverse_dimension:
                grid.sides[forward_direction] = \
                    ensurePhysicalDimensionSufficientlyLarge(
                        grid.sides[forward_direction],
                        StateSideSite.inward_index,
                        reverse_dimension
                    )
                grid.increaseSingleDirectionBandwidthDimensionTo(reverse_dimension,forward_direction)
        sides = copy(grid.sides)
        corners = copy(grid.corners)
        center = grid.center
        corners[0] = corners[0].absorbSideSiteAtCounterClockwise(sides[1])
        corners[-1] = corners[-1].absorbSideSiteAtClockwise(sides[-1])
        sides[0] = sides[0].absorbCenterSite(center,0)
        grid.contract(0)
        for correct_side, actual_side in zip(sides,grid.sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(corners,grid.corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
    #@+node:gcross.20111013080525.1251: *4* test_contract_upward
    @with_checker(number_of_calls=10)
    def test_contract_upward(self):
        grid = self.randomGrid()
        for forward_direction in [1,3]:
            reverse_direction = OPP(forward_direction)
            forward_dimension = grid.bandwidthDimension(forward_direction)
            reverse_dimension = grid.bandwidthDimension(reverse_direction)
            if forward_dimension < reverse_dimension:
                grid.sides[forward_direction] = \
                    ensurePhysicalDimensionSufficientlyLarge(
                        grid.sides[forward_direction],
                        StateSideSite.inward_index,
                        reverse_dimension
                    )
                grid.increaseSingleDirectionBandwidthDimensionTo(reverse_dimension,forward_direction)
        sides = copy(grid.sides)
        corners = copy(grid.corners)
        center = grid.center
        corners[1] = corners[1].absorbSideSiteAtCounterClockwise(sides[2])
        corners[0] = corners[0].absorbSideSiteAtClockwise(sides[0])
        sides[1] = sides[1].absorbCenterSite(center,1)
        grid.contract(1)
        for correct_side, actual_side in zip(sides,grid.sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(corners,grid.corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
    #@+node:gcross.20111014172511.1244: *4* test_increaseAxialBandwidthDimensionsBy
    @with_checker
    def test_increaseAxialBandwidthDimensionsBy(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomGrid()

        bandwidth_dimensions = list(grid.bandwidthDimensions())
        bandwidth_dimensions[direction] += increment
        bandwidth_dimensions[OPP(direction)] += increment

        grid.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )
        grid.sides[OPP(direction)] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[OPP(direction)],
                StateSideSite.inward_index,
                bandwidth_dimensions[OPP(direction)]
            )

        old_normalization = grid.computeNormalization()

        grid.increaseAxialBandwidthDimensionsBy(increment,direction)

        self.assertEqual(grid.bandwidthDimensions(),grid.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,grid.computeNormalization())
    #@+node:gcross.20111014172511.1246: *4* test_increaseAxialBandwidthDimensionsTo
    @with_checker
    def test_increaseAxialBandwidthDimensionsTo(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomGrid()

        bandwidth_dimensions = list(grid.bandwidthDimensions())
        bandwidth_dimensions[direction] = max(bandwidth_dimensions[direction],bandwidth_dimensions[OPP(direction)])
        bandwidth_dimensions[direction] += increment
        bandwidth_dimensions[OPP(direction)] = bandwidth_dimensions[direction]

        grid.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )
        grid.sides[OPP(direction)] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[OPP(direction)],
                StateSideSite.inward_index,
                bandwidth_dimensions[OPP(direction)]
            )

        old_normalization = grid.computeNormalization()

        grid.increaseAxialBandwidthDimensionsTo(bandwidth_dimensions[direction],direction)

        self.assertEqual(grid.bandwidthDimensions(),grid.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,grid.computeNormalization())
    #@+node:gcross.20111013165152.1225: *4* test_increaseSingleDirectionBandwidthDimensionBy
    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionBy(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomGrid()

        bandwidth_dimensions = list(grid.bandwidthDimensions())
        bandwidth_dimensions[direction] += increment

        grid.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )

        old_normalization = grid.computeNormalization()

        grid.increaseSingleDirectionBandwidthDimensionBy(increment,direction)

        self.assertEqual(grid.bandwidthDimensions(),grid.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,grid.computeNormalization())
    #@+node:gcross.20111014113710.1241: *4* test_increaseSingleDirectionBandwidthDimensionTo
    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionTo(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomGrid()

        bandwidth_dimensions = list(grid.bandwidthDimensions())
        bandwidth_dimensions[direction] += increment

        grid.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLarge(
                grid.sides[direction],
                StateSideSite.inward_index,
                bandwidth_dimensions[direction]
            )

        old_normalization = grid.computeNormalization()

        grid.increaseSingleDirectionBandwidthDimensionTo(bandwidth_dimensions[direction],direction)

        self.assertEqual(grid.bandwidthDimensions(),grid.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,grid.computeNormalization())
    #@+node:gcross.20111017110141.1273: *4* test_normalizeCornerAndDenormalizeClockwiseSide
    @with_checker(number_of_calls=10)
    def test_normalizeCornerAndDenormalizeClockwiseSide(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        grid.corners[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                grid.corners[direction],
                StateCornerSite.clockwise_index
            )
        old_normalization = grid.computeNormalization()
        grid.normalizeCornerAndDenormalizeClockwiseSide(direction)
        self.assertIsNormalized(grid.corners[direction].data,StateCornerSite.clockwise_index)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111017110141.1275: *4* test_normalizeCornerAndDenormalizeCounterClockwiseSide
    @with_checker(number_of_calls=10)
    def test_normalizeCornerAndDenormalizeCounterClockwiseSide(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        grid.corners[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                grid.corners[direction],
                StateCornerSite.counterclockwise_index
            )
        old_normalization = grid.computeNormalization()
        grid.normalizeCornerAndDenormalizeCounterClockwiseSide(direction)
        self.assertIsNormalized(grid.corners[direction].data,StateCornerSite.counterclockwise_index)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111014113710.1231: *4* test_normalizeSide
    @with_checker(number_of_calls=10)
    def test_normalizeSide(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid()
        grid.sides[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                grid.sides[direction],
                StateSideSite.inward_index
            )
        old_normalization = grid.computeNormalization()
        grid.normalizeSide(direction)
        self.assertIsNormalized(grid.sides[direction].data,StateSideSite.inward_index)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@-others
#@-others

tests = [
    TestCompressConnectionBetween,
    TestCompressConnectionToSelf,
    TestCompressConnectionUsingFirstTensorBetween,
    TestFormContractor,
    TestIncreaseDimensionUsingFirstTensorOnlyBetween,
    TestNormalizeAndDenormalize,
    TestTruncateConnectionToSelf,

    TestStateCornerSite,
    TestStateSideBoundary,
    TestStateSideSite,

    TestGrid,
]
#@-others

#@+<< Runner >>
#@+node:gcross.20111009135633.2971: ** << Runner >>
numpy.set_printoptions(linewidth=132)

unittest.TextTestRunner(verbosity=2).run(unittest.TestSuite(map(unittest.defaultTestLoader.loadTestsFromTestCase, tests)))
#@-<< Runner >>
#@-leo
