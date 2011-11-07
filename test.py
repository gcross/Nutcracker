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
#@+node:gcross.20111103170337.1371: *4* ExpectationSideBoundary
class TestExpectationSideBoundary(TestCase):
    #@+others
    #@+node:gcross.20111103170337.1372: *5* absorbCounterClockwiseCornerBoundary
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseCornerBoundary(self):
        side = \
            ExpectationSideBoundary(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_state_dimension = randint(1,5),
                inward_operator_dimension = randint(1,5),
                randomize = True,
            )
        corner = \
            CornerBoundary(
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
                (('C',CornerBoundary.clockwise_index),('S',ExpectationSideBoundary.counterclockwise_index)),
            ],
            [
                {"clockwise":[('S',ExpectationSideBoundary.clockwise_index)]
                ,"counterclockwise":[('C',CornerBoundary.counterclockwise_index)]
                ,"inward_state":[('S',ExpectationSideBoundary.inward_state_index)]
                ,"inward_operator":[('S',ExpectationSideBoundary.inward_operator_index)]
                ,"inward_state_conjugate":[('S',ExpectationSideBoundary.inward_state_conjugate_index)]
                }[name] for name in ExpectationSideBoundary._dimensions
            ]
        )
    #@+node:gcross.20111103170337.1373: *5* absorbCounterClockwiseSideBoundary
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseSideBoundary(self):
        side1 = \
            ExpectationSideBoundary(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_state_dimension = randint(1,5),
                inward_operator_dimension = randint(1,5),
                randomize = True,
            )
        side2 = \
            ExpectationSideBoundary(
                clockwise_dimension = side1.counterclockwise_dimension,
                counterclockwise_dimension = randint(1,5),
                inward_state_dimension = randint(1,5),
                inward_operator_dimension = randint(1,5),
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
                (('B',ExpectationSideBoundary.clockwise_index),('A',ExpectationSideBoundary.counterclockwise_index)),
            ],
            [
                {"clockwise":[('A',ExpectationSideBoundary.clockwise_index)]
                ,"counterclockwise":[('B',ExpectationSideBoundary.counterclockwise_index)]
                ,"inward_state":[(x,ExpectationSideBoundary.inward_state_index) for x in ('A','B')]
                ,"inward_operator":[(x,ExpectationSideBoundary.inward_operator_index) for x in ('A','B')]
                ,"inward_state_conjugate":[(x,ExpectationSideBoundary.inward_state_conjugate_index) for x in ('A','B')]
                }[name] for name in ExpectationSideBoundary._dimensions
            ]
        )
    #@-others
#@+node:gcross.20111009193003.5248: *4* NormalizationSideBoundary
class TestNormalizationSideBoundary(TestCase):
    #@+others
    #@+node:gcross.20111009193003.5250: *5* absorbCounterClockwiseCornerBoundary
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseCornerBoundary(self):
        side = \
            NormalizationSideBoundary(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        corner = \
            CornerBoundary(
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
                (('C',CornerBoundary.clockwise_index),('S',NormalizationSideBoundary.counterclockwise_index)),
            ],
            [
                {"clockwise":[('S',NormalizationSideBoundary.clockwise_index)]
                ,"counterclockwise":[('C',CornerBoundary.counterclockwise_index)]
                ,"inward":[('S',NormalizationSideBoundary.inward_index)]
                ,"inward_conjugate":[('S',NormalizationSideBoundary.inward_conjugate_index)]
                }[name] for name in NormalizationSideBoundary._dimensions
            ]
        )
    #@+node:gcross.20111009193003.5264: *5* absorbCounterClockwiseSideBoundary
    @with_checker(number_of_calls=10)
    def test_absorbCounterClockwiseSideBoundary(self):
        side1 = \
            NormalizationSideBoundary(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        side2 = \
            NormalizationSideBoundary(
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
                (('B',NormalizationSideBoundary.clockwise_index),('A',NormalizationSideBoundary.counterclockwise_index)),
            ],
            [
                {"clockwise":[('A',NormalizationSideBoundary.clockwise_index)]
                ,"counterclockwise":[('B',NormalizationSideBoundary.counterclockwise_index)]
                ,"inward":[(x,NormalizationSideBoundary.inward_index) for x in ('A','B')]
                ,"inward_conjugate":[(x,NormalizationSideBoundary.inward_conjugate_index) for x in ('A','B')]
                }[name] for name in NormalizationSideBoundary._dimensions
            ]
        )
    #@-others
#@+node:gcross.20111103170337.1353: *4* OperatorCornerSite
class TestOperatorCornerSite(TestCase):
    #@+others
    #@+node:gcross.20111103170337.1357: *5* absorbSideSiteAtClockwise
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtClockwise(self):
        corner = \
            OperatorCornerSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                randomize = True,
            )
        site = \
            OperatorSideSite(
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
                (('C',OperatorCornerSite.clockwise_index),('S',OperatorSideSite.counterclockwise_index)),
            ],
            [
                {"physical":[('C',OperatorCornerSite.physical_index),('S',OperatorSideSite.physical_index)]
                ,"physical_conjugate":[('C',OperatorCornerSite.physical_conjugate_index),('S',OperatorSideSite.physical_conjugate_index)]
                ,"counterclockwise":[('C',OperatorCornerSite.counterclockwise_index),('S',OperatorSideSite.inward_index)]
                ,"clockwise":[('S',OperatorSideSite.clockwise_index)]
                }[name] for name in OperatorCornerSite._dimensions
            ]
        )
    #@+node:gcross.20111103170337.1361: *5* absorbSideSiteAtCounterClockwise
    @with_checker(number_of_calls=10)
    def test_absorbSideSiteAtCounterClockwise(self):
        corner = \
            OperatorCornerSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                randomize = True,
            )
        site = \
            OperatorSideSite(
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
                (('C',OperatorCornerSite.counterclockwise_index),('S',OperatorSideSite.clockwise_index)),
            ],
            [
                {"physical":[('C',OperatorCornerSite.physical_index),('S',OperatorSideSite.physical_index)]
                ,"physical_conjugate":[('C',OperatorCornerSite.physical_conjugate_index),('S',OperatorSideSite.physical_conjugate_index)]
                ,"counterclockwise":[('S',OperatorSideSite.counterclockwise_index)]
                ,"clockwise":[('C',OperatorCornerSite.clockwise_index),('S',OperatorSideSite.inward_index)]
                }[name] for name in OperatorCornerSite._dimensions
            ]
        )
    #@+node:gcross.20111103170337.1355: *5* formExpectationBoundary
    @with_checker(number_of_calls=10)
    def test_formExpectationBoundary(self):
        state = \
            StateCornerSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                physical_dimension = randint(1,5),
                randomize = True,
            )
        operator = \
            OperatorCornerSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                physical_dimension = state.physical_dimension,
                randomize = True,
            )
        self.assertAllClose(
            self.test_formExpectationBoundary.contract(state.data,operator.data,state.data.conj()),
            operator.formExpectationBoundary(state).data
        )

    test_formExpectationBoundary.contract = \
        formContractor(
            ['S','O','S*'],
            [
                [('S',StateCornerSite.physical_index),('O',OperatorCornerSite.physical_index)],
                [('S*',StateCornerSite.physical_index),('O',OperatorCornerSite.physical_conjugate_index)],
            ],
            [
                {"clockwise":[('S',StateCornerSite.clockwise_index),('O',OperatorCornerSite.clockwise_index),('S*',StateCornerSite.clockwise_index)]
                ,"counterclockwise":[('S',StateCornerSite.counterclockwise_index),('O',OperatorCornerSite.counterclockwise_index),('S*',StateCornerSite.counterclockwise_index)]
                }[name] for name in CornerBoundary._dimensions
            ]
        )
    #@-others
#@+node:gcross.20111103110300.1375: *4* OperatorSideSite
class TestOperatorSideSite(TestCase):
    #@+others
    #@+node:gcross.20111103110300.1385: *5* absorbCenterSite
    @with_checker(number_of_calls=100)
    def test_absorbCenterSite(self,direction=irange(0,3)):
        side = \
            OperatorSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        center = OperatorCenterSite(**{
            "physical_dimension": randint(1,5),
            "rightward_dimension": randint(1,5),
            "upward_dimension": randint(1,5),
            "leftward_dimension": randint(1,5),
            "downward_dimension": randint(1,5),
            "randomize": True,
            OperatorCenterSite._bandwidth_dimensions[direction]+"_dimension": side.inward_dimension
        })
        self.assertAllClose(
             tensordot(side.data,center.data,(side.inward_index,center.bandwidthIndex(direction)))
            .transpose(
                side.physical_index,
                center.physical_index+4,
                side.physical_conjugate_index,
                center.physical_conjugate_index+4,
                side.clockwise_index,
                center.bandwidthIndex(CW(direction))+4 - (1 if CW(direction) > direction else 0),
                side.counterclockwise_index,
                center.bandwidthIndex(CCW(direction))+4 - (1 if CCW(direction) > direction else 0),
                center.bandwidthIndex(OPP(direction))+4 - (1 if OPP(direction) > direction else 0),
             )
            .reshape(
                side.physical_dimension*center.physical_dimension,
                side.physical_dimension*center.physical_dimension,
                side.clockwise_dimension*center.bandwidthDimension(CW(direction)),
                side.counterclockwise_dimension*center.bandwidthDimension(CCW(direction)),
                center.bandwidthDimension(OPP(direction)),
             )
            ,side.absorbCenterSite(center,direction).data
        )
    #@+node:gcross.20111103110300.1380: *5* formExpectationBoundary
    @with_checker(number_of_calls=10)
    def test_formExpectationBoundary(self):
        state = \
            StateSideSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                physical_dimension = randint(1,5),
                randomize = True,
            )
        operator = \
            OperatorSideSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                physical_dimension = state.physical_dimension,
                randomize = True,
            )
        self.assertAllClose(
            self.test_formExpectationBoundary.contract(state.data,operator.data,state.data.conj()),
            operator.formExpectationBoundary(state).data
        )

    test_formExpectationBoundary.contract = \
        formContractor(
            ['S','O','S*'],
            [
                [('S',StateSideSite.physical_index),('O',OperatorSideSite.physical_index)],
                [('S*',StateSideSite.physical_index),('O',OperatorSideSite.physical_conjugate_index)],
            ],
            [
                {"clockwise":[('S',StateSideSite.clockwise_index),('O',OperatorSideSite.clockwise_index),('S*',StateSideSite.clockwise_index)]
                ,"counterclockwise":[('S',StateSideSite.counterclockwise_index),('O',OperatorSideSite.counterclockwise_index),('S*',StateSideSite.counterclockwise_index)]
                ,"inward_state":[('S',StateSideSite.inward_index)]
                ,"inward_operator":[('O',OperatorSideSite.inward_index)]
                ,"inward_state_conjugate":[('S*',StateSideSite.inward_index)]
                }[name] for name in ExpectationSideBoundary._dimensions
            ]
        )
    #@-others
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
                (('C',StateCornerSite.counterclockwise_index),('S',StateSideSite.clockwise_index)),
            ],
            [
                {"physical":[('C',StateCornerSite.physical_index),('S',StateSideSite.physical_index)]
                ,"clockwise":[('C',StateCornerSite.clockwise_index),('S',StateSideSite.inward_index)]
                ,"counterclockwise":[('S',StateSideSite.counterclockwise_index)]
                }[name] for name in StateCornerSite._dimensions
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
                (('C',StateCornerSite.clockwise_index),('S',StateSideSite.counterclockwise_index)),
            ],
            [
                {"physical":[('C',StateCornerSite.physical_index),('S',StateSideSite.physical_index)]
                ,"counterclockwise":[('C',StateCornerSite.counterclockwise_index),('S',StateSideSite.inward_index)]
                ,"clockwise":[('S',StateSideSite.clockwise_index)]
                }[name] for name in StateCornerSite._dimensions
            ]
        )
    #@+node:gcross.20111009193003.5241: *5* formNormalizationBoundary
    @with_checker(number_of_calls=10)
    def test_formNormalizationBoundary(self):
        site = \
            StateCornerSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                physical_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_formNormalizationBoundary.contract(site.data,site.data.conj()),
            site.formNormalizationBoundary().data
        )

    test_formNormalizationBoundary.contract = \
        formContractor(
            ['S','S*'],
            [
                [('S'+c,StateCornerSite.physical_index) for c in ('','*')]
            ],
            [
                [('S'+c,getattr(StateCornerSite,name+'_index')) for c in ('','*')]
                for name in CornerBoundary._dimensions
            ]
        )
    #@-others
#@+node:gcross.20111009193003.1169: *4* StateSideSite
class TestStateSideSite(TestCase):
    #@+others
    #@+node:gcross.20111103110300.1383: *5* absorbCenterSite
    @with_checker(number_of_calls=100)
    def test_absorbCenterSite(self,direction=irange(0,3)):
        side = \
            StateSideSite(
                physical_dimension = randint(1,5),
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                randomize = True,
            )
        center = StateCenterSite(**{
            "physical_dimension": randint(1,5),
            "rightward_dimension": randint(1,5),
            "upward_dimension": randint(1,5),
            "leftward_dimension": randint(1,5),
            "downward_dimension": randint(1,5),
            "randomize": True,
            StateCenterSite._bandwidth_dimensions[direction]+"_dimension": side.inward_dimension
        })
        self.assertAllClose(
             tensordot(side.data,center.data,(side.inward_index,center.bandwidthIndex(direction)))
            .transpose(
                side.physical_index,
                center.physical_index+3,
                side.clockwise_index,
                center.bandwidthIndex(CW(direction))+3 - (1 if CW(direction) > direction else 0),
                side.counterclockwise_index,
                center.bandwidthIndex(CCW(direction))+3 - (1 if CCW(direction) > direction else 0),
                center.bandwidthIndex(OPP(direction))+3 - (1 if OPP(direction) > direction else 0),
             )
            .reshape(
                side.physical_dimension*center.physical_dimension,
                side.clockwise_dimension*center.bandwidthDimension(CW(direction)),
                side.counterclockwise_dimension*center.bandwidthDimension(CCW(direction)),
                center.bandwidthDimension(OPP(direction)),
             )
            ,side.absorbCenterSite(center,direction).data
        )
    #@+node:gcross.20111009193003.1170: *5* formNormalizationBoundary
    @with_checker(number_of_calls=10)
    def test_formNormalizationBoundary(self):
        site = \
            StateSideSite(
                clockwise_dimension = randint(1,5),
                counterclockwise_dimension = randint(1,5),
                inward_dimension = randint(1,5),
                physical_dimension = randint(1,5),
                randomize = True,
            )
        self.assertAllClose(
            self.test_formNormalizationBoundary.contract(site.data,site.data.conj()),
            site.formNormalizationBoundary().data
        )

    test_formNormalizationBoundary.contract = \
        formContractor(
            ['S','S*'],
            [
                [('S'+c,StateSideSite.physical_index) for c in ('','*')]
            ],
            [
                {"clockwise":[('S'+c,StateSideSite.clockwise_index) for c in ('','*')]
                ,"counterclockwise":[('S'+c,StateSideSite.counterclockwise_index) for c in ('','*')]
                ,"inward":[('S',StateSideSite.inward_index)]
                ,"inward_conjugate":[('S*',StateSideSite.inward_index)]
                }[name] for name in NormalizationSideBoundary._dimensions
            ]
        )
    #@-others
#@+node:gcross.20111103170337.1390: *3* Grids
#@+node:gcross.20111009193003.5265: *4* NormalizationGrid
class TestNormalizationGrid(TestCase):
    #@+others
    #@+node:gcross.20111013080525.1249: *5* randomNormalizationGrid
    @staticmethod
    def randomNormalizationGrid():
        grid = NormalizationGrid(1)
        horizontal_dimension = randint(1,3)
        vertical_dimension = randint(1,3)
        grid.center = \
            StateCenterSite(
                physical_dimension=randint(1,3),
                rightward_dimension=horizontal_dimension,
                upward_dimension=vertical_dimension,
                leftward_dimension=horizontal_dimension,
                downward_dimension=vertical_dimension,
                randomize=True,
            )
        grid.sides = [
            StateSideSite(
                clockwise_dimension = randint(1,3),
                counterclockwise_dimension = randint(1,3),
                inward_dimension = grid.center.bandwidthDimension(i),
                physical_dimension = randint(1,3),
                randomize = True
            )
            for i in range(4)
        ]
        grid.corners = [
            StateCornerSite(
                clockwise_dimension = grid.sides[i].counterclockwise_dimension,
                counterclockwise_dimension = grid.sides[CCW(i)].clockwise_dimension,
                physical_dimension = randint(1,3),
                randomize = True
            )
            for i in range(4)
        ]
        return grid
    #@+node:gcross.20111024143336.1335: *5* test_compressConnectionBetweenSideAndCenter_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].inward_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCenter(direction,keep=len)
        self.assertLessEqual(grid.sides[direction].inward_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1337: *5* test_compressConnectionBetweenSideAndCenter_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        number_to_keep = randint(1,min(
            product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.inward_index)),
            grid.sides[direction].inward_dimension
        ))
        grid.compressConnectionBetweenSideAndCenter(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.sides[direction].inward_dimension)
        self.assertEqual(number_to_keep,grid.center.bandwidthDimension(direction))
        grid.computeNormalization()
    #@+node:gcross.20111024143336.1339: *5* test_compressConnectionBetweenSideAndCenter_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCenter_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].inward_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCenter(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].inward_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1299: *5* test_compressConnectionBetweenSideAndClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].clockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=len)
        self.assertLessEqual(grid.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1305: *5* test_compressConnectionBetweenSideAndClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
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
    #@+node:gcross.20111024143336.1303: *5* test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].clockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1342: *5* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].counterclockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=len)
        self.assertLessEqual(grid.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111024143336.1307: *5* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
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
    #@+node:gcross.20111022200315.1340: *5* test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_connection_dimension = grid.sides[direction].counterclockwise_dimension
        old_normalization = grid.computeNormalization()
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1288: *5* test_compressCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_all(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_normalization = grid.computeNormalization()
        number_to_keep = product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.physical_index))
        grid.compressCorner(direction,keep=number_to_keep)
        self.assertEqual(grid.corners[direction].physical_dimension,number_to_keep)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1290: *5* test_compressCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        number_to_keep = randint(1,product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.physical_index)))
        grid.compressCorner(direction,keep=number_to_keep)
        self.assertEqual(grid.corners[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111022200315.1286: *5* test_compressCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_physical_dimension = grid.corners[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        grid.compressCorner(direction,threshold=0)
        self.assertLessEqual(grid.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1298: *5* test_compressSide_keep_all
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_all(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_normalization = grid.computeNormalization()
        number_to_keep = product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.physical_index))
        grid.compressSide(direction,keep=number_to_keep)
        self.assertEqual(grid.sides[direction].physical_dimension,number_to_keep)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111022200315.1300: *5* test_compressSide_keep_some
    @with_checker(number_of_calls=10)
    def test_compressSide_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        number_to_keep = randint(1,product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.physical_index)))
        grid.compressSide(direction,keep=number_to_keep)
        self.assertEqual(grid.sides[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111022200315.1294: *5* test_compressSide_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressSide_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        old_physical_dimension = grid.sides[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        grid.compressSide(direction,threshold=0)
        self.assertLessEqual(grid.sides[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111013165152.1231: *5* test_computeNormalization_random
    @with_checker(number_of_calls=10)
    def test_computeNormalization_random(self):
        grid = self.randomNormalizationGrid()
        self.assertAlmostEqual(
            self.test_computeNormalization_random.contract(*([grid.center.data,grid.center.data.conj()] + [x.formNormalizationBoundary().data for x in grid.sides + grid.corners])),
            grid.computeNormalization(),
        )

    test_computeNormalization_random.contract = \
        formContractor(
            (['O','O*'] + ['S{}'.format(i) for i in range(4)] + ['C{}'.format(i) for i in range(4)]),
            ([(('S{}'.format(i),NormalizationSideBoundary.counterclockwise_index),('C{}'.format(i),CornerBoundary.clockwise_index)) for i in range(4)]
            +[(('C{}'.format(i),CornerBoundary.counterclockwise_index),('S{}'.format(CCW(i)),NormalizationSideBoundary.clockwise_index)) for i in range(4)]
            +[(('S{}'.format(i),NormalizationSideBoundary.inward_index),('O',StateCenterSite.bandwidthIndex(i))) for i in range(4)]
            +[(('S{}'.format(i),NormalizationSideBoundary.inward_conjugate_index),('O*',StateCenterSite.bandwidthIndex(i))) for i in range(4)]
            +[(('O',StateCenterSite.physical_index),('O*',StateCenterSite.physical_index))]
            ),
            []
        )
    #@+node:gcross.20111013165152.1227: *5* test_computeNormalization_trivial
    def test_computeNormalization_trivial(self):
        for physical_dimension in range(1,5):
            self.assertAlmostEqual(NormalizationGrid(physical_dimension).computeNormalization(),1)
    #@+node:gcross.20111013080525.1263: *5* test_computeNormalizationConditionNumber_post_contract
    @with_checker(number_of_calls=10)
    def test_computeNormalizationConditionNumber_post_contract(self,
        physical_dimension = irange(1,5),
        number_of_contractions = irange(0,5),
    ):
        grid = NormalizationGrid(physical_dimension)
        for _ in range(number_of_contractions):
            grid.contract(randint(0,3))
        self.assertAlmostEqual(grid.computeNormalizationConditionNumber(),1)
    #@+node:gcross.20111013080525.1261: *5* test_computeNormalizationConditionNumber_trivial
    def test_computeNormalizationConditionNumber_trivial(self):
        for physical_dimension in range(1,5):
            self.assertAlmostEqual(NormalizationGrid(physical_dimension).computeNormalizationConditionNumber(),1)
    #@+node:gcross.20111010182600.1199: *5* test_computeNormalizationMatrix_random
    @with_checker(number_of_calls=10)
    def test_computeNormalizationMatrix_random(self):
        grid = self.randomNormalizationGrid()
        self.assertAllClose(
            self.test_computeNormalizationMatrix_random.contract(*([identity(grid.physical_dimension)] + [x.formNormalizationBoundary().data for x in grid.sides + grid.corners])),
            grid.computeNormalizationMatrix(),
        )

    test_computeNormalizationMatrix_random.contract = \
        formContractor(
            (['I'] + ['S{}'.format(i) for i in range(4)] + ['C{}'.format(i) for i in range(4)]),
            ([(('S{}'.format(i),NormalizationSideBoundary.counterclockwise_index),('C{}'.format(i),CornerBoundary.clockwise_index)) for i in range(4)]
            +[(('C{}'.format(i),CornerBoundary.counterclockwise_index),('S{}'.format(CCW(i)),NormalizationSideBoundary.clockwise_index)) for i in range(4)]
            ),
            [
                [('I',0)] + [('S{}'.format(i),NormalizationSideBoundary.inward_index) for i in range(4)],
                [('I',1)] + [('S{}'.format(i),NormalizationSideBoundary.inward_conjugate_index) for i in range(4)],
            ]
        )
    #@+node:gcross.20111010182517.1196: *5* test_computeNormalizationMatrix_trivial
    def test_computeNormalizationMatrix_trivial(self):
        for physical_dimension in range(1,5):
            self.assertAllClose(NormalizationGrid(physical_dimension).computeNormalizationMatrix(),identity(physical_dimension))
    #@+node:gcross.20111103170337.1388: *5* test_contract
    @with_checker(number_of_calls=100)
    def test_contract(self,direction=irange(0,3)):
        grid = self.randomNormalizationGrid()
        sides = copy(grid.sides)
        corners = copy(grid.corners)
        center = grid.center
        corners[direction] = corners[direction].absorbSideSiteAtCounterClockwise(sides[CCW(direction)])
        corners[CW(direction)] = corners[CW(direction)].absorbSideSiteAtClockwise(sides[CW(direction)])
        sides[direction] = sides[direction].absorbCenterSite(center,direction)
        grid.contract(direction)
        for correct_side, actual_side in zip(sides,grid.sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(corners,grid.corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
    #@+node:gcross.20111014172511.1244: *5* test_increaseAxialBandwidthDimensionsBy
    @with_checker
    def test_increaseAxialBandwidthDimensionsBy(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()

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
    #@+node:gcross.20111014172511.1246: *5* test_increaseAxialBandwidthDimensionsTo
    @with_checker
    def test_increaseAxialBandwidthDimensionsTo(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()

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
    #@+node:gcross.20111013165152.1225: *5* test_increaseSingleDirectionBandwidthDimensionBy
    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionBy(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()

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
    #@+node:gcross.20111014113710.1241: *5* test_increaseSingleDirectionBandwidthDimensionTo
    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionTo(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()

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
    #@+node:gcross.20111017110141.1273: *5* test_normalizeCornerAndDenormalizeClockwiseSide
    @with_checker(number_of_calls=10)
    def test_normalizeCornerAndDenormalizeClockwiseSide(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        grid.corners[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                grid.corners[direction],
                StateCornerSite.clockwise_index
            )
        old_normalization = grid.computeNormalization()
        grid.normalizeCornerAndDenormalizeClockwiseSide(direction)
        self.assertIsNormalized(grid.corners[direction].data,StateCornerSite.clockwise_index)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111017110141.1275: *5* test_normalizeCornerAndDenormalizeCounterClockwiseSide
    @with_checker(number_of_calls=10)
    def test_normalizeCornerAndDenormalizeCounterClockwiseSide(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
        grid.corners[direction] = \
            ensurePhysicalDimensionSufficientlyLargeToNormalize(
                grid.corners[direction],
                StateCornerSite.counterclockwise_index
            )
        old_normalization = grid.computeNormalization()
        grid.normalizeCornerAndDenormalizeCounterClockwiseSide(direction)
        self.assertIsNormalized(grid.corners[direction].data,StateCornerSite.counterclockwise_index)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
    #@+node:gcross.20111014113710.1231: *5* test_normalizeSide
    @with_checker(number_of_calls=10)
    def test_normalizeSide(self,
        direction = irange(0,3),
    ):
        grid = self.randomNormalizationGrid()
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
#@+node:gcross.20111103170337.1389: *4* ExpectationGrid
class TestExpectationGrid(TestCase):
    #@+others
    #@+node:gcross.20111103170337.1392: *5* randomExpectationGrid
    @staticmethod
    def randomExpectationGrid():
        grid = TestExpectationGrid.trivialExpectationGrid()
        O_horizontal_dimension = randint(1,2)
        O_vertical_dimension = randint(1,2)
        grid.O_center = \
            OperatorCenterSite(
                physical_dimension=randint(1,2),
                rightward_dimension=O_horizontal_dimension,
                upward_dimension=O_vertical_dimension,
                leftward_dimension=O_horizontal_dimension,
                downward_dimension=O_vertical_dimension,
                randomize=True,
            )
        grid.O_sides = [
            OperatorSideSite(
                clockwise_dimension = randint(1,2),
                counterclockwise_dimension = randint(1,2),
                inward_dimension = grid.O_center.bandwidthDimension(i),
                physical_dimension = randint(1,2),
                randomize = True
            )
            for i in range(4)
        ]
        grid.O_corners = [
            OperatorCornerSite(
                clockwise_dimension = grid.O_sides[i].counterclockwise_dimension,
                counterclockwise_dimension = grid.O_sides[CCW(i)].clockwise_dimension,
                physical_dimension = randint(1,2),
                randomize = True
            )
            for i in range(4)
        ]
        horizontal_dimension = randint(1,2)
        vertical_dimension = randint(1,2)
        grid.center = \
            StateCenterSite(
                physical_dimension=grid.O_center.physical_dimension,
                rightward_dimension=horizontal_dimension,
                upward_dimension=vertical_dimension,
                leftward_dimension=horizontal_dimension,
                downward_dimension=vertical_dimension,
                randomize=True,
            )
        grid.sides = [
            StateSideSite(
                clockwise_dimension = randint(1,2),
                counterclockwise_dimension = randint(1,2),
                inward_dimension = grid.center.bandwidthDimension(i),
                physical_dimension = grid.O_sides[i].physical_dimension,
                randomize = True
            )
            for i in range(4)
        ]
        grid.corners = [
            StateCornerSite(
                clockwise_dimension = grid.sides[i].counterclockwise_dimension,
                counterclockwise_dimension = grid.sides[CCW(i)].clockwise_dimension,
                physical_dimension = grid.O_corners[i].physical_dimension,
                randomize = True
            )
            for i in range(4)
        ]
        return grid
    #@+node:gcross.20111107123047.1382: *5* test_compressConnectionBetweenSideAndClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_O_connection_dimension = grid.O_sides[direction].clockwise_dimension
        old_connection_dimension = grid.sides[direction].clockwise_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=len)
        self.assertLessEqual(grid.O_sides[direction].clockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(grid.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107123047.1384: *5* test_compressConnectionBetweenSideAndClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(grid.O_sides[direction].data.shape,OperatorSideSite.clockwise_index)),
                    product(withoutIndex(grid.O_corners[CW(direction)].data.shape,OperatorCornerSite.counterclockwise_index)),
                    product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.clockwise_index)),
                    product(withoutIndex(grid.corners[CW(direction)].data.shape,StateCornerSite.counterclockwise_index)),
                ),
            )
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.O_sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,grid.O_corners[CW(direction)].counterclockwise_dimension)
        self.assertEqual(number_to_keep,grid.sides[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,grid.corners[CW(direction)].counterclockwise_dimension)
        grid.computeNormalization()
        grid.computeExpectation()
    #@+node:gcross.20111107123047.1388: *5* test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_O_connection_dimension = grid.O_sides[direction].clockwise_dimension
        old_connection_dimension = grid.sides[direction].clockwise_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressConnectionBetweenSideAndClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(grid.O_sides[direction].clockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(grid.sides[direction].clockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107123047.1390: *5* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_O_connection_dimension = grid.O_sides[direction].counterclockwise_dimension
        old_connection_dimension = grid.sides[direction].counterclockwise_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=len)
        self.assertLessEqual(grid.O_sides[direction].counterclockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(grid.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107123047.1394: *5* test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        number_to_keep = \
            randint(1,
                min(
                    product(withoutIndex(grid.O_sides[direction].data.shape,OperatorSideSite.counterclockwise_index)),
                    product(withoutIndex(grid.O_corners[direction].data.shape,OperatorCornerSite.clockwise_index)),
                    product(withoutIndex(grid.sides[direction].data.shape,StateSideSite.counterclockwise_index)),
                    product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.clockwise_index)),
                )
            )
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,keep=number_to_keep)
        self.assertEqual(number_to_keep,grid.O_sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,grid.O_corners[direction].clockwise_dimension)
        self.assertEqual(number_to_keep,grid.sides[direction].counterclockwise_dimension)
        self.assertEqual(number_to_keep,grid.corners[direction].clockwise_dimension)
        grid.computeNormalization()
        grid.computeExpectation()
    #@+node:gcross.20111107123047.1392: *5* test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressConnectionBetweenSideAndCounterClockwiseCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_O_connection_dimension = grid.O_sides[direction].counterclockwise_dimension
        old_connection_dimension = grid.sides[direction].counterclockwise_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressConnectionBetweenSideAndCounterClockwiseCorner(direction,threshold=0)
        self.assertLessEqual(grid.O_sides[direction].counterclockwise_dimension,old_O_connection_dimension)
        self.assertLessEqual(grid.sides[direction].counterclockwise_dimension,old_connection_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107154810.1388: *5* test_compressCorner_keep_all
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_all(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_physical_dimension = grid.corners[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressCorner(direction,keep=len)
        self.assertLessEqual(grid.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111107154810.1390: *5* test_compressCorner_keep_some
    @with_checker(number_of_calls=10)
    def test_compressCorner_keep_some(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        number_to_keep = randint(1,product(withoutIndex(grid.corners[direction].data.shape,StateCornerSite.physical_index)))
        grid.compressCorner(direction,keep=lambda x: min(number_to_keep,len(x)))
        self.assertLessEqual(grid.corners[direction].physical_dimension,number_to_keep)
    #@+node:gcross.20111107154810.1392: *5* test_compressCorner_threshold_zero
    @with_checker(number_of_calls=10)
    def test_compressCorner_threshold_zero(self,
        direction = irange(0,3),
    ):
        grid = self.randomExpectationGrid()
        old_physical_dimension = grid.corners[direction].physical_dimension
        old_normalization = grid.computeNormalization()
        old_expectation = grid.computeExpectation()
        grid.compressCorner(direction,threshold=0)
        self.assertLessEqual(grid.corners[direction].physical_dimension,old_physical_dimension)
        self.assertAlmostEqual(old_normalization/grid.computeNormalization(),1)
        self.assertAlmostEqual(old_expectation/grid.computeExpectation(),1)
    #@+node:gcross.20111103170337.1403: *5* test_computeExpectation_random
    @with_checker(number_of_calls=100)
    def test_computeExpectation_random(self):
        grid = self.randomExpectationGrid()
        self.assertAlmostEqual(
            self.test_computeExpectation_random.contract(*(
                [grid.center.data,grid.O_center.data,grid.center.data.conj()]+
                [o.formExpectationBoundary(s).data for (s,o) in zip(grid.sides + grid.corners,grid.O_sides + grid.O_corners)]
            ))/grid.computeNormalization(),
            grid.computeExpectation(),
        )

    test_computeExpectation_random.contract = \
        formContractor(
            (['S','O','S*']
            +['S{}'.format(i) for i in range(4)]
            +['C{}'.format(i) for i in range(4)]
            ),
            ([(('S{}'.format(i),ExpectationSideBoundary.counterclockwise_index)
              ,('C{}'.format(i),CornerBoundary.clockwise_index)
              ) for i in range(4)
             ]
            +[(('C{}'.format(i),CornerBoundary.counterclockwise_index)
              ,('S{}'.format(CCW(i)),ExpectationSideBoundary.clockwise_index)
              ) for i in range(4)
             ]
            +[(('O',OperatorCenterSite.bandwidthIndex(i))
              ,('S{}'.format(i),ExpectationSideBoundary.inward_operator_index)
              ) for i in range(4)
             ]
            +[(('S',StateCenterSite.bandwidthIndex(i))
              ,('S{}'.format(i),ExpectationSideBoundary.inward_state_index)
              ) for i in range(4)
             ]
            +[(('S*',StateCenterSite.bandwidthIndex(i))
              ,('S{}'.format(i),ExpectationSideBoundary.inward_state_conjugate_index)
              ) for i in range(4)
             ]
            +[(('O',OperatorCenterSite.physical_index)
              ,('S',StateCenterSite.physical_index)
              )
             ,(('O',OperatorCenterSite.physical_conjugate_index)
              ,('S*',StateCenterSite.physical_index)
              )
             ]
            ),
            []
        )
    #@+node:gcross.20111103170337.1401: *5* test_computeExpectation_trivial
    def test_computeNormalization_trivial(self):
        self.assertAlmostEqual(self.trivialExpectationGrid().computeExpectation(),1)
    #@+node:gcross.20111103170337.1399: *5* test_computeExpectationMatrix_random
    @with_checker(number_of_calls=100)
    def test_computeExpectationMatrix_random(self):
        grid = self.randomExpectationGrid()
        self.assertAllClose(
            self.test_computeExpectationMatrix_random.contract(*(
                [grid.O_center.data]+
                [o.formExpectationBoundary(s).data for (s,o) in zip(grid.sides + grid.corners,grid.O_sides + grid.O_corners)]
            )),
            grid.computeExpectationMatrix(),
        )

    test_computeExpectationMatrix_random.contract = \
        formContractor(
            (['O']
            +['S{}'.format(i) for i in range(4)]
            +['C{}'.format(i) for i in range(4)]
            ),
            ([(('S{}'.format(i),ExpectationSideBoundary.counterclockwise_index)
              ,('C{}'.format(i),CornerBoundary.clockwise_index)
              ) for i in range(4)
             ]
            +[(('C{}'.format(i),CornerBoundary.counterclockwise_index)
              ,('S{}'.format(CCW(i)),ExpectationSideBoundary.clockwise_index)
              ) for i in range(4)
             ]
            +[(('O',OperatorCenterSite.bandwidthIndex(i))
              ,('S{}'.format(i),ExpectationSideBoundary.inward_operator_index)
              ) for i in range(4)
             ]
            ),
            [
                [('O',OperatorCenterSite.physical_index)] + [('S{}'.format(i),ExpectationSideBoundary.inward_state_index) for i in range(4)],
                [('O',OperatorCenterSite.physical_conjugate_index)] + [('S{}'.format(i),ExpectationSideBoundary.inward_state_conjugate_index) for i in range(4)],
            ]
        )
    #@+node:gcross.20111103170337.1396: *5* test_computeExpectationMatrix_trivial
    def test_computeExpectationMatrix_trivial(self):
        self.assertAllClose(self.trivialExpectationGrid().computeExpectationMatrix(),identity(1))
    #@+node:gcross.20111103170337.1394: *5* test_contract
    @with_checker(number_of_calls=100)
    def test_contract(self,direction=irange(0,3)):
        grid = self.randomExpectationGrid()
        sides = copy(grid.sides)
        corners = copy(grid.corners)
        center = grid.center
        O_sides = copy(grid.O_sides)
        O_corners = copy(grid.O_corners)
        O_center = copy(grid.O_center)
        corners[direction] = corners[direction].absorbSideSiteAtCounterClockwise(sides[CCW(direction)])
        corners[CW(direction)] = corners[CW(direction)].absorbSideSiteAtClockwise(sides[CW(direction)])
        sides[direction] = sides[direction].absorbCenterSite(center,direction)
        O_corners[direction] = O_corners[direction].absorbSideSiteAtCounterClockwise(O_sides[CCW(direction)])
        O_corners[CW(direction)] = O_corners[CW(direction)].absorbSideSiteAtClockwise(O_sides[CW(direction)])
        O_sides[direction] = O_sides[direction].absorbCenterSite(O_center,direction)
        grid.contract(direction)
        for correct_side, actual_side in zip(sides,grid.sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(corners,grid.corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
        for correct_side, actual_side in zip(O_sides,grid.O_sides):
            self.assertAllClose(correct_side.data,actual_side.data)
        for correct_corner, actual_corner in zip(O_corners,grid.O_corners):
            self.assertAllClose(correct_corner.data,actual_corner.data)
    #@+node:gcross.20111103170337.1397: *5* trivialExpectationGrid
    @staticmethod
    def trivialExpectationGrid():
        return ExpectationGrid(OperatorCenterSite.trivial(),[OperatorSideSite.trivial()]*4,[OperatorCornerSite.trivial()]*4)
    #@-others
#@-others

if __name__ == "__main__":
    numpy.set_printoptions(linewidth=132)
    unittest.main()
#@-leo
