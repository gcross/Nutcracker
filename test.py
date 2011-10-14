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
    #@+node:gcross.20111009135633.2981: *4* assertVanishing
    def assertVanishing(self,v):
        self.assertAlmostEqual(norm(v),0)
    #@-others
#@+node:gcross.20111009135633.2982: ** Tests
#@+others
#@+node:gcross.20111013183808.3918: *3* Functions
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

        indices = list(range(tensor_1_ndim))
        del indices[index_1]
        self.assertAllClose(
            tensordot(new_tensor_1,new_tensor_1.conj(),(indices,indices)),
            identity(tensor_1.shape[index_1])
        )
        self.assertAllClose(
            tensordot(tensor_1,tensor_2,(index_1,index_2)),
            tensordot(new_tensor_1,new_tensor_2,(index_1,index_2))
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
    def randomGrid(suitable_for_normalization=False):
        grid = Grid(1)
        grid.sides = []
        for _ in range(4):
            clockwise_dimension = randint(1,3)
            counterclockwise_dimension = randint(1,3)
            physical_dimension = randint(1,3)
            if suitable_for_normalization:
                inward_dimension = randint(1,clockwise_dimension*counterclockwise_dimension*physical_dimension)
            else:
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
                counterclockwise_dimension = grid.sides[(i+1)%4].clockwise_dimension,
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
            +[(('C{}'.format(i),1),('S{}'.format((i+1)%4),0)) for i in range(4)]
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
            +[(('C{}'.format(i),1),('S{}'.format((i+1)%4),0)) for i in range(4)]
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
    #@+node:gcross.20111013165152.1225: *4* test_increaseSingleDirectionBandwidthDimensionBy
    @with_checker
    def test_increaseSingleDirectionBandwidthDimensionBy(self,
        direction = irange(0,3),
        increment = irange(0,3),
    ):
        grid = self.randomGrid()

        bandwidth_dimensions = list(grid.bandwidthDimensions())
        bandwidth_dimensions[direction] += increment
        old_normalization = grid.computeNormalization()

        grid.increaseSingleDirectionBandwidthDimensionBy(increment,direction)

        self.assertEqual(grid.bandwidthDimensions(),grid.bandwidthDimensions())
        self.assertAlmostEqual(old_normalization,grid.computeNormalization())
    #@+node:gcross.20111014113710.1231: *4* test_normalizeSide
    @with_checker(number_of_calls=10)
    def test_normalizeSide(self,
        direction = irange(0,3),
    ):
        grid = self.randomGrid(True)
        old_normalization = grid.computeNormalization()
        grid.normalizeSide(direction)
        self.assertAlmostEqual(old_normalization,grid.computeNormalization())
    #@-others
#@-others

tests = [
    TestFormContractor,
    TestNormalizeAndDenormalize,
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
