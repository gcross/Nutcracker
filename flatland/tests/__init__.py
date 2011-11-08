#@+leo-ver=5-thin
#@+node:gcross.20111107154810.1403: * @file __init__.py
#@+<< Imports >>
#@+node:gcross.20111107154810.1424: ** << Imports >>
from copy import copy
import numpy
from numpy import all, allclose, array, dot, identity, product, tensordot
from numpy.random import rand
from paycheck import *
from random import randint
import unittest

from ..utils import *
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
#@-others
#@-leo
