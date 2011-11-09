#@+leo-ver=5-thin
#@+node:gcross.20111107131531.1346: * @file __init__.py
#@+<< Imports >>
#@+node:gcross.20111107131531.1351: ** << Imports >>
from copy import copy
import numpy
from numpy import all, allclose, array, dot, identity, product, tensordot
from numpy.linalg import norm
from numpy.random import rand
from paycheck import *
from random import randint
import unittest

from ..utils import *
#@-<< Imports >>

#@+others
#@+node:gcross.20111108100704.1374: ** Functions
#@+node:gcross.20111028110210.1335: *3* ensurePhysicalDimensionSufficientlyLarge
def ensurePhysicalDimensionSufficientlyLarge(tensor,index,dimension):
    if dimension > product(withoutIndex(tensor.dimensions(),index)):
        new_shape = list(tensor.dimensions())
        new_shape[0] = dimension
        return type(tensor)(crand(*new_shape))
    else:
        return tensor
#@+node:gcross.20111028110210.1337: *3* ensurePhysicalDimensionSufficientlyLargeToNormalize
def ensurePhysicalDimensionSufficientlyLargeToNormalize(tensor,index):
    return ensurePhysicalDimensionSufficientlyLarge(tensor,index,tensor.dimension(index))
#@+node:gcross.20111109104457.1813: *3* prependContractor
class prependContractor(object):
    def __init__(self,*args,**keywords):
        self.contractor = formContractor(*args,**keywords)
    def __call__(self,f):
        def new_f(*args,**keywords):
            return f(self.contractor,*args,**keywords)
        return new_f
#@+node:gcross.20111108100704.1408: *3* randomIndex
def randomIndex(ndim):
    return randint(0,ndim-1)
#@+node:gcross.20111108100704.1415: *3* randomNormalizableTensorAndIndex
def randomNormalizableTensorAndIndex(ndim):
    index = randomIndex(ndim)
    shape = randomShape(ndim)
    shape[index] = randint(1,product(withoutIndex(shape,index)))
    return crand(*shape), index
#@+node:gcross.20111108100704.1409: *3* randomShape
def randomShape(ndim):
    return [randint(1,5) for _ in range(ndim)]
#@+node:gcross.20111108100704.1410: *3* randomShapeAgreeingWith
def randomShapeAgreeingWith(ndim,index,other_dimension):
    shape = randomShape(ndim)
    shape[index] = other_dimension
    return shape
#@+node:gcross.20111108100704.1411: *3* randomTensor
def randomTensor(ndim):
    return crand(*randomShape(ndim))
#@+node:gcross.20111108100704.1412: *3* randomTensorAgreeingWith
def randomTensorAgreeingWith(ndim,index,other_dimension):
    return crand(*randomShapeAgreeingWith(ndim,index,other_dimension))
#@+node:gcross.20111108100704.1413: *3* randomTensorAndIndex
def randomTensorAndIndex(ndim):
    return randomTensor(ndim), randomIndex(ndim)
#@+node:gcross.20111108100704.1414: *3* randomTensorAndIndexAgreeingWith
def randomTensorAndIndexAgreeingWith(ndim,other_dimension):
    index = randomIndex(ndim)
    return randomTensorAgreeingWith(ndim,index,other_dimension), index
#@+node:gcross.20111107123726.3152: ** Classes
#@+node:gcross.20111107123726.3153: *3* TestCase
class TestCase(unittest.TestCase):
    #@+others
    #@+node:gcross.20111107123726.3154: *4* assertAllClose
    def assertAllClose(self,v1,v2):
        v1 = array(v1)
        v2 = array(v2)
        self.assertEqual(v1.shape,v2.shape)
        self.assertTrue(allclose(v1,v2))
    #@+node:gcross.20111107123726.3155: *4* assertAllEqual
    def assertAllEqual(self,v1,v2):
        v1 = array(v1)
        v2 = array(v2)
        self.assertEqual(v1.shape,v2.shape)
        self.assertTrue(all(v1 == v2))
    #@+node:gcross.20111108100704.1373: *4* assertNormalized
    def assertNormalized(self,tensor,index):
        self.assertAllClose(
            tensordot(tensor.conj(),tensor,(withoutIndex(range(tensor.ndim),index),)*2),
            identity(tensor.shape[index])
        )
    #@+node:gcross.20111107123726.3156: *4* assertVanishing
    def assertVanishing(self,v):
        self.assertAlmostEqual(norm(v),0)
    #@-others
#@-others
#@-leo
