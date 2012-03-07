# Imports {{{

from numpy import allclose, array
from random import randint, shuffle
import unittest

from ..utils import randomComplexSample

# }}}

# Classes {{{

class TestCase(unittest.TestCase):
    def assertAllClose(self,v1,v2): # {{{
        v1 = array(v1)
        v2 = array(v2)
        self.assertEqual(v1.shape,v2.shape)
        self.assertTrue(allclose(v1,v2))
    # }}}

    def assertAllEqual(self,v1,v2): # {{{
        v1 = array(v1)
        v2 = array(v2)
        self.assertEqual(v1.shape,v2.shape)
        self.assertTrue(all(v1 == v2))
    # }}}

    def assertDataAlmostEqual(self,v1,v2,rtol=1e-05,atol=1e-08): # {{{
        self.assertEqual(v1.shape,v2.shape)
        self.assertTrue(v1.allcloseTo(v2,rtol=rtol,atol=atol))
    # }}}

    def assertTensorsAlmostEqual(self,v1,v2,rtol=1e-05,atol=1e-08): # {{{
        self.assertDataAlmostEqual(self,v1.data,v2.data,rtol,atol)
    # }}}

    def assertNormalized(self,tensor,index): # {{{
        self.assertAllClose(
            tensordot(tensor.conj(),tensor,(withoutIndex(range(tensor.ndim),index),)*2),
            identity(tensor.shape[index])
        )
    # }}}

    def assertVanishing(self,v): # {{{
        self.assertAlmostEqual(norm(v),0)
    # }}}

# }}}

# Decorators {{{

class prependDataContractor(object): # {{{
    def __init__(self,*args,**keywords):
        self.contractor = formDataContractor(*args,**keywords)
    def __call__(self,f):
        def new_f(*args,**keywords):
            return f(self.contractor,*args,**keywords)
        return new_f
# }}}

# }}}

# Functions {{{

def randomIndices(shape): # {{{
    return tuple(randint(0,dimension-1) for dimension in shape)
# }}}

def randomPartitioningOf(values,number_of_partitions=None): # {{{
    if not number_of_partitions:
        number_of_partitions = randint(1,len(values))
    partitions = [[] for _ in xrange(number_of_partitions)]
    for value in values:
        partitions[randint(0,number_of_partitions-1)].append(value)
    return partitions
# }}}

def randomPermutation(size): # {{{
    permutation = range(size)
    shuffle(permutation)
    return permutation
# }}}

def randomShape(ndim,maximum=5): # {{{
    return tuple(randint(1,maximum) for _ in range(ndim))
# }}}

# }}}

# Exports {{{
__all__ = [
    # Classes {{{
    "TestCase",
    # }}}
    # Decorators {{{
    "prependDataContractor",
    # }}}
    # Functions {{{
    "randomIndices",
    "randomPartitioningOf",
    "randomPermutation",
    "randomShape",
    # }}}
]
# }}}
