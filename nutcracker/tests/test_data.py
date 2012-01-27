# Imports {{{
from itertools import izip
from numpy import zeros
from paycheck import *

from . import *
from ..data import Data
# }}} Imports 

# Tests {{{
class test_Data_toArray(TestCase): # {{{
    @with_checker
    def test_no_components(self,ndim=irange(1,5)): # {{{
        sparse_shape = tuple(randint(1,3) for _ in xrange(ndim))
        dense_shape = tuple(randint(1,3) for _ in xrange(ndim))
        shape = tuple(x*y for x,y in izip(sparse_shape,dense_shape))
        data = Data(
            index_table = zeros(shape=(0,ndim)),
            matrix_table = zeros(shape=(0,) + dense_shape),
            sparse_shape = sparse_shape
        )
        self.assertAllEqual(zeros(shape),data.toArray())
    # }}}
    @with_checker
    def test_trivial_sparse(self,ndim=irange(1,5),entry=float): # {{{
        sparse_shape = tuple(randint(1,3) for _ in xrange(ndim))
        indices = tuple(randint(0,n-1) for n in sparse_shape)
        data = Data(
            index_table = array(indices, shape=(1,ndim)),
            matrix_table = array(entry, dtype=float).reshape((1,)*(ndim+1)),
            sparse_shape = sparse_shape
        )
        correct_array = zeros(sparse_shape)
        correct_array[indices] = entry
        self.assertAllEqual(correct_array,data.toArray())
    # }}}
# }}}
# }}} Tests
