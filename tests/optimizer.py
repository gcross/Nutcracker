#@+leo-ver=4-thin
#@+node:gcross.20091109182634.1542:@thin optimizer.py
#@@language python
#@@tabwidth -4

import unittest
from paycheck import *

from numpy import *
from numpy.random import rand
from random import randint, choice, random

from paulis import *
from utils import *

import vmps

#@+others
#@+node:gcross.20091109182634.1543:optimize
class optimize(unittest.TestCase):
    #@    @+others
    #@+node:gcross.20091109182634.1546:test_correct_result_for_simple_operator
    def test_correct_result_for_simple_operator(self):
        left_environment = ones((1,1,1),complex128)
        right_environment = ones((1,1,1),complex128)
        sparse_operator_indices = ones((2,1))
        sparse_operator_matrices = diag([1,1,1,-1])
        sparse_operator_matrices = sparse_operator_matrices.reshape(sparse_operator_matrices.shape + (1,))
        result = array(crand(1,1,4),order='Fortran')
        status = vmps.optimizer.optimize(left_environment,sparse_operator_indices,sparse_operator_matrices,right_environment,"SR",0,10000,result)
        self.assertEqual(0,status)
        result /= result[0,0,-1]
        self.assertTrue(allclose(result.ravel(),array([0,0,0,1])))
    #@-node:gcross.20091109182634.1546:test_correct_result_for_simple_operator
    #@-others
#@-node:gcross.20091109182634.1543:optimize
#@-others

tests = [
    optimize,
    ]

if __name__ == "__main__":
    unittest.main()
#@-node:gcross.20091109182634.1542:@thin optimizer.py
#@-leo
