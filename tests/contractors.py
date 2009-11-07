#@+leo-ver=4-thin
#@+node:gcross.20091106154604.1983:@thin contractors.py
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
#@+node:gcross.20091106154604.1986:pre_iteration
pre_iteration_correct_contractor = make_contractor_from_implicit_joins([
    [1,2,3],       # left environment
    [32,1,21],     # state site tensor (conjugated)
    [32,42,2,22],  # operator site tensor
    [42,3,23],     # state site tensor
],[
    21,
    22,
    23,
])

class pre_iteration(unittest.TestCase):

    @with_checker
    def test_agreement_with_contractor(self,
        b = irange(2,100),
        c = irange(2,20),
        d = irange(2,4),
    ):
        left_environment = crand(b,b,c)
        sparse_operator_indices = array([(randint(1,c),randint(1,c)) for _ in xrange(randint(2,2*c))]).transpose()
        sparse_operator_matrices = array(array([random()*choice([I,X,Y,Z]) for _ in xrange(sparse_operator_indices.shape[-1])],dtype=complex128).transpose(1,2,0),order='Fortran')
        output_tensor = vmps.contractors.pre_iteration(left_environment,sparse_operator_indices,sparse_operator_matrices)
#@-node:gcross.20091106154604.1986:pre_iteration
#@-others

tests = [
    pre_iteration
    ]

if __name__ == "__main__":
    unittest.main()
#@-node:gcross.20091106154604.1983:@thin contractors.py
#@-leo
