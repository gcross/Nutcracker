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
#@+node:gcross.20091108152444.1533:Functions
#@+node:gcross.20091108152444.1535:sparse_to_operator
def sparse_to_operator(c,d,sparse_operator_indices,sparse_operator_matrices):
    operator = zeros((d,d,c,c),complex128)
    for (index1,index2),matrix in zip(sparse_operator_indices.transpose(),sparse_operator_matrices.transpose(2,0,1)):
        operator[...,index1-1,index2-1] += matrix
    return operator
#@-node:gcross.20091108152444.1535:sparse_to_operator
#@-node:gcross.20091108152444.1533:Functions
#@+node:gcross.20091108152444.1534:Tests
#@+node:gcross.20091106154604.1986:pre_iteration
pre_iteration_correct_contractor = make_contractor_from_implicit_joins([
    [1,2,3],       # left environment
    [5,6,3,4],  # operator site tensor
],[1,5,4,2,6])

class pre_iteration(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        b = irange(2,20),
        c = irange(2,10),
    ):
        d = 2
        left_environment = crand(b,b,c)
        sparse_operator_indices = array([(randint(1,c),randint(1,c)) for _ in xrange(randint(2,2*c))]).transpose()
        sparse_operator_matrices = array(array([random()*choice([I,X,Y,Z]) for _ in xrange(sparse_operator_indices.shape[-1])],dtype=complex128).transpose(1,2,0),order='Fortran')
        actual_output_tensor = vmps.contractors.pre_iteration(left_environment,sparse_operator_indices,sparse_operator_matrices)
        operator = sparse_to_operator(c,d,sparse_operator_indices,sparse_operator_matrices)
        correct_output_tensor = pre_iteration_correct_contractor(left_environment,operator)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091106154604.1986:pre_iteration
#@+node:gcross.20091107163338.1531:iteration
iteration_correct_contractor = make_contractor_from_implicit_joins([
    [1,2,3,4,5], # iteration tensor
    [6,1,2],     # state site tensor
    [7,6,3],     # right environment
],[7,4,5])

class iteration(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        c = irange(2,10),
    ):
        d = 2
        iteration_tensor = crand(bl,d,c,bl,d)
        state_site_tensor = crand(br,bl,d)
        right_environment = crand(br,br,c)
        _, actual_output_tensor = vmps.contractors.iteration(iteration_tensor,state_site_tensor,right_environment)
        correct_output_tensor = iteration_correct_contractor(iteration_tensor,state_site_tensor,right_environment)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091107163338.1531:iteration
#@-node:gcross.20091108152444.1534:Tests
#@-others

tests = [
    pre_iteration,
    iteration
    ]

if __name__ == "__main__":
    unittest.main()
#@-node:gcross.20091106154604.1983:@thin contractors.py
#@-leo
