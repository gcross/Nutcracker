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
#@+node:gcross.20091110011014.1558:generate_random_sparse_matrices
def generate_random_sparse_matrices(c,d):
    sparse_operator_indices = array([(randint(1,c),randint(1,c)) for _ in xrange(randint(2,2*c))]).transpose()
    sparse_operator_matrices = crand(d,d,sparse_operator_indices.shape[-1])
    operator_site_tensor = zeros((d,d,c,c),complex128)
    for (index1,index2),matrix in zip(sparse_operator_indices.transpose(),sparse_operator_matrices.transpose(2,0,1)):
        operator_site_tensor[...,index2-1,index1-1] += matrix
    return sparse_operator_indices, sparse_operator_matrices, operator_site_tensor
#@-node:gcross.20091110011014.1558:generate_random_sparse_matrices
#@-node:gcross.20091108152444.1533:Functions
#@+node:gcross.20091108152444.1534:Tests
#@+node:gcross.20091106154604.1986:iteration_stage_1
e = {}

for index, (v1, v2) in enumerate([
    ("L1","O1"),
    ("L2","I2"),
    ("L3","I5"),
    ("O2","I3"),
    ("O3","I1"),
    ("O4","I4"),
]):
    assert v1 not in e
    assert v2 not in e
    e[v1] = index
    e[v2] = index

iteration_stage_1_correct_contractor = make_contractor_from_implicit_joins([
    [e["L"+str(i)] for i in reversed(xrange(1,3+1))],  # left environment
    [e["O"+str(i)] for i in reversed(xrange(1,4+1))],  # operator site tensor
],[e["I"+str(i)] for i in reversed(xrange(1,5+1))])

class iteration_stage_1(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        b = irange(2,20),
        c = irange(2,10),
    ):
        d = 2
        left_environment = crand(b,b,c)
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(c,d)
        actual_output_tensor = vmps.contractors.iteration_stage_1(left_environment,sparse_operator_indices,sparse_operator_matrices)
        correct_output_tensor = iteration_stage_1_correct_contractor(left_environment,operator_site_tensor)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091106154604.1986:iteration_stage_1
#@+node:gcross.20091107163338.1531:iteration_stage_2
e = {}

for index, (v1, v2) in enumerate([
    ("I2","O2"),
    ("I1","O1"),
    ("I3","O3"),
    ("I4","S1"),
    ("I5","S2"),
    ("S3","O4"),
]):
    assert v1 not in e
    assert v2 not in e
    e[v1] = index
    e[v2] = index

iteration_stage_2_correct_contractor = make_contractor_from_implicit_joins([
    [e["I"+str(i)] for i in reversed(xrange(1,5+1))],  # iteration stage 1 tensor
    [e["S"+str(i)] for i in reversed(xrange(1,3+1))],  # state site tensor
],[e["O"+str(i)] for i in reversed(xrange(1,4+1))])    # iteration stage 2 tensor

class iteration_stage_2(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        c = irange(2,10),
    ):
        d = 2
        iteration_stage_1_tensor = crand(bl,d,c,bl,d)
        state_site_tensor = crand(br,bl,d)
        actual_output_tensor = vmps.contractors.iteration_stage_2(iteration_stage_1_tensor,state_site_tensor)
        correct_output_tensor = iteration_stage_2_correct_contractor(iteration_stage_1_tensor,state_site_tensor)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091107163338.1531:iteration_stage_2
#@+node:gcross.20091110011014.1555:iteration_stage_3
e = {}

for index, (v1, v2) in enumerate([
    ("I2","O2"),
    ("I1","O1"),
    ("I3","R1"),
    ("I4","R2"),
    ("R3","O3"),
]):
    assert v1 not in e
    assert v2 not in e
    e[v1] = index
    e[v2] = index

iteration_stage_3_correct_contractor = make_contractor_from_implicit_joins([
    [e["I"+str(i)] for i in reversed(xrange(1,4+1))],  # iteration stage 2 tensor
    [e["R"+str(i)] for i in reversed(xrange(1,3+1))],  # right environment tensor
],[e["O"+str(i)] for i in reversed(xrange(1,3+1))])    # iteration stage 3 tensor

class iteration_stage_3(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        c = irange(2,10),
    ):
        d = 2
        iteration_stage_2_tensor = crand(br,c,br,d)
        right_environment = crand(br,br,c)
        actual_output_tensor = vmps.contractors.iteration_stage_3(iteration_stage_2_tensor,right_environment)
        correct_output_tensor = iteration_stage_3_correct_contractor(iteration_stage_2_tensor,right_environment)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091110011014.1555:iteration_stage_3
#@+node:gcross.20091108152444.1537:combined_iteration
#@<< Correct contractor >>
#@+node:gcross.20091109182634.1547:<< Correct contractor >>
e = {}

for index, (v1, v2) in enumerate([
    ("L1","O1"),
    ("L2","S*2"),
    ("L3","S2"),
    ("R1","O2"),
    ("R2","S3"),
    ("R3","S*3"),
    ("O3","S*1"),
    ("O4","S1"),
]):
    assert v1 not in e
    assert v2 not in e
    e[v1] = index
    e[v2] = index

combined_iteration_correct_contractor = make_contractor_from_implicit_joins([
    [e["L"+str(i)] for i in reversed(xrange(1,3+1))],  # left environment
    [e["O"+str(i)] for i in reversed(xrange(1,4+1))],  # operator site tensor
    [e["S"+str(i)] for i in reversed(xrange(1,3+1))],  # state site tensor
    [e["R"+str(i)] for i in reversed(xrange(1,3+1))],  # right environment
],[e["S*"+str(i)] for i in reversed(xrange(1,3+1))])
#@nonl
#@-node:gcross.20091109182634.1547:<< Correct contractor >>
#@nl


class combined_iteration(unittest.TestCase):
    #@    @+others
    #@+node:gcross.20091109182634.1548:test_correct_contractor
    @with_checker(number_of_calls=10)
    def test_correct_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        c = irange(2,10),
    ):
        d = 2
        left_environment = crand(bl,bl,c)
        state_site_tensor = crand(br,bl,d)
        right_environment = crand(br,br,c)
        operator_site_tensor = crand(d,d,c,c)
        correct_output_tensor = iteration_correct_contractor(pre_iteration_correct_contractor(left_environment,operator_site_tensor),state_site_tensor,right_environment)
        actual_output_tensor = combined_iteration_correct_contractor(left_environment,operator_site_tensor,state_site_tensor,right_environment)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
    #@-node:gcross.20091109182634.1548:test_correct_contractor
    #@+node:gcross.20091109182634.1549:test_agreement_with_contractor
    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        c = irange(2,10),
    ):
        d = 2
        left_environment = crand(bl,bl,c)
        state_site_tensor = crand(br,bl,d)
        right_environment = crand(br,br,c)
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(c,d)
        iteration_tensor = vmps.contractors.pre_iteration(left_environment,sparse_operator_indices,sparse_operator_matrices)
        _, actual_output_tensor = vmps.contractors.iteration(iteration_tensor,state_site_tensor,right_environment)
        correct_output_tensor = combined_iteration_correct_contractor(left_environment,operator_site_tensor,state_site_tensor,right_environment)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
    #@-node:gcross.20091109182634.1549:test_agreement_with_contractor
    #@+node:gcross.20091109182634.1551:test_single_Z_operator
    @with_checker(number_of_calls=10)
    def test_single_Z_operator(self):
        d = 2
        left_environment = ones((1,1,1),complex128)
        right_environment = ones((1,1,1),complex128)
        sparse_operator_indices = ones((2,1))
        sparse_operator_matrices = Z.reshape(2,2,1)
        state_site_tensor = array(crand(1,1,2),order='Fortran')

        iteration_tensor = vmps.contractors.pre_iteration(left_environment,sparse_operator_indices,sparse_operator_matrices)
        _, actual_output_tensor = vmps.contractors.iteration(iteration_tensor,state_site_tensor,right_environment)

        correct_output_tensor = state_site_tensor.copy()
        correct_output_tensor[...,1] *= -1

        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
    #@-node:gcross.20091109182634.1551:test_single_Z_operator
    #@-others
#@-node:gcross.20091108152444.1537:combined_iteration
#@+node:gcross.20091110011014.1557:contract_sos_left
e = {}

for index, (v1, v2) in enumerate([
    ("L2","S*2"),
    ("L1","O1"),
    ("L3","S2"),
    ("O3","S*1"),
    ("O4","S1"),
    ("O2","N1"),
    ("S3","N3"),
    ("S*3","N2"),
]):
    if v1 in e:
        raise ValueError("vertex {0} appears twice".format(v1))
    if v2 in e:
        raise ValueError("vertex {0} appears twice".format(v2))
    e[v1] = index
    e[v2] = index

contract_sos_left_correct_contractor = make_contractor_from_implicit_joins([
    [e["L"+str(i)] for i in reversed(xrange(1,3+1))],
    [e["O"+str(i)] for i in reversed(xrange(1,4+1))],
    [e["S"+str(i)] for i in reversed(xrange(1,3+1))],
    [e["S*"+str(i)] for i in reversed(xrange(1,3+1))],
],[e["N"+str(i)] for i in reversed(xrange(1,3+1))])

class contract_sos_left(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        c = irange(2,10),
    ):
        d = 2
        left_environment = crand(bl,bl,c)
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(c,d)
        state_site_tensor = crand(br,bl,d)
        actual_output_tensor = vmps.contractors.contract_sos_left(
            left_environment,
            sparse_operator_indices, sparse_operator_matrices,
            state_site_tensor
        )
        correct_output_tensor = contract_sos_left_correct_contractor(
            left_environment,
            operator_site_tensor,
            state_site_tensor,
            state_site_tensor.conj(),
        )
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091110011014.1557:contract_sos_left
#@-node:gcross.20091108152444.1534:Tests
#@-others

tests = [
    iteration_stage_1,
    iteration_stage_2,
    iteration_stage_3,
    contract_sos_left,
    ]

if __name__ == "__main__":
    unittest.main()
#@-node:gcross.20091106154604.1983:@thin contractors.py
#@-leo
