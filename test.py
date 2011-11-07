#@+leo-ver=5-thin
#@+node:gcross.20111107123726.3138: * @file test.py
#@+<< Import needed modules >>
#@+node:gcross.20111107123726.3139: ** << Import needed modules >>
import unittest
from paycheck import *
from numpy import array, zeros, all, double, tensordot, multiply, complex128, allclose, ones, diag, identity, dot, argmin, rank, set_printoptions
from numpy.linalg import norm
from numpy.random import rand
from scipy.linalg import qr, eigh
from random import randint, choice
import random
import __builtin__
import nutcracker.core as core
#@-<< Import needed modules >>

#@+others
#@+node:gcross.20111107123726.3140: ** Functions
#@+node:gcross.20111107123726.3141: *3* Macro building functions
#@+at
# These routines build macros to perform tensor contractions.  They do this
# by construction a string of Python code which performs the contraction,
# and then compiling the code into a function.
#@@c

#@+others
#@+node:gcross.20111107123726.3142: *4* n2l
#@+at
# Utility function converting numbers to letters.
#@@c
n2l = map(chr,range(ord('A'),ord('Z')+1))
#@+node:gcross.20111107123726.3143: *4* make_contractor
def make_contractor(tensor_index_labels,index_join_pairs,result_index_labels,name="f"):    # pre-process parameters
    tensor_index_labels = list(map(list,tensor_index_labels))
    index_join_pairs = list(index_join_pairs)
    result_index_labels = list([list(index_group) if hasattr(index_group,"__getitem__") else [index_group] for index_group in result_index_labels])

    assert sum(len(index_group) for index_group in tensor_index_labels) == (sum(len(index_group) for index_group in result_index_labels)+2*len(index_join_pairs))

    function_definition_statements = ["def %s(%s):" % (name,",".join(n2l[:len(tensor_index_labels)]))]

    #@+<< def build_statements >>
    #@+node:gcross.20111107123726.3144: *5* << def build_statements >>
    def build_statements(tensor_index_labels,index_join_pairs,result_index_labels):
    #@+at
    # This routine recursively builds a list of statements which performs the full tensor contraction.
    # 
    # First, if there is only one tensor left, then transpose and reshape it to match the result_index_labels.
    #@@c
        if len(tensor_index_labels) == 1:
            if len(result_index_labels) == 0:
                return ["return A"]
            else:
                final_index_labels = tensor_index_labels[0]
                result_indices = [[final_index_labels.index(index) for index in index_group] for index_group in result_index_labels]
                transposed_indices = __builtin__.sum(result_indices,[])
                assert type(transposed_indices) == list
                assert len(final_index_labels) == len(transposed_indices)
                new_shape = ",".join(["(%s)" % "*".join(["shape[%i]"%index for index in index_group]) for index_group in result_indices])     
                return ["shape=A.shape","return A.transpose(%s).reshape(%s)" % (transposed_indices,new_shape)]
    #@+at
    # Second, if all joins have finished, then take outer products to combine all remaining tensors into one.
    #@@c
        elif len(index_join_pairs) == 0:
            if tensor_index_labels[-1] is None:
                return build_statements(tensor_index_labels[:-1],index_join_pairs,result_index_labels)
            elif len(tensor_index_labels[-1]) == 0:
                v = n2l[len(tensor_index_labels)-1]
                return ["A*=%s" % v, "del %s" % v] + build_statements(tensor_index_labels[:-1],index_join_pairs,result_index_labels)
            else:
                v = n2l[len(tensor_index_labels)-1]
                tensor_index_labels[0] += tensor_index_labels[-1]
                return ["A = multiply.outer(A,%s)" % v, "del %s" % v] + build_statements(tensor_index_labels[:-1],index_join_pairs,result_index_labels)
    #@+at
    # Otherwise, do the first join, walking through index_join_pairs to find any other pairs which connect the same two tensors.
    #@@c
        else:
            #@+<< Search for all joins between these tensors >>
            #@+node:gcross.20111107123726.3145: *6* << Search for all joins between these tensors >>
            #@+at
            # This function searches for the tensors which are joined, and reorders the indices in the join so that the index corresponding to the tensor appearing first in the list of tensors appears first in the join.
            #@@c
            def find_tensor_ids(join):
                reordered_join = [None,None]
                tensor_ids = [0,0]
                join = list(join)
                while tensor_ids[0] < len(tensor_index_labels):
                    index_labels = tensor_index_labels[tensor_ids[0]]
                    if index_labels is None:
                        tensor_ids[0] += 1
                    elif join[0] in index_labels:
                        reordered_join[0] = index_labels.index(join[0])
                        del join[0]
                        break
                    elif join[1] in index_labels:
                        reordered_join[0] = index_labels.index(join[1])
                        del join[1]
                        break
                    else:
                        tensor_ids[0] += 1
                assert len(join) == 1 # otherwise index was not found in any tensor
                tensor_ids[1] = tensor_ids[0] + 1
                while tensor_ids[1] < len(tensor_index_labels):
                    index_labels = tensor_index_labels[tensor_ids[1]]
                    if index_labels is None:
                        tensor_ids[1] += 1
                    elif join[0] in index_labels:
                        reordered_join[reordered_join.index(None)] = index_labels.index(join[0])
                        del join[0]
                        break
                    else:
                        tensor_ids[1] += 1
                assert len(join) == 0 # otherwise index was not found in any tensor
                return tensor_ids, reordered_join

            join_indices = [0]
            tensor_ids,reordered_join = find_tensor_ids(index_join_pairs[0])

            indices = [[],[]]

            for j in xrange(2):
                indices[j].append(reordered_join[j])

            # Search for other joins between these tensors
            for i in xrange(1,len(index_join_pairs)):
                tensor_ids_,reordered_join = find_tensor_ids(index_join_pairs[i])
                if tensor_ids == tensor_ids_:
                    join_indices.append(i)
                    for j in xrange(2):
                        indices[j].append(reordered_join[j])

            #@-<< Search for all joins between these tensors >>

            #@+<< Build tensor contraction statements >>
            #@+node:gcross.20111107123726.3146: *6* << Build tensor contraction statements >>
            tensor_vars = [n2l[id] for id in tensor_ids]

            statements = [
                "try:",
                "   %s = tensordot(%s,%s,%s)" % (tensor_vars[0],tensor_vars[0],tensor_vars[1],indices),
                "   del %s" % tensor_vars[1],
                "except ValueError:",
                "   raise ValueError('indices %%s do not match for tensor %%i, shape %%s, and tensor %%i, shape %%s.' %% (%s,%i,%s.shape,%i,%s.shape))" % (indices,tensor_ids[0],tensor_vars[0],tensor_ids[1],tensor_vars[1])
            ]
            #@-<< Build tensor contraction statements >>

            #@+<< Delete joins from list and update tensor specifications >>
            #@+node:gcross.20111107123726.3147: *6* << Delete joins from list and update tensor specifications >>
            join_indices.reverse()
            for join_index in join_indices:
                del index_join_pairs[join_index]

            new_tensor_index_labels_0 = list(tensor_index_labels[tensor_ids[0]])
            indices[0].sort(reverse=True)
            for index in indices[0]:
                del new_tensor_index_labels_0[index]

            new_tensor_index_labels_1 = list(tensor_index_labels[tensor_ids[1]])
            indices[1].sort(reverse=True)
            for index in indices[1]:
                del new_tensor_index_labels_1[index]

            tensor_index_labels[tensor_ids[0]] = new_tensor_index_labels_0+new_tensor_index_labels_1
            tensor_index_labels[tensor_ids[1]] = None
            #@-<< Delete joins from list and update tensor specifications >>

            return statements + build_statements(tensor_index_labels,index_join_pairs,result_index_labels)
    #@-<< def build_statements >>

    function_definition_statements += ["\t" + statement for statement in build_statements(tensor_index_labels,index_join_pairs,result_index_labels)]

    function_definition = "\n".join(function_definition_statements)+"\n"

    f_globals = {"tensordot":tensordot,"multiply":multiply}
    f_locals = {}

    exec function_definition in f_globals, f_locals

    f = f_locals[name]
    f.source = function_definition
    return f
#@+node:gcross.20111107123726.3148: *4* make_contractor_from_implicit_joins
def make_contractor_from_implicit_joins(tensor_index_labels,result_index_labels,name="f"):
    tensor_index_labels = list(map(list,tensor_index_labels))
    found_indices = {}
    index_join_pairs = []
    for i in xrange(len(tensor_index_labels)):
        for index_position, index in enumerate(tensor_index_labels[i]):
            if index in found_indices:
                other_tensor = found_indices[index]
                if other_tensor is None:
                    raise ValueError("index label %s found in more than two tensors" % index)
                else:
                    # rename this instance of the index and add to the list of join pairs
                    tensor_index_labels[i][index_position] = (i,index)
                    index_join_pairs.append((index,(i,index)))
                    # mark that we have found two instances of this index for
                    # error-checking purposes
                    found_indices[index] = None
            else:
                found_indices[index] = i
    return make_contractor(tensor_index_labels,index_join_pairs,result_index_labels,name)
#@-others
#@+node:gcross.20111107123726.3149: *3* crand
def crand(*shape):
    return rand(*shape)*2-1+rand(*shape)*2j-1j
#@+node:gcross.20111107123726.3150: *3* generate_random_sparse_matrices
def generate_random_sparse_matrices(cl,cr,d,symmetric=False):
    sparse_operator_indices = array([(randint(1,cl),randint(1,cr)) for _ in xrange(randint(2,cl+cr))]).transpose()
    sparse_operator_matrices = crand(d,d,sparse_operator_indices.shape[-1])
    if symmetric:
        sparse_operator_matrices += sparse_operator_matrices.transpose(1,0,2).conj()
    operator_site_tensor = zeros((d,d,cr,cl),complex128)
    for (index1,index2),matrix in zip(sparse_operator_indices.transpose(),sparse_operator_matrices.transpose(2,0,1)):
        operator_site_tensor[...,index2-1,index1-1] += matrix
    return sparse_operator_indices, sparse_operator_matrices, operator_site_tensor
#@+node:gcross.20111107123726.3151: *3* form_contractor
def form_contractor(edges,input_tensors,output_tensor):
    e = {}

    for index, (v1, v2) in enumerate(edges):
        if v1 in e:
            raise ValueError("vertex {0} appears twice".format(v1))
        if v2 in e:
            raise ValueError("vertex {0} appears twice".format(v2))
        e[v1] = index
        e[v2] = index

    output_name, output_size = output_tensor
    return make_contractor_from_implicit_joins(
        [[e[input_name+str(index)] for index in xrange(1,input_size+1)] for (input_name,input_size) in input_tensors],
        [e[output_name+str(index)] for index in xrange(1,output_size+1)],
    )
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
    #@+node:gcross.20111107123726.3156: *4* assertVanishing
    def assertVanishing(self,v):
        self.assertAlmostEqual(norm(v),0)
    #@-others
#@+node:gcross.20111107123726.3157: ** Tests
#@+node:gcross.20111107123726.3158: *3* Contractors
#@+others
#@+node:gcross.20111107123726.3174: *4* compute_expectation
compute_expectation_correct_contractor = form_contractor([
    ("L2","S*2"),
    ("L3","O4"),
    ("L1","S2"),
    ("R1","S*1"),
    ("R3","O3"),
    ("R2","S1"),
    ("O2","S*3"),
    ("O1","S3"),
], [
    ("L",3),
    ("S",3),
    ("O",4),
    ("S*",3),
    ("R",3),
], (None,0)
)

class compute_expectation(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        cl = irange(2,10),
        cr = irange(2,10),
    ):
        d = 2
        left_environment = crand(bl,bl,cl)
        right_environment = crand(br,br,cr)
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(cl,cr,d)
        state_site_tensor = crand(br,bl,d)
        actual_output_tensor = core.compute_expectation(
            left_environment,
            state_site_tensor,
            sparse_operator_indices, sparse_operator_matrices,
            right_environment,
        )
        correct_output_tensor = compute_expectation_correct_contractor(
            left_environment,
            state_site_tensor,
            operator_site_tensor,
            state_site_tensor.conj(),
            right_environment,
        )
        self.assertAlmostEqual(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3175: *4* compute_optimization_matrix
class compute_optimization_matrix(TestCase):

    correct_contractor = staticmethod(form_contractor([
        ("L2","M2"),
        ("L3","O4"),
        ("L1","M5"),
        ("R1","M1"),
        ("R3","O3"),
        ("R2","M4"),
        ("O2","M3"),
        ("O1","M6"),
    ], [
        ("L",3),
        ("O",4),
        ("R",3),
    ], ("M",6)
    ))

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        d = irange(2,4),
        bl = irange(2,20),
        br = irange(2,20),
        cl = irange(2,10),
        cr = irange(2,10),
    ):
        left_environment = crand(bl,bl,cl)
        right_environment = crand(br,br,cr)
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(cl,cr,d)
        actual_output_tensor = core.compute_optimization_matrix(
            left_environment,
            sparse_operator_indices, sparse_operator_matrices,
            right_environment,
        )
        correct_output_tensor = optimization_matrix_contractor(
            left_environment,
            operator_site_tensor,
            right_environment,
        )
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3177: *4* contract_matrix_left
class contract_matrix_left(TestCase):

    correct_contractor = staticmethod(form_contractor([
        ("L1","M2"),
        ("L2","M*2"),
        ("L'1","M1"),
        ("L'2","M*1"),
    ], [
        ("L",2),
        ("M",2),
        ("M*",2),
    ], ("L'",2)
    ))

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
    ):
        left_environment = crand(bl,bl)
        matrix = crand(br,bl)
        actual_output_tensor = core.contract_matrix_left(
            left_environment,
            matrix,
        )
        correct_output_tensor = self.correct_contractor(
            left_environment,
            matrix,
            matrix.conj(),
        )
        self.assertEqual(actual_output_tensor.shape,correct_output_tensor.shape)
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3163: *4* contract_sos
#@+node:gcross.20111107123726.3164: *5* contract_sos_left
contract_sos_left_correct_contractor = form_contractor([
    ("L2","S*2"),
    ("L3","O4"),
    ("L1","S2"),
    ("O1","S3"),
    ("O2","S*3"),
    ("O3","N3"),
    ("S1","N1"),
    ("S*1","N2"),
], [
    ("L",3),
    ("O",4),
    ("S",3),
    ("S*",3),
], ("N",3)
)

class contract_sos_left(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        cl = irange(2,10),
        cr = irange(2,10),
    ):
        d = 2
        left_environment = crand(bl,bl,cl)
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(cl,cr,d)
        state_site_tensor = crand(br,bl,d)
        actual_output_tensor = core.contract_sos_left(
            cr,
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
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3165: *5* contract_sos_right_stage_1
contract_sos_right_stage_1_correct_contractor = form_contractor([
    ("R1","S*1"),
    ("R3","O4"),
    ("R2","O3"),
    ("S*3","O2"),
    ("S*2","O1"),
], [
    ("R",3),
    ("S*",3),
], ("O",4)
)

class contract_sos_right_stage_1(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        cr = irange(2,10),
    ):
        d = 2
        right_environment = crand(br,br,cr)
        state_site_tensor = crand(br,bl,d)
        actual_output_tensor = core.contract_sos_right_stage_1(
            right_environment,
            state_site_tensor
        )
        correct_output_tensor = contract_sos_right_stage_1_correct_contractor(
            right_environment,
            state_site_tensor.conj(),
        )
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3166: *5* contract_sos_right_stage_2a
contract_sos_right_stage_2a_correct_contractor = form_contractor([
    ("O2","I2"),
    ("O1","S3"),
    ("S2","I1"),
    ("S1","I3"),
], [
    ("O",2),
    ("S",3),
], ("I",3)
)

class contract_sos_right_stage_2a(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        c = irange(2,10),
    ):
        d = 2
        state_site_tensor = crand(br,bl,d)
        matrix = crand(d,d)
        actual_output_tensor = core.contract_sos_right_stage_2a(
            matrix,
            state_site_tensor,
        )
        correct_output_tensor = contract_sos_right_stage_2a_correct_contractor(
            matrix,
            state_site_tensor
        )
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3167: *5* contract_sos_right_stage_2b
contract_sos_right_stage_2b_correct_contractor = form_contractor([
    ("A3","B3"),
    ("A2","B2"),
    ("A1","C1"),
    ("B1","C2"),
], [
    ("A",3),
    ("B",3),
], ("C",2)
)

class contract_sos_right_stage_2b(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
    ):
        d = 2
        A = crand(bl,d,br)
        B = crand(bl,d,br)
        actual_output_tensor = zeros((bl,bl),complex128,order='Fortran')
        core.contract_sos_right_stage_2b(A,B,actual_output_tensor)
        correct_output_tensor = contract_sos_right_stage_2b_correct_contractor(A,B)
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3168: *5* contract_sos_right_stage_2
contract_sos_right_stage_2_correct_contractor = form_contractor([
    ("I1","R1"),
    ("I2","O2"),
    ("I3","S1"),
    ("I4","O3"),
    ("S3","O1"),
    ("O4","R3"),
    ("S2","R2"),
], [
    ("I",4),
    ("O",4),
    ("S",3),
], ("R",3)
)

class contract_sos_right_stage_2(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        cl = irange(2,10),
        cr = irange(2,10),
    ):
        d = 2
        sos_right_stage_1_tensor = crand(bl,d,br,cr)
        state_site_tensor = crand(br,bl,d)
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(cl,cr,d)
        actual_output_tensor = core.contract_sos_right_stage_2(
            cl,
            sos_right_stage_1_tensor,
            sparse_operator_indices, sparse_operator_matrices,
            state_site_tensor
        )
        correct_output_tensor = contract_sos_right_stage_2_correct_contractor(
            sos_right_stage_1_tensor,
            operator_site_tensor,
            state_site_tensor
        )
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3169: *5* contract_sos_right
contract_sos_right_correct_contractor = form_contractor([
    ("R1","S*1"),
    ("R3","O3"),
    ("R2","S1"),
    ("O2","S*3"),
    ("O1","S3"),
    ("O4","N3"),
    ("S2","N2"),
    ("S*2","N1"),
], [
    ("R",3),
    ("O",4),
    ("S",3),
    ("S*",3),
], ("N",3)
)

class contract_sos_right(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        cl = irange(2,10),
        cr = irange(2,10),
    ):
        d = 2
        right_environment = crand(br,br,cr)
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(cl,cr,d)
        state_site_tensor = crand(br,bl,d)
        actual_output_tensor = core.contract_sos_right(
            cl,
            right_environment,
            sparse_operator_indices, sparse_operator_matrices,
            state_site_tensor
        )
        correct_output_tensor = contract_sos_right_correct_contractor(
            right_environment,
            operator_site_tensor,
            state_site_tensor,
            state_site_tensor.conj(),
        )
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3170: *4* contract_ss
#@+node:gcross.20111107123726.3171: *5* contract_vs_left
contract_vs_left_correct_contractor = form_contractor([
    ("L2","O1"),
    ("L1","N2"),
    ("O2","N3"),
    ("O3","L'2"),
    ("N1","L'1"),
], [
    ("L",2),
    ("O",3),
    ("N",3),
], ("L'",2)
)

class contract_vs_left(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        nsl = irange(2,20),
        nsr = irange(2,20),
        osl = irange(2,20),
        osr = irange(2,20),
        d = irange(2,4),
    ):
        left_environment = crand(nsl,osl)
        new_state_site_tensor = crand(nsr,nsl,d)
        old_state_site_tensor = crand(osl,d,osr)
        actual_output_tensor = core.contract_vs_left(
            left_environment,
            old_state_site_tensor,
            new_state_site_tensor,
        )
        correct_output_tensor = contract_vs_left_correct_contractor(
            left_environment,
            old_state_site_tensor,
            new_state_site_tensor,
        )
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3172: *5* contract_vs_right
contract_vs_right_correct_contractor = form_contractor([
    ("R1","O3"),
    ("R2","N1"),
    ("O2","N3"),
    ("O1","R'1"),
    ("N2","R'2"),
], [
    ("R",2),
    ("O",3),
    ("N",3),
], ("R'",2)
)

class contract_vs_right(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        nsl = irange(2,20),
        nsr = irange(2,20),
        osl = irange(2,20),
        osr = irange(2,20),
        d = irange(2,4),
    ):
        right_environment = crand(osr,nsr)
        new_state_site_tensor = crand(nsr,nsl,d)
        old_state_site_tensor = crand(osl,d,osr)
        actual_output_tensor = core.contract_vs_right(
            right_environment,
            old_state_site_tensor,
            new_state_site_tensor,
        )
        correct_output_tensor = contract_vs_right_correct_contractor(
            right_environment,
            old_state_site_tensor,
            new_state_site_tensor,
        )
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3173: *5* form_overlap_vector
form_overlap_vector_correct_contractor = form_contractor([
    ("R1","O3"),
    ("R2","N1"),
    ("O2","N3"),
    ("O1","L2"),
    ("L1","N2"),
], [
    ("L",2),
    ("R",2),
    ("O",3),
], ("N",3)
)

class form_overlap_vector(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        nsl = irange(2,20),
        nsr = irange(2,20),
        osl = irange(2,20),
        osr = irange(2,20),
        d = irange(2,4),
    ):
        left_environment = crand(nsl,osl)
        right_environment = crand(osr,nsr)
        old_state_site_tensor = crand(osl,d,osr)
        actual_output_tensor = core.form_overlap_vector(
            left_environment,
            right_environment,
            old_state_site_tensor,
        )
        correct_output_tensor = form_overlap_vector_correct_contractor(
            left_environment,
            right_environment,
            old_state_site_tensor,
        )
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3176: *4* extend_state_vector_fragment
class extend_state_vector_fragment(TestCase):

    correct_contractor = staticmethod(form_contractor([
        ("F2","N3"),
        ("F1","S2"),
        ("S3","N2"),
        ("S1","N1"),
    ], [
        ("F",2),
        ("S",3),
    ], ("N",3)
    ))

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        dl = irange(2,10),
        dr = irange(2,10),
        bm = irange(2,10),
        br = irange(2,10),
    ):
        fragment = crand(bm,dl)
        state_site_tensor = crand(br,bm,dr)
        actual_output_tensor = core.extend_state_vector_fragment(
            fragment,
            state_site_tensor,
        )
        correct_output_tensor = self.correct_contractor(
            fragment,
            state_site_tensor,
        )
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3159: *4* iteration
#@+node:gcross.20111107123726.3160: *5* iteration_stage_1
iteration_stage_1_correct_contractor = form_contractor([
    ("L3","O4"),
    ("L2","I4"),
    ("L1","I1"),
    ("O3","I3"),
    ("O2","I5"),
    ("O1","I2"),
], [
    ("L",3),
    ("O",4),
], ("I",5)
)

class iteration_stage_1(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        b = irange(2,20),
        cl = irange(2,10),
        cr = irange(2,10),
    ):
        d = 2
        left_environment = crand(b,b,cl)
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(cl,cr,d)
        actual_output_tensor = core.iteration_stage_1(cr,left_environment,sparse_operator_indices,sparse_operator_matrices)
        correct_output_tensor = iteration_stage_1_correct_contractor(left_environment,operator_site_tensor)
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3161: *5* iteration_stage_2
iteration_stage_2_correct_contractor = form_contractor([
    ("I4","O3"),
    ("I5","O4"),
    ("I3","O2"),
    ("I2","S3"),
    ("I1","S2"),
    ("S1","O1"),
], [
    ("I",5),
    ("S",3),
], ("O",4)
)

class iteration_stage_2(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        cr = irange(2,10),
    ):
        d = 2
        iteration_stage_1_tensor = crand(bl,d,cr,bl,d)
        state_site_tensor = crand(br,bl,d)
        actual_output_tensor = core.iteration_stage_2(iteration_stage_1_tensor,state_site_tensor)
        correct_output_tensor = iteration_stage_2_correct_contractor(iteration_stage_1_tensor,state_site_tensor)
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3162: *5* iteration_stage_3
iteration_stage_3_correct_contractor = form_contractor([
    ("I3","O2"),
    ("I4","O3"),
    ("I2","R3"),
    ("I1","R2"),
    ("R1","O1"),
], [
    ("I",4),
    ("R",3),
], ("O",3)
)

class iteration_stage_3(TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        cr = irange(2,10),
    ):
        d = 2
        iteration_stage_2_tensor = crand(br,cr,br,d)
        right_environment = crand(br,br,cr)
        actual_output_tensor = core.iteration_stage_3(iteration_stage_2_tensor,right_environment)
        correct_output_tensor = iteration_stage_3_correct_contractor(iteration_stage_2_tensor,right_environment)
        self.assertAllClose(actual_output_tensor,correct_output_tensor)
#@+node:gcross.20111107123726.3235: *4* optimization_matrix_contractor
optimization_matrix_contractor = form_contractor([
    ("L1","M5"),
    ("L2","M2"),
    ("L3","O4"),
    ("R1","M1"),
    ("R2","M4"),
    ("R3","O3"),
    ("O1","M6"),
    ("O2","M3"),
], [
    ("L",3),
    ("O",4),
    ("R",3),
], ("M",6)
)
#@-others
#@+node:gcross.20111107123726.3188: *3* Randomization
#@+node:gcross.20111107123726.3189: *4* rand_norm_state_site_tensor
class rand_norm_state_site_tensor(TestCase):
    #@+others
    #@-others

    @with_checker
    def testCorrectness(self,bl=irange(2,4),br=irange(2,4)):
        d = 2
        normalized_tensor = core.rand_norm_state_site_tensor(br,bl,d)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((0,2,),)*2)
        self.assertAllClose(identity(bl),should_be_identity)
#@+node:gcross.20111107123726.3190: *4* rand_unnorm_state_site_tensor
class rand_unnorm_state_site_tensor(TestCase):
    #@+others
    #@-others

    @with_checker
    def testCorrectness(self,bl=irange(2,4),br=irange(2,4)):
        d = 2
        unnormalized_tensor = core.rand_unnorm_state_site_tensor(br,bl,d)
        self.assertAlmostEqual(1,norm(unnormalized_tensor.ravel()))
#@+node:gcross.20111107123726.3191: *3* Utility Functions
#@+node:gcross.20111107123726.3192: *4* orthogonalize_matrix_in_place
class orthogonalize_matrix_in_place(TestCase):
    @with_checker
    def test_orthogonality(self,projector_length=irange(8,20),number_of_projectors=irange(1,7)):
        projector_matrix = array(crand(projector_length,number_of_projectors),order='Fortran')
        core.orthogonalize_matrix_in_place(projector_matrix)
        self.assertAllClose(dot(projector_matrix.transpose().conj(),projector_matrix),identity(number_of_projectors))

    @with_checker
    def test_completeness(self,projector_length=irange(8,20),number_of_projectors=irange(1,7)):
        projectors = crand(projector_length,number_of_projectors)
        projector_matrix = array(projectors.copy(),order='Fortran')
        core.orthogonalize_matrix_in_place(projector_matrix)
        self.assertAllClose(dot(projector_matrix,dot(projector_matrix.conj().transpose(),projectors)),projectors)

    @with_checker
    def test_rank(self,projector_length=irange(8,20),number_of_projectors=irange(1,7)):
        rank = randint(1,number_of_projectors)
        projector_matrix = array(dot(crand(projector_length,rank),crand(rank,number_of_projectors)),order='Fortran')
        computed_rank = core.orthogonalize_matrix_in_place(projector_matrix)
        self.assertEqual(computed_rank,rank)
#@+node:gcross.20111107123726.3193: *4* compute_orthogonal_basis
class compute_orthogonal_basis(TestCase):
    @with_checker
    def test_shape(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        _, basis = core.compute_orthogonal_basis(m,vectors)
        self.assertEqual(basis.shape,(m,m))

    @with_checker
    def test_orthogonality(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        _, basis = core.compute_orthogonal_basis(m,vectors)
        self.assertAllClose(dot(basis.transpose().conj(),basis),identity(m))

    @with_checker
    def test_rank(self,m=irange(8,20),n=irange(1,7)):
        rank = randint(1,n)
        vectors = array(dot(crand(m,rank),crand(rank,n)),order='Fortran')
        computed_rank, _ = core.compute_orthogonal_basis(m,vectors)
        self.assertEqual(computed_rank,rank)

    @with_checker
    def test_projector_subspace(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        _, basis = core.compute_orthogonal_basis(n,vectors)
        self.assertAllClose(dot(basis[:,:],dot(basis[:,:].conj().transpose(),vectors)),vectors)

    @with_checker
    def test_remaining_subspace(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        _, basis = core.compute_orthogonal_basis(m,vectors)
        self.assertAlmostEqual(norm(dot(basis[:,n:].conj().transpose(),vectors)),0)
#@+node:gcross.20111107123726.3194: *4* compute_orthogonal_subspace
class compute_orthogonal_subspace(TestCase):
    @with_checker
    def test_shape(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        basis = core.compute_orthogonal_subspace(vectors)
        self.assertEqual(basis.shape,(m,n-m))

    @with_checker
    def test_orthogonality(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        basis = core.compute_orthogonal_subspace(vectors)
        self.assertAllClose(dot(basis.transpose().conj(),basis),identity(n-m))

    @with_checker
    def test_subspace(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        _, basis = core.compute_orthogonal_subspace(vectors)
        self.assertAlmostEqual(norm(dot(basis.conj().transpose(),vectors)),0)
#@+node:gcross.20111107123726.3196: *4* swap_inplace
class swap_inplace(TestCase):
    @with_checker
    def test_correctness(self,n=irange(1,10)):
        swaps = [randint(1,n) for _ in xrange(n)]
        vector = crand(n)
        correct_vector = vector.copy()
        for (i,swap) in enumerate(swap-1 for swap in swaps):
            if i != swap:
                shelf = correct_vector[i]
                correct_vector[i] = correct_vector[swap]
                correct_vector[swap] = shelf
        core.swap_inplace(swaps,vector)
        self.assertAllEqual(vector,correct_vector)

    @with_checker
    def test_swap_followed_by_unswap(self,n=irange(1,10)):
        swaps = [randint(1,n) for _ in xrange(n)]
        vector = crand(n)
        correct_vector = vector.copy()
        core.swap_inplace(swaps,vector)
        core.unswap_inplace(swaps,vector)
        self.assertAllEqual(vector,correct_vector)
#@+node:gcross.20111107123726.3197: *4* unswap_inplace
class unswap_inplace(TestCase):
    @with_checker
    def test_correctness(self,n=irange(1,10)):
        swaps = [randint(1,n) for _ in xrange(n)]
        vector = crand(n)
        correct_vector = vector.copy()
        for (i,swap) in reversed(list(enumerate(swap-1 for swap in swaps))):
            if i != swap:
                shelf = correct_vector[i]
                correct_vector[i] = correct_vector[swap]
                correct_vector[swap] = shelf
        core.unswap_inplace(swaps,vector)
        self.assertAllEqual(vector,correct_vector)

    @with_checker
    def test_unswap_followed_by_swap(self,n=irange(1,10)):
        swaps = [randint(1,n) for _ in xrange(n)]
        vector = crand(n)
        correct_vector = vector.copy()
        core.unswap_inplace(swaps,vector)
        core.swap_inplace(swaps,vector)
        self.assertAllEqual(vector,correct_vector)
#@+node:gcross.20111107123726.3198: *4* swap_matrix_inplace
class swap_matrix_inplace(TestCase):
    @with_checker
    def test_correctness(self,n=irange(2,10)):
        swaps = [randint(1,n) for _ in xrange(n)]
        matrix = array(crand(n,n),order='F')
        correct_matrix = matrix.copy()
        for (i,swap) in enumerate(swap-1 for swap in swaps):
            if i != swap:
                shelf = correct_matrix[i,:].copy()
                correct_matrix[i,:] = correct_matrix[swap,:]
                correct_matrix[swap,:] = shelf
        for (i,swap) in enumerate(swap-1 for swap in swaps):
            if i != swap:
                shelf = correct_matrix[:,i].copy()
                correct_matrix[:,i] = correct_matrix[:,swap]
                correct_matrix[:,swap] = shelf
        core.swap_matrix_inplace(swaps,matrix)
        self.assertAllEqual(matrix,correct_matrix)

    @with_checker
    def test_subspace(self,n=irange(2,10)):
        swaps = [randint(1,n) for _ in xrange(n)]
        original_matrix = array(crand(n,n),order='F')
        original_vector_1 = crand(n)
        original_vector_2 = crand(n)

        matrix = array(original_matrix.copy(),order='F')
        vector_1 = original_vector_1.copy()
        vector_2 = original_vector_2.copy()

        core.swap_matrix_inplace(swaps,matrix)
        core.swap_inplace(swaps,vector_1)
        core.swap_inplace(swaps,vector_2)

        original_scalar = dot(original_vector_1,dot(original_matrix,original_vector_2))
        scalar = dot(vector_1,dot(matrix,vector_2))

        self.assertAlmostEqual(scalar,original_scalar)
#@+node:gcross.20111107123726.3199: *3* Projectors
#@+node:gcross.20111107123726.3200: *4* Functions
#@+node:gcross.20111107123726.3201: *5* generate_reflectors
def generate_reflectors(full_space_dimension,number_of_projectors=None,overproject=False):
    if number_of_projectors is None:
        if overproject:
            number_of_projectors = randint(1,full_space_dimension*3/2)
        else:
            number_of_projectors = randint(1,full_space_dimension-1)
    projectors = crand(full_space_dimension,number_of_projectors)
    reflectors = array(projectors,order='F')
    rank, coefficients, swaps = core.convert_vectors_to_reflectors(reflectors)
    orthogonal_subspace_dimension = full_space_dimension - rank
    return projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension
#@+node:gcross.20111107123726.3202: *5* generate_q
def generate_q(full_space_dimension):
    projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension,overproject=True)
    q = core.compute_q_from_reflectors(reflectors,coefficients,swaps)
    rank = full_space_dimension - orthogonal_subspace_dimension
    return projectors.conj().transpose(), q, rank
#@+node:gcross.20111107123726.3203: *4* compute_overlap_with_projectors
class compute_overlap_with_projectors(TestCase):
    @with_checker
    def test_correctness(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension,overproject=True)
        vector = crand(full_space_dimension)
        x = core.unproject_from_orthogonal_space(reflectors,coefficients,swaps,array([1,0]))
        self.assertAlmostEqual(
            core.compute_overlap_with_projectors(reflectors,coefficients,swaps,vector),
            norm(dot(vector,projectors))
        )
#@+node:gcross.20111107123726.3204: *4* compute_q_from_reflectors
class compute_q_from_reflectors(TestCase):
    @with_checker
    def test_shape(self,full_space_dimension=irange(2,10)):
        projectors, q, rank = generate_q(full_space_dimension)
        self.assertEqual(q.shape,(full_space_dimension,full_space_dimension))

    @with_checker
    def test_orthogonality(self,full_space_dimension=irange(2,10)):
        projectors, q, rank = generate_q(full_space_dimension)
        self.assertAllClose(dot(q.transpose().conj(),q),identity(full_space_dimension))

    @with_checker
    def test_projector_subspace(self,full_space_dimension=irange(2,10)):
        projectors, q, rank = generate_q(full_space_dimension)
        subspace = q[:,:rank]
        projectors_in_subspace = dot(projectors,subspace)
        for projector, projector_in_subspace in zip(projectors,projectors_in_subspace):
            self.assertAlmostEqual(norm(projector),norm(projector_in_subspace))
        new_projectors = dot(projectors_in_subspace,subspace.conj().transpose())
        self.assertAllClose(new_projectors,projectors)

    @with_checker
    def test_orthogonal_subspace(self,full_space_dimension=irange(2,10)):
        projectors, q, rank = generate_q(full_space_dimension)
        subspace = q[:,rank:]
        self.assertAlmostEqual(norm(dot(projectors,subspace)),0)

#@+node:gcross.20111107123726.3205: *4* project_into_orthogonal_space
class project_into_orthogonal_space(TestCase):
    #@+others
    #@+node:gcross.20111107123726.3206: *5* test_agreement_with_multiplication_by_q
    @with_checker
    def test_agreement_with_multiplication_by_q(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension)
        q = core.compute_q_from_reflectors(reflectors,coefficients,swaps)

        vector = crand(full_space_dimension)
        correct_projected_vector = dot(vector,q)[-orthogonal_subspace_dimension:]
        actual_projected_vector = core.project_into_orthogonal_space(orthogonal_subspace_dimension,reflectors,coefficients,swaps,vector)
        self.assertAllClose(correct_projected_vector,actual_projected_vector)
    #@+node:gcross.20111107123726.3207: *5* test_kills_projectors
    @with_checker
    def test_kills_projectors(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension)
        for projector in projectors.transpose():
            orthogonal_space_vector = core.project_into_orthogonal_space(orthogonal_subspace_dimension,reflectors,coefficients,swaps,projector.conj())
            self.assertAlmostEqual(norm(orthogonal_space_vector),0)
    #@+node:gcross.20111107123726.3208: *5* test_unproject_dot_project_on_arbitrary_vector
    @with_checker
    def test_unproject_dot_project_on_arbitrary_vector(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension)

        vector = \
            core.unproject_from_orthogonal_space(reflectors,coefficients,swaps,
                core.project_into_orthogonal_space(orthogonal_subspace_dimension,reflectors,coefficients,swaps,crand(full_space_dimension))
            )
        self.assertVanishing(dot(vector,projectors))
    #@+node:gcross.20111107123726.3209: *5* test_unproject_dot_project_on_orthogonal_random
    @with_checker
    def test_unproject_dot_project_on_orthogonal_random(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension)

        old_vector = core.unproject_from_orthogonal_space(reflectors,coefficients,swaps,crand(orthogonal_subspace_dimension))
        self.assertVanishing(dot(old_vector,projectors))
        new_vector = \
            core.unproject_from_orthogonal_space(reflectors,coefficients,swaps,
                core.project_into_orthogonal_space(orthogonal_subspace_dimension,reflectors,coefficients,swaps,old_vector)
            )
        self.assertAllClose(old_vector,new_vector)
    #@-others

#@+node:gcross.20111107123726.3210: *4* unproject_from_orthogonal_space
class unproject_from_orthogonal_space(TestCase):
    #@+others
    #@+node:gcross.20111107123726.3211: *5* test_agreement_with_multiplication_by_q
    @with_checker
    def test_agreement_with_multiplication_by_q(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension)
        q = core.compute_q_from_reflectors(reflectors,coefficients,swaps)

        vector = crand(orthogonal_subspace_dimension)
        correct_projected_vector = dot(vector,q[:,-orthogonal_subspace_dimension:].conj().transpose())
        actual_projected_vector = core.unproject_from_orthogonal_space(reflectors,coefficients,swaps,vector)
        self.assertAllClose(correct_projected_vector,actual_projected_vector)
    #@+node:gcross.20111107123726.3212: *5* test_result_is_in_orthogonal_subspace
    @with_checker
    def test_result_is_in_orthogonal_subspace(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension)

        vector = core.unproject_from_orthogonal_space(reflectors,coefficients,swaps,crand(orthogonal_subspace_dimension))
        self.assertVanishing(dot(vector,projectors))
    #@+node:gcross.20111107123726.3213: *5* test_project_dot_unproject_is_identity
    @with_checker
    def test_project_dot_unproject_is_identity(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension)

        old_vector = crand(orthogonal_subspace_dimension)
        new_vector = \
            core.project_into_orthogonal_space(orthogonal_subspace_dimension,reflectors,coefficients,swaps,
                core.unproject_from_orthogonal_space(reflectors,coefficients,swaps,old_vector)
            )
        self.assertAllClose(old_vector,new_vector)
    #@-others

#@+node:gcross.20111107123726.3214: *4* project_matrix_into_orthog_space
class project_matrix_into_orthog_space(TestCase):
    #@+others
    #@+node:gcross.20111107123726.3215: *5* test_agreement_with_multiplication_by_q
    @with_checker
    def test_agreement_with_multiplication_by_q(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension)
        q = core.compute_q_from_reflectors(reflectors,coefficients,swaps)

        matrix = crand(full_space_dimension,full_space_dimension)
        correct_projected_matrix = dot(q.transpose(),dot(matrix,q.conj()))[-orthogonal_subspace_dimension:,-orthogonal_subspace_dimension:]
        actual_projected_matrix = core.project_matrix_into_orthog_space(orthogonal_subspace_dimension,reflectors,coefficients,swaps,matrix)
        self.assertAllClose(correct_projected_matrix,actual_projected_matrix)
    #@+node:gcross.20111107123726.3216: *5* test_agreement_with_project
    @with_checker
    def test_agreement_with_project(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension)
        q = core.compute_q_from_reflectors(reflectors,coefficients,swaps)

        matrix = crand(full_space_dimension,full_space_dimension)
        projected_matrix = core.project_matrix_into_orthog_space(orthogonal_subspace_dimension,reflectors,coefficients,swaps,matrix)

        vector = crand(orthogonal_subspace_dimension)
        result_1 = dot(projected_matrix,vector)
        result_2 = \
            core.project_into_orthogonal_space(orthogonal_subspace_dimension,reflectors,coefficients,swaps,
                dot(matrix,
                    core.unproject_from_orthogonal_space(reflectors,coefficients,swaps,vector)
                )
            )

        self.assertAllClose(result_1,result_2)
    #@-others
#@+node:gcross.20111107123726.3217: *4* filter_components_outside_orthog
class filter_components_outside_orthog(TestCase):
    @with_checker
    def test_result_is_orthogonal(self,full_space_dimension=irange(2,10)):
        projectors, reflectors, coefficients, swaps, orthogonal_subspace_dimension = generate_reflectors(full_space_dimension,overproject=True)
        vector = dot(projectors.conj(),rand(projectors.shape[-1]))
        filtered_vector = core.filter_components_outside_orthog(orthogonal_subspace_dimension,reflectors,coefficients,swaps,vector)
        self.assertVanishing(dot(filtered_vector,projectors))
#@+node:gcross.20111107123726.3218: *3* Normalization
#@+node:gcross.20111107123726.3219: *4* norm_denorm_going_left
class norm_denorm_going_left(TestCase):
    #@+others
    #@-others

    @with_checker
    def testCorrectness(self,bll=irange(2,4),bl=irange(2,4),br=irange(2,4)):
        dl = d = 2
        tensor_to_denormalize = crand(bl,bll,d)
        tensor_to_normalize = crand(br,bl,d)
        info, denormalized_tensor, normalized_tensor = core.norm_denorm_going_left(tensor_to_denormalize,tensor_to_normalize)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((0,2,),)*2)
        self.assertAllClose(identity(bl),should_be_identity)
        self.assertAllClose(
            tensordot(tensor_to_denormalize,tensor_to_normalize,(0,1)),
            tensordot(denormalized_tensor,normalized_tensor,(0,1)),
        )

    @with_checker
    def testCorrectnessOnLarge(self):
        br = 10
        bl = 20
        bll = 40
        dl = d = 2
        tensor_to_denormalize = crand(bl,bll,d)
        tensor_to_normalize = crand(br,bl,d)
        info, denormalized_tensor, normalized_tensor = core.norm_denorm_going_left(tensor_to_denormalize,tensor_to_normalize)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((0,2,),)*2)
        self.assertAllClose(identity(bl),should_be_identity)
        self.assertAllClose(
            tensordot(tensor_to_denormalize,tensor_to_normalize,(0,1)),
            tensordot(denormalized_tensor,normalized_tensor,(0,1)),
        )
#@+node:gcross.20111107123726.3220: *4* norm_denorm_going_right
class norm_denorm_going_right(TestCase):
    #@+others
    #@-others

    @with_checker
    def testCorrectness(self,brr=irange(2,4),br=irange(2,4),bl=irange(2,4)):
        dr = d = 2
        tensor_to_normalize = crand(br,bl,d)
        tensor_to_denormalize = crand(brr,br,dr)
        info, normalized_tensor, denormalized_tensor = core.norm_denorm_going_right(tensor_to_normalize,tensor_to_denormalize)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((1,2,),)*2)
        self.assertAllClose(identity(br),should_be_identity)
        self.assertAllClose(
            tensordot(tensor_to_normalize,tensor_to_denormalize,(0,1)),
            tensordot(normalized_tensor,denormalized_tensor,(0,1)),
        )

    @with_checker
    def testCorrectnessOnLarge(self):
        bl = 10
        br = 20
        brr = 40
        dr = d = 2
        tensor_to_normalize = crand(br,bl,d)
        tensor_to_denormalize = crand(brr,br,dr)
        info, normalized_tensor, denormalized_tensor = core.norm_denorm_going_right(tensor_to_normalize,tensor_to_denormalize)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((1,2,),)*2)
        self.assertAllClose(identity(br),should_be_identity)
        self.assertAllClose(
            tensordot(tensor_to_normalize,tensor_to_denormalize,(0,1)),
            tensordot(normalized_tensor,denormalized_tensor,(0,1)),
        )
#@+node:gcross.20111107123726.3221: *3* Bandwidth increase
#@+node:gcross.20111107123726.3222: *4* create_bandwidth_increase_matrix
class create_bandwidth_increase_matrix(TestCase):
    #@+others
    #@-others

    @with_checker
    def testCorrectness(self,old_bandwidth=irange(2,8),new_bandwidth=irange(9,32)):
        matrix = core.create_bandwidth_increase_matrix(old_bandwidth,new_bandwidth)
        self.assertEqual((new_bandwidth,old_bandwidth),matrix.shape)
        should_be_identity = dot(matrix.transpose().conj(),matrix)
        self.assertAllClose(identity(old_bandwidth),should_be_identity)
#@+node:gcross.20111107123726.3223: *4* absorb_bi_matrix_from_left
class absorb_bi_matrix_from_left(TestCase):
    #@+others
    #@-others

    @with_checker
    def testCorrectness(self,br=irange(1,4),bl=irange(2,8),new_bl=irange(9,32),d=irange(2,4)):
        old_state_site_tensor = crand(br,bl,d)
        matrix = core.create_bandwidth_increase_matrix(bl,new_bl)
        new_state_site_tensor = core.absorb_bi_matrix_from_left(old_state_site_tensor,matrix)
        self.assertAllClose(tensordot(old_state_site_tensor,matrix.transpose().conj(),(1,0)).transpose(0,2,1),new_state_site_tensor)
        new_state_site_tensor = core.absorb_bi_matrix_from_left(new_state_site_tensor,matrix.conj().transpose())
        self.assertAllClose(old_state_site_tensor,new_state_site_tensor)
#@+node:gcross.20111107123726.3224: *4* absorb_bi_matrix_from_right
class absorb_bi_matrix_from_right(TestCase):
    #@+others
    #@-others

    @with_checker
    def testCorrectness(self,br=irange(2,8),new_br=irange(9,32),bl=irange(1,4),d=irange(2,4)):
        old_state_site_tensor = crand(br,bl,d)
        matrix = core.create_bandwidth_increase_matrix(br,new_br)
        new_state_site_tensor = core.absorb_bi_matrix_from_right(old_state_site_tensor,matrix)
        self.assertAllClose(tensordot(matrix,old_state_site_tensor,(1,0)),new_state_site_tensor)
        new_state_site_tensor = core.absorb_bi_matrix_from_right(new_state_site_tensor,matrix.transpose().conj())
        self.assertAllClose(old_state_site_tensor,new_state_site_tensor)
#@+node:gcross.20111107123726.3225: *4* increase_bandwidth_between
class increase_bandwidth_between(TestCase):
    #@+others
    #@-others

    @with_checker
    def testCorrectness(self,
        bl=irange(2,4),
        bm=irange(2,4),
        new_bm=irange(5,8),
        br=irange(2,4),
        dl=irange(4,5),
        dr=irange(4,5)
    ):
        old_left_tensor = crand(bm,bl,dl)
        old_right_tensor = crand(br,bm,dr)
        info, new_left_tensor, new_right_tensor = core.increase_bandwidth_between(new_bm,old_left_tensor,old_right_tensor)
        self.assertEqual(0,info)
        should_be_identity = tensordot(new_right_tensor.conj(),new_right_tensor,((0,2,),)*2)
        self.assertAllClose(identity(new_bm),should_be_identity)
        self.assertAllClose(
            tensordot(old_left_tensor,old_right_tensor,(0,1)),
            tensordot(new_left_tensor,new_right_tensor,(0,1)),
        )
#@+node:gcross.20111107123726.3226: *3* Overlap tensor formation
#@+node:gcross.20111107123726.3227: *4* form_overlap_site_tensor
class form_overlap_site_tensor(TestCase):
    #@+others
    #@-others

    @with_checker
    def test_correctness(self,br=irange(2,4),bl=irange(2,4),d=irange(2,4)):
        state_site_tensor = crand(br,bl,d)
        overlap_site_tensor = core.form_overlap_site_tensor(state_site_tensor)
        self.assertAllClose(state_site_tensor.transpose(1,2,0).conj(),overlap_site_tensor)
#@+node:gcross.20111107123726.3228: *4* form_norm_overlap_tensors
class form_norm_overlap_tensors(TestCase):
    #@+others
    #@-others

    @with_checker
    def test_correctness(self,br=irange(2,4),bm=irange(2,4),bl=irange(2,4),dl=irange(2,4),dr=irange(2,4)):
        unnormalized_state_tensor_1 = crand(bm,bl,dl)
        right_norm_state_tensor_2 = crand(br,bm,dr)
        resulting_tensors = core.form_norm_overlap_tensors(unnormalized_state_tensor_1,right_norm_state_tensor_2)
        left_norm_overlap_tensor_1 = resulting_tensors[0]
        unnormalized_overlap_tensor_1 = resulting_tensors[1]
        unnormalized_state_tensor_2 = resulting_tensors[2]
        right_norm_overlap_tensor_2 = resulting_tensors[3]
        self.assertAllClose(right_norm_state_tensor_2,right_norm_overlap_tensor_2.transpose(2,0,1).conj())
        self.assertAllClose(unnormalized_state_tensor_1,unnormalized_overlap_tensor_1.transpose(2,0,1).conj())
        left_norm_state_tensor_1 = left_norm_overlap_tensor_1.transpose(2,0,1).conj()
        self.assertAllClose(
            tensordot(unnormalized_state_tensor_1,right_norm_state_tensor_2,(0,1)),
            tensordot(left_norm_state_tensor_1,unnormalized_state_tensor_2,(0,1)),
        )
#@+node:gcross.20111107123726.3229: *3* non_hermitian_matrix_detection
class non_hermitian_matrix_detection(TestCase):
    @with_checker
    def test_hermitian_operators(self,number_of_sites=irange(2,10)):
        left_boundary_1 = ones((1,),complex128)
        left_boundary_2 = ones((1,),complex128)
        old_c = 1
        for i in xrange(number_of_sites):
            if i == number_of_sites-1:
                new_c = 1
            else:
                new_c = randint(1,5)
            d = randint(1,5)
            sparse_operator_indices, sparse_operator_matrices, _ = generate_random_sparse_matrices(old_c,new_c,d,symmetric=True)
            left_boundary_1, left_boundary_2 = core.contract_operator_random_left(
                new_c, d,
                left_boundary_1, left_boundary_2,
                sparse_operator_indices, sparse_operator_matrices.reshape(d*d,sparse_operator_matrices.shape[-1])
            )
            old_c = new_c
        self.assertEqual(left_boundary_1.shape,(1,))
        self.assertEqual(left_boundary_2.shape,(1,))
        self.assertAlmostEqual(left_boundary_1.ravel(),left_boundary_2.conj().ravel())


    @with_checker
    def test_non_hermitian_operators(self,number_of_sites=irange(2,10)):
        left_boundary_1 = ones((1,),complex128)
        left_boundary_2 = ones((1,),complex128)
        old_c = 1
        for i in xrange(number_of_sites):
            if i == number_of_sites-1:
                new_c = 1
            else:
                new_c = randint(1,5)
            d = randint(1,5)
            sparse_operator_indices, sparse_operator_matrices, _ = generate_random_sparse_matrices(old_c,new_c,d,symmetric=False)
            left_boundary_1, left_boundary_2 = core.contract_operator_random_left(
                new_c, d,
                left_boundary_1, left_boundary_2,
                sparse_operator_indices, sparse_operator_matrices.reshape(d*d,sparse_operator_matrices.shape[-1])
            )
            old_c = new_c
        self.assertEqual(left_boundary_1.shape,(1,))
        self.assertEqual(left_boundary_2.shape,(1,))
        if norm(left_boundary_1) > 1e-7:
            self.assertNotAlmostEqual(left_boundary_1.ravel(),left_boundary_2.conj().ravel())
#@-others

set_printoptions(linewidth=132)

tests = [
    iteration_stage_1,
    iteration_stage_2,
    iteration_stage_3,
    contract_sos_left,
    contract_sos_right_stage_1,
    contract_sos_right_stage_2a,
    contract_sos_right_stage_2b,
    contract_sos_right_stage_2,
    contract_sos_right,
    contract_vs_left,
    contract_vs_right,
    form_overlap_vector,
    compute_expectation,
    compute_optimization_matrix,
    rand_norm_state_site_tensor,
    rand_unnorm_state_site_tensor,
    norm_denorm_going_left,
    norm_denorm_going_right,
    create_bandwidth_increase_matrix,
    absorb_bi_matrix_from_left,
    absorb_bi_matrix_from_right,
    increase_bandwidth_between,
    orthogonalize_matrix_in_place,
    compute_orthogonal_basis,
    compute_overlap_with_projectors,
    compute_q_from_reflectors,
    project_into_orthogonal_space,
    unproject_from_orthogonal_space,
    project_matrix_into_orthog_space,
    form_overlap_site_tensor,
    form_norm_overlap_tensors,
    filter_components_outside_orthog,
    swap_inplace,
    unswap_inplace,
    swap_matrix_inplace,
    non_hermitian_matrix_detection,
    extend_state_vector_fragment,
    contract_matrix_left,
]

#@+<< Runner >>
#@+node:gcross.20111107123726.3230: ** << Runner >>
unittest.TextTestRunner(verbosity=2).run(unittest.TestSuite(map(unittest.defaultTestLoader.loadTestsFromTestCase, tests)))
#@-<< Runner >>
#@-leo
