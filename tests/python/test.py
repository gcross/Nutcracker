#@+leo-ver=4-thin
#@+node:gcross.20091106154604.1979:@thin test.py
#@<< Import needed modules >>
#@+node:gcross.20091115094257.1715:<< Import needed modules >>
import unittest
from paycheck import *
from numpy import array, zeros, all, double, tensordot, multiply, complex128, allclose, ones, diag, identity, dot, argmin, rank
from numpy.linalg import norm
from numpy.random import rand
from scipy.linalg import qr, eigh
from random import randint, choice
import random
import __builtin__
import vmps
#@-node:gcross.20091115094257.1715:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091108152444.1533:Functions
#@+node:gcross.20091110205054.1959:Macro building functions
#@+at
# These routines build macros to perform tensor contractions.  They do this
# by construction a string of Python code which performs the contraction,
# and then compiling the code into a function.
#@-at
#@@c

#@+others
#@+node:gcross.20091110205054.1960:n2l
#@+at
# Utility function converting numbers to letters.
#@-at
#@@c
n2l = map(chr,range(ord('A'),ord('Z')+1))
#@-node:gcross.20091110205054.1960:n2l
#@+node:gcross.20091110205054.1961:make_contractor
def make_contractor(tensor_index_labels,index_join_pairs,result_index_labels,name="f"):    # pre-process parameters
    tensor_index_labels = list(map(list,tensor_index_labels))
    index_join_pairs = list(index_join_pairs)
    result_index_labels = list([list(index_group) if hasattr(index_group,"__getitem__") else [index_group] for index_group in result_index_labels])

    assert sum(len(index_group) for index_group in tensor_index_labels) == (sum(len(index_group) for index_group in result_index_labels)+2*len(index_join_pairs))

    function_definition_statements = ["def %s(%s):" % (name,",".join(n2l[:len(tensor_index_labels)]))]

    #@    << def build_statements >>
    #@+node:gcross.20091110205054.1962:<< def build_statements >>
    def build_statements(tensor_index_labels,index_join_pairs,result_index_labels):
    #@+at
    # This routine recursively builds a list of statements which performs the 
    # full tensor contraction.
    # 
    # First, if there is only one tensor left, then transpose and reshape it 
    # to match the result_index_labels.
    #@-at
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
    # Second, if all joins have finished, then take outer products to combine 
    # all remaining tensors into one.
    #@-at
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
    # Otherwise, do the first join, walking through index_join_pairs to find 
    # any other pairs which connect the same two tensors.
    #@-at
    #@@c
        else:
            #@        << Search for all joins between these tensors >>
            #@+node:gcross.20091110205054.1963:<< Search for all joins between these tensors >>
            #@+at
            # This function searches for the tensors which are joined, and 
            # reorders the indices in the join so that the index corresponding 
            # to the tensor appearing first in the list of tensors appears 
            # first in the join.
            #@-at
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

            #@-node:gcross.20091110205054.1963:<< Search for all joins between these tensors >>
            #@nl

            #@        << Build tensor contraction statements >>
            #@+node:gcross.20091110205054.1964:<< Build tensor contraction statements >>
            tensor_vars = [n2l[id] for id in tensor_ids]

            statements = [
                "try:",
                "   %s = tensordot(%s,%s,%s)" % (tensor_vars[0],tensor_vars[0],tensor_vars[1],indices),
                "   del %s" % tensor_vars[1],
                "except ValueError:",
                "   raise ValueError('indices %%s do not match for tensor %%i, shape %%s, and tensor %%i, shape %%s.' %% (%s,%i,%s.shape,%i,%s.shape))" % (indices,tensor_ids[0],tensor_vars[0],tensor_ids[1],tensor_vars[1])
            ]
            #@-node:gcross.20091110205054.1964:<< Build tensor contraction statements >>
            #@nl

            #@        << Delete joins from list and update tensor specifications >>
            #@+node:gcross.20091110205054.1965:<< Delete joins from list and update tensor specifications >>
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
            #@-node:gcross.20091110205054.1965:<< Delete joins from list and update tensor specifications >>
            #@nl

            return statements + build_statements(tensor_index_labels,index_join_pairs,result_index_labels)
    #@-node:gcross.20091110205054.1962:<< def build_statements >>
    #@nl

    function_definition_statements += ["\t" + statement for statement in build_statements(tensor_index_labels,index_join_pairs,result_index_labels)]

    function_definition = "\n".join(function_definition_statements)+"\n"

    f_globals = {"tensordot":tensordot,"multiply":multiply}
    f_locals = {}

    exec function_definition in f_globals, f_locals

    f = f_locals[name]
    f.source = function_definition
    return f
#@nonl
#@-node:gcross.20091110205054.1961:make_contractor
#@+node:gcross.20091110205054.1966:make_contractor_from_implicit_joins
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
#@nonl
#@-node:gcross.20091110205054.1966:make_contractor_from_implicit_joins
#@-others
#@-node:gcross.20091110205054.1959:Macro building functions
#@+node:gcross.20091110205054.1968:crand
def crand(*shape):
    return rand(*shape)*2-1+rand(*shape)*2j-1j
#@-node:gcross.20091110205054.1968:crand
#@+node:gcross.20091110011014.1558:generate_random_sparse_matrices
def generate_random_sparse_matrices(cl,cr,d):
    sparse_operator_indices = array([(randint(1,cl),randint(1,cr)) for _ in xrange(randint(2,cl+cr))]).transpose()
    sparse_operator_matrices = crand(d,d,sparse_operator_indices.shape[-1])
    sparse_operator_matrices += sparse_operator_matrices.transpose(1,0,2).conj()
    operator_site_tensor = zeros((d,d,cr,cl),complex128)
    for (index1,index2),matrix in zip(sparse_operator_indices.transpose(),sparse_operator_matrices.transpose(2,0,1)):
        operator_site_tensor[...,index2-1,index1-1] += matrix
    return sparse_operator_indices, sparse_operator_matrices, operator_site_tensor
#@-node:gcross.20091110011014.1558:generate_random_sparse_matrices
#@+node:gcross.20091110135225.1559:form_contractor
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
#@-node:gcross.20091110135225.1559:form_contractor
#@-node:gcross.20091108152444.1533:Functions
#@+node:gcross.20091110205054.1947:Tests
#@+node:gcross.20091108152444.1534:Contractors
#@+others
#@+node:gcross.20091110135225.1567:iteration
#@+node:gcross.20091106154604.1986:iteration_stage_1
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

class iteration_stage_1(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        b = irange(2,20),
        cl = irange(2,10),
        cr = irange(2,10),
    ):
        d = 2
        left_environment = crand(b,b,cl)
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(cl,cr,d)
        actual_output_tensor = vmps.iteration_stage_1(cr,left_environment,sparse_operator_indices,sparse_operator_matrices)
        correct_output_tensor = iteration_stage_1_correct_contractor(left_environment,operator_site_tensor)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091106154604.1986:iteration_stage_1
#@+node:gcross.20091107163338.1531:iteration_stage_2
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

class iteration_stage_2(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        cr = irange(2,10),
    ):
        d = 2
        iteration_stage_1_tensor = crand(bl,d,cr,bl,d)
        state_site_tensor = crand(br,bl,d)
        actual_output_tensor = vmps.iteration_stage_2(iteration_stage_1_tensor,state_site_tensor)
        correct_output_tensor = iteration_stage_2_correct_contractor(iteration_stage_1_tensor,state_site_tensor)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091107163338.1531:iteration_stage_2
#@+node:gcross.20091110011014.1555:iteration_stage_3
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

class iteration_stage_3(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        cr = irange(2,10),
    ):
        d = 2
        iteration_stage_2_tensor = crand(br,cr,br,d)
        right_environment = crand(br,br,cr)
        actual_output_tensor = vmps.iteration_stage_3(iteration_stage_2_tensor,right_environment)
        correct_output_tensor = iteration_stage_3_correct_contractor(iteration_stage_2_tensor,right_environment)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091110011014.1555:iteration_stage_3
#@-node:gcross.20091110135225.1567:iteration
#@+node:gcross.20091110135225.1568:contract_sos
#@+node:gcross.20091110011014.1557:contract_sos_left
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

class contract_sos_left(unittest.TestCase):

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
        actual_output_tensor = vmps.contract_sos_left(
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
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091110011014.1557:contract_sos_left
#@+node:gcross.20091110135225.1563:contract_sos_right_stage_1
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

class contract_sos_right_stage_1(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        cr = irange(2,10),
    ):
        d = 2
        right_environment = crand(br,br,cr)
        state_site_tensor = crand(br,bl,d)
        actual_output_tensor = vmps.contract_sos_right_stage_1(
            right_environment,
            state_site_tensor
        )
        correct_output_tensor = contract_sos_right_stage_1_correct_contractor(
            right_environment,
            state_site_tensor.conj(),
        )
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091110135225.1563:contract_sos_right_stage_1
#@+node:gcross.20091110135225.1574:contract_sos_right_stage_2a
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

class contract_sos_right_stage_2a(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
        c = irange(2,10),
    ):
        d = 2
        state_site_tensor = crand(br,bl,d)
        matrix = crand(d,d)
        actual_output_tensor = vmps.contract_sos_right_stage_2a(
            matrix,
            state_site_tensor,
        )
        correct_output_tensor = contract_sos_right_stage_2a_correct_contractor(
            matrix,
            state_site_tensor
        )
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091110135225.1574:contract_sos_right_stage_2a
#@+node:gcross.20091110135225.1576:contract_sos_right_stage_2b
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

class contract_sos_right_stage_2b(unittest.TestCase):

    @with_checker(number_of_calls=10)
    def test_agreement_with_contractor(self,
        bl = irange(2,20),
        br = irange(2,20),
    ):
        d = 2
        A = crand(bl,d,br)
        B = crand(bl,d,br)
        actual_output_tensor = zeros((bl,bl),complex128,order='Fortran')
        vmps.contract_sos_right_stage_2b(A,B,actual_output_tensor)
        correct_output_tensor = contract_sos_right_stage_2b_correct_contractor(A,B)
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091110135225.1576:contract_sos_right_stage_2b
#@+node:gcross.20091110135225.1566:contract_sos_right_stage_2
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

class contract_sos_right_stage_2(unittest.TestCase):

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
        actual_output_tensor = vmps.contract_sos_right_stage_2(
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
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091110135225.1566:contract_sos_right_stage_2
#@+node:gcross.20091110205054.1909:contract_sos_right
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

class contract_sos_right(unittest.TestCase):

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
        actual_output_tensor = vmps.contract_sos_right(
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
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091110205054.1909:contract_sos_right
#@-node:gcross.20091110135225.1568:contract_sos
#@+node:gcross.20091116094945.1741:contract_ss
#@+node:gcross.20091116094945.1740:contract_ss_left
contract_ss_left_correct_contractor = form_contractor([
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

class contract_ss_left(unittest.TestCase):

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
        actual_output_tensor = vmps.contract_ss_left(
            left_environment,
            old_state_site_tensor,
            new_state_site_tensor,
        )
        correct_output_tensor = contract_ss_left_correct_contractor(
            left_environment,
            old_state_site_tensor,
            new_state_site_tensor,
        )
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091116094945.1740:contract_ss_left
#@+node:gcross.20091116094945.1745:contract_ss_right
contract_ss_right_correct_contractor = form_contractor([
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

class contract_ss_right(unittest.TestCase):

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
        actual_output_tensor = vmps.contract_ss_right(
            right_environment,
            old_state_site_tensor,
            new_state_site_tensor,
        )
        correct_output_tensor = contract_ss_right_correct_contractor(
            right_environment,
            old_state_site_tensor,
            new_state_site_tensor,
        )
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091116094945.1745:contract_ss_right
#@+node:gcross.20091116094945.1750:form_overlap_vector
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

class form_overlap_vector(unittest.TestCase):

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
        actual_output_tensor = vmps.form_overlap_vector(
            left_environment,
            right_environment,
            old_state_site_tensor,
        )
        correct_output_tensor = form_overlap_vector_correct_contractor(
            left_environment,
            right_environment,
            old_state_site_tensor,
        )
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091116094945.1750:form_overlap_vector
#@-node:gcross.20091116094945.1741:contract_ss
#@+node:gcross.20091110205054.1918:compute_expectation
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

class compute_expectation(unittest.TestCase):

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
        actual_output_tensor = vmps.compute_expectation(
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
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20091110205054.1918:compute_expectation
#@+node:gcross.20100513131210.1746:compute_optimization_matrix
class compute_optimization_matrix(unittest.TestCase):

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
        actual_output_tensor = vmps.compute_optimization_matrix(
            left_environment,
            sparse_operator_indices, sparse_operator_matrices,
            right_environment,
        )
        correct_output_tensor = optimization_matrix_contractor(
            left_environment,
            operator_site_tensor,
            right_environment,
        )
        self.assertTrue(allclose(actual_output_tensor,correct_output_tensor))
#@-node:gcross.20100513131210.1746:compute_optimization_matrix
#@-others
#@-node:gcross.20091108152444.1534:Contractors
#@+node:gcross.20100517000234.1763:Optimization
#@+others
#@+node:gcross.20100517000234.1764:optimization_matrix_contractor
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
#@-node:gcross.20100517000234.1764:optimization_matrix_contractor
#@+node:gcross.20091109182634.1543:optimizer_tests
class optimizer_tests(unittest.TestCase):
    #@    @+others
    #@+node:gcross.20100506200958.2701:test_correct_result_for_arbitrary_operator
    @with_checker(number_of_calls=10)
    def test_correct_result_for_arbitrary_operator(self):
        d = randint(*self.d_range)
        bl = randint(*self.b_range)
        br = randint(*self.b_range)
        cl = randint(*self.c_range)
        cr = randint(*self.c_range)
        left_environment = crand(bl,bl,cl)
        left_environment += left_environment.transpose(1,0,2).conj()
        right_environment = crand(br,br,cr)
        right_environment += right_environment.transpose(1,0,2).conj()
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(cl,cr,d)
        optimization_matrix = optimization_matrix_contractor(left_environment,operator_site_tensor,right_environment).reshape(d*bl*br,d*bl*br)
        self.assertTrue(allclose(optimization_matrix,optimization_matrix.conj().transpose()))
        guess = crand(br,bl,d)
        info, result, actual_eigenvalue = \
            self.call_optimizer(left_environment,sparse_operator_indices,sparse_operator_matrices,right_environment,zeros((0,0)),"SR",0,10000,guess)
        self.assertEqual(info,0)
        correct_eigenvalues, correct_eigenvectors = eigh(optimization_matrix)
        correct_eigenvectors = correct_eigenvectors.transpose()
        correct_solution_index = argmin(correct_eigenvalues)
        correct_eigenvalue = correct_eigenvalues[correct_solution_index]
        self.assertAlmostEqual(actual_eigenvalue,correct_eigenvalue)
        actual_eigenvector = result.ravel()
        actual_eigenvector /= actual_eigenvector[0]
        correct_eigenvector = correct_eigenvectors[correct_solution_index]
        correct_eigenvector /= correct_eigenvector[0]
        self.assertTrue(allclose(actual_eigenvector,correct_eigenvector))
    #@-node:gcross.20100506200958.2701:test_correct_result_for_arbitrary_operator
    #@+node:gcross.20100517150547.1762:test_correct_result_for_arbitrary_operator_with_projectors
    @with_checker(number_of_calls=10)
    def test_correct_result_for_arbitrary_operator_with_projectors(self):
        d = randint(*self.d_range)
        bl = randint(*self.b_range)
        br = randint(*self.b_range)
        cl = randint(*self.c_range)
        cr = randint(*self.c_range)

        left_environment = crand(bl,bl,cl)
        left_environment += left_environment.transpose(1,0,2).conj()
        right_environment = crand(br,br,cr)
        right_environment += right_environment.transpose(1,0,2).conj()
        sparse_operator_indices, sparse_operator_matrices, operator_site_tensor = generate_random_sparse_matrices(cl,cr,d)
        optimization_matrix = optimization_matrix_contractor(left_environment,operator_site_tensor,right_environment).transpose().reshape(d*bl*br,d*bl*br).transpose()
        self.assertTrue(allclose(optimization_matrix,optimization_matrix.conj().transpose()))

        if d*bl*br == 1:
            number_of_projectors = 0
        else:
            number_of_projectors = randint(1,d*bl*br-1)

        self.assertTrue(d*bl*br>0)
        self.assertTrue(d*bl*br>number_of_projectors)

        projector_matrix, _ = qr(crand(d*bl*br,number_of_projectors),overwrite_a=True,econ=False,mode='qr')
        projector_rank = rank(projector_matrix)
        projectors = array(projector_matrix[:,:projector_rank],order='Fortran')

        guess = vmps.project(projectors,crand(br*bl*d)).reshape(d,bl,br).transpose()
        guess /= norm(guess.ravel())
        info, result, actual_eigenvalue = \
            self.call_optimizer(left_environment,sparse_operator_indices,sparse_operator_matrices,right_environment,projectors,"SR",0,10000,guess)
        self.assertEqual(info,0)
        actual_eigenvector = result.transpose().ravel()
        self.assertAlmostEqual(norm(dot(projectors.conj().transpose(),actual_eigenvector)),0)
        self.assertAlmostEqual(dot(actual_eigenvector.conj(),dot(optimization_matrix,actual_eigenvector))/norm(actual_eigenvector),actual_eigenvalue)

        orthogonal_subspace = projector_matrix[:,projector_rank:]
        projected_optimization_matrix = \
            dot(orthogonal_subspace.conj().transpose(),dot(optimization_matrix,orthogonal_subspace))
        self.assertTrue(allclose(projected_optimization_matrix.conj().transpose(),projected_optimization_matrix))
        correct_eigenvalues, correct_projected_eigenvectors = eigh(projected_optimization_matrix)
        correct_eigenvectors = dot(orthogonal_subspace,correct_projected_eigenvectors).transpose()
        correct_solution_index = argmin(correct_eigenvalues)
        correct_eigenvalue = correct_eigenvalues[correct_solution_index]
        self.assertAlmostEqual(actual_eigenvalue,correct_eigenvalue)
        actual_eigenvector /= actual_eigenvector[0]
        correct_eigenvector = correct_eigenvectors[correct_solution_index]
        correct_eigenvector /= correct_eigenvector[0]
        self.assertTrue(allclose(actual_eigenvector,correct_eigenvector))
    #@-node:gcross.20100517150547.1762:test_correct_result_for_arbitrary_operator_with_projectors
    #@+node:gcross.20091109182634.1546:test_correct_result_for_simple_operator
    def test_correct_result_for_simple_operator(self):
        left_environment = ones((1,1,1),complex128)
        right_environment = ones((1,1,1),complex128)
        sparse_operator_indices = ones((2,1))
        sparse_operator_matrices = diag([1,1,1,-1])
        sparse_operator_matrices = sparse_operator_matrices.reshape(sparse_operator_matrices.shape + (1,))
        info, result, eigenvalue = self.call_optimizer(left_environment,sparse_operator_indices,sparse_operator_matrices,right_environment,zeros((0,0)),"SR",0,10000,crand(1,1,4))
        self.assertEqual(info,0)
        result /= result[0,0,-1]
        self.assertTrue(allclose(result.ravel(),array([0,0,0,1])))
        self.assertAlmostEqual(-1,eigenvalue)
    #@-node:gcross.20091109182634.1546:test_correct_result_for_simple_operator
    #@+node:gcross.20091115201814.1735:test_orthogonalization_1
    def test_orthogonalization_1(self):
        left_environment = ones((1,1,1),complex128)
        right_environment = ones((1,1,1),complex128)
        sparse_operator_indices = ones((2,1))
        sparse_operator_matrices = diag([1,1,-1,-2])
        sparse_operator_matrices = sparse_operator_matrices.reshape(sparse_operator_matrices.shape + (1,))
        projectors = array([[0,0,0,1]]).transpose()
        guess = vmps.project(projectors,crand(4)).reshape(1,1,4)
        info, result, eigenvalue = self.call_optimizer(left_environment,sparse_operator_indices,sparse_operator_matrices,right_environment,projectors,"SR",0,10000,guess)
        self.assertEqual(info,0)
        result /= result[0,0,-2]
        self.assertTrue(allclose(result.ravel(),array([0,0,1,0])))
        self.assertAlmostEqual(-1,eigenvalue)
    #@-node:gcross.20091115201814.1735:test_orthogonalization_1
    #@+node:gcross.20091119150241.1879:test_orthogonalization_1_complex
    def test_orthogonalization_1_complex(self):
        left_environment = ones((1,1,1),complex128)
        right_environment = ones((1,1,1),complex128)
        sparse_operator_indices = ones((2,1))
        sparse_operator_matrices = diag([1,1,-1,-2])
        sparse_operator_matrices = sparse_operator_matrices.reshape(sparse_operator_matrices.shape + (1,))
        projectors = array([[0,0,0,1j]]).transpose()
        guess = vmps.project(projectors,crand(4)).reshape(1,1,4)
        info, result, eigenvalue = self.call_optimizer(left_environment,sparse_operator_indices,sparse_operator_matrices,right_environment,projectors,"SR",0,10000,guess)
        self.assertEqual(info,0)
        result /= result[0,0,-2]
        self.assertTrue(allclose(result.ravel(),array([0,0,1,0])))
        self.assertAlmostEqual(-1,eigenvalue)
    #@-node:gcross.20091119150241.1879:test_orthogonalization_1_complex
    #@+node:gcross.20091115201814.1738:test_orthogonalization_2
    def test_orthogonalization_2(self):
        left_environment = ones((1,1,1),complex128)
        right_environment = ones((1,1,1),complex128)
        sparse_operator_indices = ones((2,1))
        sparse_operator_matrices = diag([-1,-2,-3,-4])
        sparse_operator_matrices = sparse_operator_matrices.reshape(sparse_operator_matrices.shape + (1,))
        projectors = array([[0,0,0,1],[0,0,1,0]]).transpose()
        guess = vmps.project(projectors,crand(4)).reshape(1,1,4)
        info, result, eigenvalue = self.call_optimizer(left_environment,sparse_operator_indices,sparse_operator_matrices,right_environment,projectors,"SR",0,10000,guess)
        self.assertEqual(info,0)
        result /= result[0,0,1]
        self.assertTrue(allclose(result.ravel(),array([0,1,0,0])))
        self.assertAlmostEqual(-2,eigenvalue)
    #@-node:gcross.20091115201814.1738:test_orthogonalization_2
    #@+node:gcross.20091119150241.1877:test_orthogonalization_2_complex
    def test_orthogonalization_2_complex(self):
        left_environment = ones((1,1,1),complex128)
        right_environment = ones((1,1,1),complex128)
        sparse_operator_indices = ones((2,1))
        sparse_operator_matrices = diag([-1,-2,-3,-4])
        sparse_operator_matrices = sparse_operator_matrices.reshape(sparse_operator_matrices.shape + (1,))
        projectors = array([[0,0,0,1j],[0,0,1j,0]]).transpose()
        guess = vmps.project(projectors,crand(4)).reshape(1,1,4)
        info, result, eigenvalue = self.call_optimizer(left_environment,sparse_operator_indices,sparse_operator_matrices,right_environment,projectors,"SR",0,10000,guess)
        self.assertEqual(info,0)
        result /= result[0,0,1]
        self.assertTrue(allclose(result.ravel(),array([0,1,0,0])))
        self.assertAlmostEqual(-2,eigenvalue)
    #@-node:gcross.20091119150241.1877:test_orthogonalization_2_complex
    #@-others
#@-node:gcross.20091109182634.1543:optimizer_tests
#@-others

class optimize(optimizer_tests):
    d_range = (1,4)
    b_range = (1,10)
    c_range = (1,10)
    call_optimizer = staticmethod(vmps.optimize)

class optimize_strategy_1(optimizer_tests):
    d_range = (1,4)
    b_range = (1,4)
    c_range = (1,4)
    call_optimizer = staticmethod(vmps.optimize_strategy_1)

class optimize_strategy_2(optimizer_tests):
    d_range = (2,4)
    b_range = (2,10)
    c_range = (2,10)
    call_optimizer = staticmethod(vmps.optimize_strategy_2)

class optimize_strategy_3(optimizer_tests):
    d_range = (2,4)
    b_range = (2,10)
    c_range = (2,10)
    call_optimizer = staticmethod(vmps.optimize_strategy_3)
#@-node:gcross.20100517000234.1763:Optimization
#@+node:gcross.20091123113033.1634:Randomization
#@+node:gcross.20091110205054.1924:rand_norm_state_site_tensor
class rand_norm_state_site_tensor(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def testCorrectness(self,bl=irange(2,4),br=irange(2,4)):
        d = 2
        normalized_tensor = vmps.rand_norm_state_site_tensor(br,bl,d)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((0,2,),)*2)
        self.assertTrue(allclose(identity(bl),should_be_identity))
#@-node:gcross.20091110205054.1924:rand_norm_state_site_tensor
#@+node:gcross.20091123113033.1633:rand_unnorm_state_site_tensor
class rand_unnorm_state_site_tensor(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def testCorrectness(self,bl=irange(2,4),br=irange(2,4)):
        d = 2
        unnormalized_tensor = vmps.rand_unnorm_state_site_tensor(br,bl,d)
        self.assertAlmostEqual(1,norm(unnormalized_tensor.ravel()))
#@-node:gcross.20091123113033.1633:rand_unnorm_state_site_tensor
#@+node:gcross.20100521141104.1772:random_projector_matrix
class random_projector_matrix(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def test_correctness(self,n=irange(2,10)):
        number_of_projectors = randint(1,n)
        projector_matrix = vmps.random_projector_matrix(n,number_of_projectors)
        self.assertTrue(allclose(dot(projector_matrix.conj().transpose(),projector_matrix),identity(number_of_projectors)))
#@-node:gcross.20100521141104.1772:random_projector_matrix
#@-node:gcross.20091123113033.1634:Randomization
#@+node:gcross.20100514235202.1744:Utility Functions
#@+node:gcross.20091116175016.1815:orthogonalize_matrix_in_place
class orthogonalize_matrix_in_place(unittest.TestCase):
    @with_checker
    def test_orthogonality(self,projector_length=irange(8,20),number_of_projectors=irange(1,7)):
        projector_matrix = array(crand(projector_length,number_of_projectors),order='Fortran')
        vmps.orthogonalize_matrix_in_place(projector_matrix)
        self.assertTrue(allclose(dot(projector_matrix.transpose().conj(),projector_matrix),identity(number_of_projectors)))

    @with_checker
    def test_completeness(self,projector_length=irange(8,20),number_of_projectors=irange(1,7)):
        projectors = crand(projector_length,number_of_projectors)
        projector_matrix = array(projectors.copy(),order='Fortran')
        vmps.orthogonalize_matrix_in_place(projector_matrix)
        self.assertTrue(allclose(dot(projector_matrix,dot(projector_matrix.conj().transpose(),projectors)),projectors))

    @with_checker
    def test_rank(self,projector_length=irange(8,20),number_of_projectors=irange(1,7)):
        rank = randint(1,number_of_projectors)
        projector_matrix = array(dot(crand(projector_length,rank),crand(rank,number_of_projectors)),order='Fortran')
        computed_rank = vmps.orthogonalize_matrix_in_place(projector_matrix)
        self.assertTrue(computed_rank,rank)
#@-node:gcross.20091116175016.1815:orthogonalize_matrix_in_place
#@+node:gcross.20100513214001.1748:compute_orthogonal_basis
class compute_orthogonal_basis(unittest.TestCase):
    @with_checker
    def test_shape(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        _, basis = vmps.compute_orthogonal_basis(m,vectors)
        self.assertEqual(basis.shape,(m,m))

    @with_checker
    def test_orthogonality(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        _, basis = vmps.compute_orthogonal_basis(m,vectors)
        self.assertTrue(allclose(dot(basis.transpose().conj(),basis),identity(m)))

    @with_checker
    def test_rank(self,m=irange(8,20),n=irange(1,7)):
        rank = randint(1,n)
        vectors = array(dot(crand(m,rank),crand(rank,n)),order='Fortran')
        computed_rank, _ = vmps.compute_orthogonal_basis(m,vectors)
        self.assertEqual(computed_rank,rank)

    @with_checker
    def test_projector_subspace(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        _, basis = vmps.compute_orthogonal_basis(n,vectors)
        self.assertTrue(allclose(dot(basis[:,:],dot(basis[:,:].conj().transpose(),vectors)),vectors))

    @with_checker
    def test_remaining_subspace(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        _, basis = vmps.compute_orthogonal_basis(m,vectors)
        self.assertAlmostEqual(norm(dot(basis[:,n:].conj().transpose(),vectors)),0)
#@-node:gcross.20100513214001.1748:compute_orthogonal_basis
#@+node:gcross.20100517000234.1792:compute_orthogonal_basis
class compute_orthogonal_subspace(unittest.TestCase):
    @with_checker
    def test_shape(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        basis = vmps.compute_orthogonal_subspace(vectors)
        self.assertEqual(basis.shape,(m,n-m))

    @with_checker
    def test_orthogonality(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        basis = vmps.compute_orthogonal_subspace(vectors)
        self.assertTrue(allclose(dot(basis.transpose().conj(),basis),identity(n-m)))

    @with_checker
    def test_subspace(self,m=irange(8,20),n=irange(1,7)):
        vectors = array(crand(m,n),order='Fortran')
        _, basis = vmps.compute_orthogonal_subspace(vectors)
        self.assertAlmostEqual(norm(dot(basis.conj().transpose(),vectors)),0)
#@-node:gcross.20100517000234.1792:compute_orthogonal_basis
#@+node:gcross.20100514235202.1745:lapack_eigenvalue_minimizer
class lapack_eigenvalue_minimizer(unittest.TestCase):
    @with_checker
    def test_correctness(self,n=irange(1,10)):
        matrix = crand(n,n)
        matrix += matrix.transpose().conj()
        correct_eigenvalues, correct_eigenvectors = eigh(matrix)
        observed_eigenvalue, observed_eigenvector = vmps.lapack_eigenvalue_minimizer(matrix)
        correct_minimal_eigenvalue = correct_eigenvalues[0]
        self.assertAlmostEqual(observed_eigenvalue,correct_minimal_eigenvalue)
        correct_minimal_eigenvector = correct_eigenvectors[:,0]
        correct_minimal_eigenvector /= correct_minimal_eigenvector[0]
        observed_eigenvector /= observed_eigenvector[0]
        self.assertTrue(allclose(observed_eigenvector,correct_minimal_eigenvector))
#@-node:gcross.20100514235202.1745:lapack_eigenvalue_minimizer
#@+node:gcross.20100517160929.1762:project
class project(unittest.TestCase):
    @with_checker
    def test_correctness(self,n=irange(2,10)):
        projector_matrix, _ = qr(crand(n,randint(1,n-1)),overwrite_a=True,econ=False,mode='qr')
        projector_rank = rank(projector_matrix)
        projectors = projector_matrix[:,:projector_rank]
        self.assertTrue(allclose(dot(projectors.transpose().conj(),projectors),identity(projector_rank)))
        vector = crand(n)
        projected_vector = vmps.project(projectors,vector)
        self.assertAlmostEqual(norm(dot(projectors.transpose().conj(),projected_vector)),0)
#@-node:gcross.20100517160929.1762:project
#@-node:gcross.20100514235202.1744:Utility Functions
#@+node:gcross.20091110205054.1948:Normalization
#@+node:gcross.20091110205054.1933:norm_denorm_going_left
class norm_denorm_going_left(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def testCorrectness(self,bll=irange(2,4),bl=irange(2,4),br=irange(2,4)):
        dl = d = 2
        tensor_to_denormalize = crand(bl,bll,d)
        tensor_to_normalize = crand(br,bl,d)
        info, denormalized_tensor, normalized_tensor = vmps.norm_denorm_going_left(tensor_to_denormalize,tensor_to_normalize)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((0,2,),)*2)
        self.assertTrue(allclose(identity(bl),should_be_identity))
        self.assertTrue(allclose(
            tensordot(tensor_to_denormalize,tensor_to_normalize,(0,1)),
            tensordot(denormalized_tensor,normalized_tensor,(0,1)),
        ))

    @with_checker
    def testCorrectnessOnLarge(self):
        br = 10
        bl = 20
        bll = 40
        dl = d = 2
        tensor_to_denormalize = crand(bl,bll,d)
        tensor_to_normalize = crand(br,bl,d)
        info, denormalized_tensor, normalized_tensor = vmps.norm_denorm_going_left(tensor_to_denormalize,tensor_to_normalize)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((0,2,),)*2)
        self.assertTrue(allclose(identity(bl),should_be_identity))
        self.assertTrue(allclose(
            tensordot(tensor_to_denormalize,tensor_to_normalize,(0,1)),
            tensordot(denormalized_tensor,normalized_tensor,(0,1)),
        ))
#@-node:gcross.20091110205054.1933:norm_denorm_going_left
#@+node:gcross.20091110205054.1937:norm_denorm_going_right
class norm_denorm_going_right(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def testCorrectness(self,brr=irange(2,4),br=irange(2,4),bl=irange(2,4)):
        dr = d = 2
        tensor_to_normalize = crand(br,bl,d)
        tensor_to_denormalize = crand(brr,br,dr)
        info, normalized_tensor, denormalized_tensor = vmps.norm_denorm_going_right(tensor_to_normalize,tensor_to_denormalize)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((1,2,),)*2)
        self.assertTrue(allclose(identity(br),should_be_identity))
        self.assertTrue(allclose(
            tensordot(tensor_to_normalize,tensor_to_denormalize,(0,1)),
            tensordot(normalized_tensor,denormalized_tensor,(0,1)),
        ))

    @with_checker
    def testCorrectnessOnLarge(self):
        bl = 10
        br = 20
        brr = 40
        dr = d = 2
        tensor_to_normalize = crand(br,bl,d)
        tensor_to_denormalize = crand(brr,br,dr)
        info, normalized_tensor, denormalized_tensor = vmps.norm_denorm_going_right(tensor_to_normalize,tensor_to_denormalize)
        self.assertEqual(0,info)
        should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,((1,2,),)*2)
        self.assertTrue(allclose(identity(br),should_be_identity))
        self.assertTrue(allclose(
            tensordot(tensor_to_normalize,tensor_to_denormalize,(0,1)),
            tensordot(normalized_tensor,denormalized_tensor,(0,1)),
        ))
#@-node:gcross.20091110205054.1937:norm_denorm_going_right
#@-node:gcross.20091110205054.1948:Normalization
#@+node:gcross.20091115094257.1724:Bandwidth increase
#@+node:gcross.20091115094257.1714:create_bandwidth_increase_matrix
class create_bandwidth_increase_matrix(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def testCorrectness(self,old_bandwidth=irange(2,8),new_bandwidth=irange(9,32)):
        matrix = vmps.create_bandwidth_increase_matrix(old_bandwidth,new_bandwidth)
        self.assertEqual((new_bandwidth,old_bandwidth),matrix.shape)
        should_be_identity = dot(matrix.transpose().conj(),matrix)
        self.assertTrue(allclose(identity(old_bandwidth),should_be_identity))
#@-node:gcross.20091115094257.1714:create_bandwidth_increase_matrix
#@+node:gcross.20091115094257.1723:absorb_bi_matrix_from_left
class absorb_bi_matrix_from_left(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def testCorrectness(self,br=irange(1,4),bl=irange(2,8),new_bl=irange(9,32),d=irange(2,4)):
        old_state_site_tensor = crand(br,bl,d)
        matrix = vmps.create_bandwidth_increase_matrix(bl,new_bl)
        new_state_site_tensor = vmps.absorb_bi_matrix_from_left(old_state_site_tensor,matrix)
        self.assertTrue(allclose(tensordot(old_state_site_tensor,matrix.transpose().conj(),(1,0)).transpose(0,2,1),new_state_site_tensor))
        new_state_site_tensor = vmps.absorb_bi_matrix_from_left(new_state_site_tensor,matrix.conj().transpose())
        self.assertTrue(allclose(old_state_site_tensor,new_state_site_tensor))
#@-node:gcross.20091115094257.1723:absorb_bi_matrix_from_left
#@+node:gcross.20091115094257.1719:absorb_bi_matrix_from_right
class absorb_bi_matrix_from_right(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def testCorrectness(self,br=irange(2,8),new_br=irange(9,32),bl=irange(1,4),d=irange(2,4)):
        old_state_site_tensor = crand(br,bl,d)
        matrix = vmps.create_bandwidth_increase_matrix(br,new_br)
        new_state_site_tensor = vmps.absorb_bi_matrix_from_right(old_state_site_tensor,matrix)
        self.assertTrue(allclose(tensordot(matrix,old_state_site_tensor,(1,0)),new_state_site_tensor))
        new_state_site_tensor = vmps.absorb_bi_matrix_from_right(new_state_site_tensor,matrix.transpose().conj())
        self.assertTrue(allclose(old_state_site_tensor,new_state_site_tensor))
#@-node:gcross.20091115094257.1719:absorb_bi_matrix_from_right
#@+node:gcross.20091115094257.1725:increase_bandwidth_between
class increase_bandwidth_between(unittest.TestCase):
    #@    @+others
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
        info, new_left_tensor, new_right_tensor = vmps.increase_bandwidth_between(new_bm,old_left_tensor,old_right_tensor)
        self.assertEqual(0,info)
        should_be_identity = tensordot(new_right_tensor.conj(),new_right_tensor,((0,2,),)*2)
        self.assertTrue(allclose(identity(new_bm),should_be_identity))
        self.assertTrue(allclose(
            tensordot(old_left_tensor,old_right_tensor,(0,1)),
            tensordot(new_left_tensor,new_right_tensor,(0,1)),
        ))
#@-node:gcross.20091115094257.1725:increase_bandwidth_between
#@-node:gcross.20091115094257.1724:Bandwidth increase
#@+node:gcross.20091118141720.1805:Overlap tensor formation
#@+node:gcross.20091118141720.1807:form_overlap_site_tensor
class form_overlap_site_tensor(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def test_correctness(self,br=irange(2,4),bl=irange(2,4),d=irange(2,4)):
        state_site_tensor = crand(br,bl,d)
        overlap_site_tensor = vmps.form_overlap_site_tensor(state_site_tensor)
        self.assertTrue(allclose(state_site_tensor.transpose(1,2,0),overlap_site_tensor))
#@-node:gcross.20091118141720.1807:form_overlap_site_tensor
#@+node:gcross.20091118141720.1809:form_norm_overlap_tensors
class form_norm_overlap_tensors(unittest.TestCase):
    #@    @+others
    #@-others

    @with_checker
    def test_correctness(self,br=irange(2,4),bm=irange(2,4),bl=irange(2,4),dl=irange(2,4),dr=irange(2,4)):
        unnormalized_state_tensor_1 = crand(bm,bl,dl)
        right_norm_state_tensor_2 = crand(br,bm,dr)
        resulting_tensors = vmps.form_norm_overlap_tensors(unnormalized_state_tensor_1,right_norm_state_tensor_2)
        left_norm_overlap_tensor_1 = resulting_tensors[0]
        unnormalized_overlap_tensor_1 = resulting_tensors[1]
        unnormalized_state_tensor_2 = resulting_tensors[2]
        right_norm_overlap_tensor_2 = resulting_tensors[3]
        self.assertTrue(allclose(right_norm_state_tensor_2,right_norm_overlap_tensor_2.transpose(2,0,1)))
        self.assertTrue(allclose(unnormalized_state_tensor_1,unnormalized_overlap_tensor_1.transpose(2,0,1)))
        left_norm_state_tensor_1 = left_norm_overlap_tensor_1.transpose(2,0,1)
        self.assertTrue(allclose(
            tensordot(unnormalized_state_tensor_1,right_norm_state_tensor_2,(0,1)),
            tensordot(left_norm_state_tensor_1,unnormalized_state_tensor_2,(0,1)),
        ))
#@-node:gcross.20091118141720.1809:form_norm_overlap_tensors
#@-node:gcross.20091118141720.1805:Overlap tensor formation
#@-node:gcross.20091110205054.1947:Tests
#@-others

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
    contract_ss_left,
    contract_ss_right,
    form_overlap_vector,
    compute_expectation,
    compute_optimization_matrix,
    optimize,
    optimize_strategy_1,
    optimize_strategy_2,
    optimize_strategy_3,
    rand_norm_state_site_tensor,
    rand_unnorm_state_site_tensor,
    random_projector_matrix,
    norm_denorm_going_left,
    norm_denorm_going_right,
    create_bandwidth_increase_matrix,
    absorb_bi_matrix_from_left,
    absorb_bi_matrix_from_right,
    increase_bandwidth_between,
    orthogonalize_matrix_in_place,
    compute_orthogonal_basis,
    lapack_eigenvalue_minimizer,
    project,
    form_overlap_site_tensor,
    form_norm_overlap_tensors,
]

#@<< Runner >>
#@+node:gcross.20091106154604.1981:<< Runner >>
unittest.TextTestRunner(verbosity=2).run(unittest.TestSuite(map(unittest.defaultTestLoader.loadTestsFromTestCase, tests)))
#@-node:gcross.20091106154604.1981:<< Runner >>
#@nl
#@-node:gcross.20091106154604.1979:@thin test.py
#@-leo
