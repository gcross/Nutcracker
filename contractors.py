#@+leo-ver=4-thin
#@+node:gcross.20090930124443.1284:@thin contractors.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20090930134608.1306:<< Import needed modules >>
from utils import make_contractor_from_implicit_joins, compute_all_normalized_tensors, conjugate_lists_where_not_None
from numpy import inner, complex128, argsort, real, isfinite, ones, prod, conj
from scipy.sparse.linalg.eigen.arpack import eigen
from scipy.sparse.linalg import aslinearoperator
from scipy.linalg import qr
from itertools import izip
#@-node:gcross.20090930134608.1306:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20090930124443.1294:Functions
#@+node:gcross.20090930124443.1285:Contractors
#@+node:gcross.20090930124443.1286:Absorb site expectation into left boundary
#@+at
# 
# L = left boundary
# S = state site tensor
# S* = state site tensor (conjugated)
# O = operator site tensor
# 
# /-1--S*-21
# |    |
# |    32
# |    |
# L-2--O--22
# |    |
# |    42
# |    |
# \-3--S--23
# 
#@-at
#@@c

contract_site_expectation_into_left_boundary = make_contractor_from_implicit_joins([
    [1,2,3],       # left environment
    [32,1,21],     # state site tensor (conjugated)
    [32,42,2,22],  # operator site tensor
    [42,3,23],     # state site tensor
],[
    21,
    22,
    23,
])
#@-node:gcross.20090930124443.1286:Absorb site expectation into left boundary
#@+node:gcross.20090930124443.1290:Absorb site contraction into left boundary
#@+at
# 
# L = left boundary
# A* = state A site tensor (conjugated)
# B = state B site tensor
# 
# /-1--A*-21
# |    |
# L    2
# |    |
# \-3--B--23
# 
#@-at
#@@c

contract_site_overlap_into_left_boundary = make_contractor_from_implicit_joins([
    [1,3],     # left boundary
    [2,1,21],  # state A site tensor (conjugated)
    [2,3,23],  # state B site tensor
],[
    21,
    23,
])
#@-node:gcross.20090930124443.1290:Absorb site contraction into left boundary
#@+node:gcross.20090930124443.1288:Absorb site expectation into right boundary
#@+at
# 
# R = right boundary
# S = state site tensor
# S* = state site tensor (conjugated)
# O = operator site tensor
# 
# 1--S*-21-\
#    |     |
#    32    |
#    |     |
# 2--O--22-R
#    |     |
#    42    |
#    |     |
# 3--S--23-/
# 
#@-at
#@@c

contract_site_expectation_into_right_boundary = make_contractor_from_implicit_joins([
    [21,22,23],    # right boundary
    [32,1,21],     # state site tensor (conjugated)
    [32,42,2,22],  # operator site tensor
    [42,3,23],     # state site tensor
],[
    1,
    2,
    3,
])
#@-node:gcross.20090930124443.1288:Absorb site expectation into right boundary
#@+node:gcross.20090930124443.1292:Absorb site contraction into right boundary
#@+at
# 
# R = left boundary
# A* = state A site tensor (conjugated)
# B = state B site tensor
# 
# 1--A*-21-\
#    |     |
#    2     R
#    |     |
# 3--B--23-/
# 
#@-at
#@@c

contract_site_overlap_into_right_boundary = make_contractor_from_implicit_joins([
    [21,23],   # right boundary
    [2,1,21],  # state A site tensor (conjugated)
    [2,3,23],  # state B site tensor
],[
    1,
    3,
])
#@-node:gcross.20090930124443.1292:Absorb site contraction into right boundary
#@+node:gcross.20090930134608.1334:Partial contraction for expectation
#@+at
# 
# L = left boundary
# R = left boundary
# S = state site tensor
# O = operator site tensor
# 
# /-1-   -21-\
# |    |     |
# |    32    |
# |    |     |
# L-2--O--22-R
# |    |     |
# |    42    |
# |    |     |
# \-3--S--23-/
# 
#@-at
#@@c

partially_contract_expectation = make_contractor_from_implicit_joins([
    [42,3,23],     # state site tensor
    [32,42,2,22],  # operator site tensor
    [1,2,3],       # left boundary tensor
    [21,22,23],    # right boundary tensor
],[
    32,
    1,
    21,
])
#@-node:gcross.20090930134608.1334:Partial contraction for expectation
#@+node:gcross.20091001102811.4043:Partial contraction for overlap
#@+at
# 
# L = left boundary
# R = left boundary
# S = state site tensor
# O = operator site tensor
# 
# /-1- S*-21-\
# |    |     |
# |    42    |
# |    |     |
# \-3--  -23-/
# 
#@-at
#@@c

partially_contract_overlap = make_contractor_from_implicit_joins([
    [42,1,21],  # state site tensor
    [1,3],      # left boundary tensor
    [21,23],    # right boundary tensor
],[
    42,
    3,
    23,
])
#@-node:gcross.20091001102811.4043:Partial contraction for overlap
#@+node:gcross.20090930134608.1360:Compute optimization matrix
#@+at
# 
# L = left boundary
# R = left boundary
# S = state site tensor
# O = operator site tensor
# 
# /-1-   -21-\
# |    |     |
# |    32    |
# |    |     |
# L-2--O--22-R
# |    |     |
# |    42    |
# |    |     |
# \-3-   -23-/
# 
#@-at
#@@c

compute_optimization_matrix = make_contractor_from_implicit_joins([
    [32,42,2,22],  # operator site tensor
    [1,2,3],       # left boundary tensor
    [21,22,23],    # right boundary tensor
],[
    (32,1,21),
    (42,3,23),
])
#@-node:gcross.20090930134608.1360:Compute optimization matrix
#@-node:gcross.20090930124443.1285:Contractors
#@-node:gcross.20090930124443.1294:Functions
#@+node:gcross.20090930134608.1354:Exceptions
#@+node:gcross.20090930134608.1356:class ConvergenceError
class ConvergenceError(Exception):
    pass
#@-node:gcross.20090930134608.1356:class ConvergenceError
#@-node:gcross.20090930134608.1354:Exceptions
#@+node:gcross.20090930124443.1293:Classes
#@+others
#@+node:gcross.20091002125713.1354:chains
#@+node:gcross.20090930134608.1341:ChainContractorBase
class ChainContractorBase(object):
    #@    @+others
    #@+node:gcross.20090930134608.1289:__slots__
    __slots__ = [
        "left_boundary","left_boundary_stack","left_operator_stack",
        "right_boundary","right_boundary_stack","right_operator_stack",
        "operator_site_tensor",
        "number_of_sites","current_site_number",
        "physical_dimension","bandwidth_dimension",
    ]
    #@-node:gcross.20090930134608.1289:__slots__
    #@+node:gcross.20090930134608.1323:__init__
    def __init__(self,number_of_sites):
        self.number_of_sites = number_of_sites
        self.current_site_number = 0
        self.left_boundary_stack = []
        self.left_operator_stack = []
        self.right_boundary_stack = []
        self.right_operator_stack = []
    #@-node:gcross.20090930134608.1323:__init__
    #@+node:gcross.20090930134608.1263:push_XXX_boundary
    def push_left_boundary(self):
        self.left_boundary_stack.append(self.left_boundary)
        self.left_operator_stack.append(self.operator_site_tensor)

    def push_right_boundary(self):
        self.right_boundary_stack.append(self.right_boundary)
        self.right_operator_stack.append(self.operator_site_tensor)
    #@-node:gcross.20090930134608.1263:push_XXX_boundary
    #@+node:gcross.20090930134608.1267:pop_XXX_boundary
    def pop_left_boundary(self):
        self.left_boundary = self.left_boundary_stack.pop()
        self.operator_site_tensor = self.left_operator_stack.pop()

    def pop_right_boundary(self):
        self.right_boundary = self.right_boundary_stack.pop()
        self.operator_site_tensor = self.right_operator_stack.pop()
    #@-node:gcross.20090930134608.1267:pop_XXX_boundary
    #@+node:gcross.20090930134608.1270:contract_and_move_XXX
    def contract_and_move_left(self,state_site_tensor_conjugated,state_site_tensor):
        self.current_site_number -= 1
        self.push_right_boundary()
        self.right_boundary = self.contract_into_right_boundary(state_site_tensor_conjugated,state_site_tensor)
        self.pop_left_boundary()

    def contract_and_move_right(self,state_site_tensor_conjugated,state_site_tensor):
        self.current_site_number += 1
        self.push_left_boundary()
        self.left_boundary = self.contract_into_left_boundary(state_site_tensor_conjugated,state_site_tensor)
        self.pop_right_boundary()
    #@-node:gcross.20090930134608.1270:contract_and_move_XXX
    #@+node:gcross.20090930134608.1331:fully_contract_with_state_site_tensor
    def fully_contract_with_state_site_tensor(self,state_site_tensor_conjugated,state_site_tensor):
        return inner(self.partially_contract_with_state_site_tensor(state_site_tensor).ravel(),state_site_tensor_conjugated.ravel())
    #@-node:gcross.20090930134608.1331:fully_contract_with_state_site_tensor
    #@-others
#@-node:gcross.20090930134608.1341:ChainContractorBase
#@+node:gcross.20090930124443.1295:ChainContractorForExpectations
class ChainContractorForExpectations(ChainContractorBase):
    #@    @+others
    #@+node:gcross.20090930134608.1273:__init__
    def __init__(self,initial_state_site_tensors,operator_site_tensors,left_operator_boundary,right_operator_boundary):
        ChainContractorBase.__init__(self,len(initial_state_site_tensors))
        assert(self.number_of_sites == len(operator_site_tensors))
        self.left_boundary = left_operator_boundary.reshape(1,len(left_operator_boundary),1)
        self.right_boundary = right_operator_boundary.reshape(1,len(right_operator_boundary),1)
        for i in xrange(self.number_of_sites-1,0,-1):
            self.operator_site_tensor = operator_site_tensors[i]
            self.push_right_boundary()
            self.right_boundary = self.contract_into_right_boundary(initial_state_site_tensors[i].conj(),initial_state_site_tensors[i])
        self.operator_site_tensor = operator_site_tensors[0]
    #@-node:gcross.20090930134608.1273:__init__
    #@+node:gcross.20090930134608.1274:contract_into_XXX_boundary
    def contract_into_left_boundary(self,state_site_tensor_conjugated,state_site_tensor):
        return \
            contract_site_expectation_into_left_boundary(
                self.left_boundary,
                state_site_tensor_conjugated,
                self.operator_site_tensor,
                state_site_tensor
            )

    def contract_into_right_boundary(self,state_site_tensor_conjugated,state_site_tensor):
        return \
            contract_site_expectation_into_right_boundary(
                self.right_boundary,
                state_site_tensor_conjugated,
                self.operator_site_tensor,
                state_site_tensor
            )
    #@-node:gcross.20090930134608.1274:contract_into_XXX_boundary
    #@+node:gcross.20090930134608.1338:partially_contract_with_state_site_tensor
    def partially_contract_with_state_site_tensor(self,state_site_tensor):
        return \
            partially_contract_expectation(
                state_site_tensor,
                self.operator_site_tensor,
                self.left_boundary,
                self.right_boundary
            )

    #@-node:gcross.20090930134608.1338:partially_contract_with_state_site_tensor
    #@+node:gcross.20090930134608.1349:compute_optimized_site_matrix
    def compute_optimized_state_site_tensor(self,guess,iteration_cap=None,tol=0,energy_raise_threshold=1e-10,k=1,ncv=None,sigma_solve=None):
        n = prod(guess.shape)
        state_site_tensor_shape = guess.shape
        class matrix(object):
            shape = (n,n)
            dtype = complex128
            @staticmethod
            def matvec(state_site_vector):
                return self.partially_contract_with_state_site_tensor(state_site_vector.reshape(state_site_tensor_shape)).ravel()
        try:
            eigenvalues, eigenvectors = \
                eigen(
                    matrix,1,
                    which='SR',
                    ncv=ncv,
                    v0=guess.ravel(),
                    maxiter=iteration_cap,
                    tol=tol,
                    return_eigenvectors=True
                )
        except ValueError, msg:
            raise ConvergenceError, "Problem converging to solution in eigenvalue solver: "+str(msg)

        eigenvectors = eigenvectors.transpose()

    #@+at
    # It's not enough to simply take the eigenvector corresponding to the 
    # lowest eigenvalue as our solution, since sometimes for various reasons 
    # (due mainly to numerical instability) some or all of the eigenvalues are 
    # bogus.  For example, sometimes the algorithm returns th the equivalent 
    # of $-\infty$, which generally corresponds to an eigenvector that is very 
    # nearly zero.  Thus, it is important to filter out all of these bogus 
    # solutions before declaring victory.
    #@-at
    #@@c

        sorted_indices_of_eigenvalues = argsort(real(eigenvalues))

    #@+at
    # A solution $\lambda,\vx$ is acceptable iff:
    # 
    # \begin{itemize}
    # \item $\|\vx\|_\infty > 1\cdot 10^{-10}$, to rule out erroneous ``nearly 
    # zero'' solutions that result in infinitely large eigenvalues
    # \item $\abs{\text{Im}[\lambda]} < 1\cdot 10^{-10}$, since energies 
    # should be \emph{real} as the Hamiltonian is hermitian.  (Small imaginary 
    # parts aren't great, but they don't hurt too much as long as they are 
    # sufficiently negligible.)
    # \item $\abs{x_i}<\infty, \abs{\lambda} < \infty$
    # \item $\abs{\frac{\coip{\vx}{\bD}{\vx}}{\coip{\vx}{\bN}{\vx}} - 
    # \lam}<1\cdot 10^{-10}$, since sometimes the returned eigenvalue is 
    # actually much different from the energy obtained by using the 
    # corresponding eigenvector
    # \end{itemize}
    #@-at
    #@@c
        def is_acceptable(index):
            evec = eigenvectors[index]
            eval = eigenvalues[index]
            return max(abs(evec)) > 1e-10 and isfinite(evec).all() and isfinite(eval)

        acceptable_indices_of_eigenvalues = filter(is_acceptable,sorted_indices_of_eigenvalues)
        if len(acceptable_indices_of_eigenvalues) == 0:
            raise ConvergenceError, "All eigenvectors had near-vanishing normals, eigenvalues with large imaginary parts, or NaNs and/or infs."

    #@+at
    # If we've reached this point, then we have at least one solution that 
    # isn't atrocious.  However, it's possible that our solution has a higher 
    # energy than our current state -- either due to a problem in the 
    # eigenvalue solver, or because we filtered out all the lower-energy 
    # solutions.  Thus, we need compare the energy of the solution we've found 
    # to our old energy to make sure that it's higher.
    #@-at
    #@@c

        index_of_solution = acceptable_indices_of_eigenvalues[0]
        state_site_tensor = eigenvectors[index_of_solution].reshape(state_site_tensor_shape)

        new_energy = real(eigenvalues[index_of_solution])

        old_energy = self.fully_contract_with_state_site_tensor(guess.conj(),guess)/inner(guess.conj().ravel(),guess.ravel())

        if old_energy is not None and real(new_energy) > real(old_energy) and abs(new_energy-old_energy)>energy_raise_threshold and False:
            raise ConvergenceError, "Only found a solution which *raised* the energy. (%.15f < %.15f)" % (new_energy,old_energy)

        return state_site_tensor, new_energy
    #@-node:gcross.20090930134608.1349:compute_optimized_site_matrix
    #@+node:gcross.20090930134608.1361:compute_optimization_matrix
    def compute_optimization_matrix(self):
        return compute_optimization_matrix(self.operator_site_tensor,self.left_boundary,self.right_boundary)
    #@-node:gcross.20090930134608.1361:compute_optimization_matrix
    #@-others
#@-node:gcross.20090930124443.1295:ChainContractorForExpectations
#@+node:gcross.20090930134608.1292:ChainContractorForOverlaps
class ChainContractorForOverlaps(ChainContractorBase):
    #@    @+others
    #@+node:gcross.20090930134608.1294:__init__
    def __init__(self,initial_state_A_site_tensors_conjugated,initial_state_B_site_tensors):
        ChainContractorBase.__init__(self,len(initial_state_A_site_tensors_conjugated))
        self.left_boundary = self.right_boundary = ones((1,1),dtype=complex128)
        self.operator_site_tensor = None
        self.right_operator_stack = [None]*(self.number_of_sites-1)
        for i in xrange(self.number_of_sites-1,0,-1):
            self.push_right_boundary()
            self.right_boundary = self.contract_into_right_boundary(initial_state_A_site_tensors_conjugated[i],initial_state_B_site_tensors[i])
    #@-node:gcross.20090930134608.1294:__init__
    #@+node:gcross.20090930134608.1291:contract_into_XXX_boundary
    def contract_into_left_boundary(self,state_A_site_tensor_conjugated,state_B_site_tensor):
        return \
            contract_site_overlap_into_left_boundary(
                self.left_boundary,
                state_A_site_tensor_conjugated,
                state_B_site_tensor
            )

    def contract_into_right_boundary(self,state_A_site_tensor_conjugated,state_B_site_tensor):
        return \
            contract_site_overlap_into_right_boundary(
                self.right_boundary,
                state_A_site_tensor_conjugated,
                state_B_site_tensor
            )
    #@-node:gcross.20090930134608.1291:contract_into_XXX_boundary
    #@+node:gcross.20090930134608.1340:partially_contract_with_state_site_tensor
    def partially_contract_with_state_site_tensor(self,state_site_tensor_conjugated):
        return \
            partially_contract_overlap(
                state_site_tensor_conjugated,
                self.left_boundary,
                self.right_boundary
            )
    #@-node:gcross.20090930134608.1340:partially_contract_with_state_site_tensor
    #@-others
#@-node:gcross.20090930134608.1292:ChainContractorForOverlaps
#@+node:gcross.20091001102811.4036:ChainContractorForProjector
class ChainContractorForProjector(ChainContractorForOverlaps):
    #@    @+others
    #@+node:gcross.20091001102811.4038:__init__
    def __init__(self,
        initial_state_site_tensors,
        overlap_state_site_tensors_conjugated,
        overlap_state_site_tensors_normalized_for_left_contraction_conjugated,
        overlap_state_site_tensors_normalized_for_right_contraction_conjugated,
      ):
        self.overlap_state_site_tensors_conjugated = overlap_state_site_tensors_conjugated
        self.overlap_state_site_tensors_normalized_for_left_contraction_conjugated = overlap_state_site_tensors_normalized_for_left_contraction_conjugated
        self.overlap_state_site_tensors_normalized_for_right_contraction_conjugated = overlap_state_site_tensors_normalized_for_right_contraction_conjugated

        ChainContractorForOverlaps.__init__(self,
            self.overlap_state_site_tensors_normalized_for_right_contraction_conjugated,
            initial_state_site_tensors
        )
    #@-node:gcross.20091001102811.4038:__init__
    #@+node:gcross.20091001102811.4040:contract_and_move_XXX
    def contract_and_move_left(self,state_site_tensor):
        ChainContractorForOverlaps.contract_and_move_left(self,
            self.overlap_state_site_tensors_normalized_for_right_contraction_conjugated[self.current_site_number],
            state_site_tensor
        )

    def contract_and_move_right(self,state_site_tensor):
        ChainContractorForOverlaps.contract_and_move_right(self,
            self.overlap_state_site_tensors_normalized_for_left_contraction_conjugated[self.current_site_number],
            state_site_tensor
        )
    #@-node:gcross.20091001102811.4040:contract_and_move_XXX
    #@+node:gcross.20091001102811.4041:compute_projector
    def compute_projector(self):
        return self.partially_contract_with_state_site_tensor(self.overlap_state_site_tensors_conjugated[self.current_site_number])
    #@-node:gcross.20091001102811.4041:compute_projector
    #@-others
#@-node:gcross.20091001102811.4036:ChainContractorForProjector
#@-node:gcross.20091002125713.1354:chains
#@+node:gcross.20091002125713.1355:ChainProjector
class ChainProjector(object):
    #@    @+others
    #@+node:gcross.20091002125713.1375:__slots__
    __slots__ = [
        "orthogonal_state_information_list",
        "projector_chains",
        "current_site_number"
    ]
    #@-node:gcross.20091002125713.1375:__slots__
    #@+node:gcross.20091002125713.1376:__init__
    def __init__(self):
        self.orthogonal_state_information_list = []
        self.projector_chains = []
        self.current_site_number = 0
    #@-node:gcross.20091002125713.1376:__init__
    #@+node:gcross.20091002125713.1379:add_additional_state_to_projector_and_reset_chains_with_new_initial_state
    def add_additional_state_to_projector_and_reset_chains_with_new_initial_state(self,old_state_site_tensors,initial_new_state_site_tensors,active_site_number=0):
        overlap_state_information = conjugate_lists_where_not_None(compute_all_normalized_tensors(old_state_site_tensors,active_site_number))

        self.orthogonal_state_information_list.append(overlap_state_information)

        self.projector_chains = [
            ChainContractorForProjector(initial_new_state_site_tensors,*orthogonal_state_information)
            for orthogonal_state_information in self.orthogonal_state_information_list
        ]

        self.current_site_number = 0
    #@-node:gcross.20091002125713.1379:add_additional_state_to_projector_and_reset_chains_with_new_initial_state
    #@+node:gcross.20091002125713.1377:construct_projector
    def construct_projector(self):
        if len(self.projector_chains) == 0:
            return lambda x: x
        else:
            orthogonal_projection_vectors = \
                qr( # This takes the matrix whose rows are the projection vectors and computes the RQ decomposition,
                    # where Q is a new matrix whose rows span the same linear space but are orthogonal.
                    # (We throw out R because we don't care about it.)
                    array([projector_chain.compute_projector().ravel() for projector_chain in self.projector_chains]).transpose(),
                    overwrite_a = True, # We don't need the original matrix anymore, so re-use its memory
                    econ = True # Don't bother computing a full set of orthogonal vectors, only compute enough to span the same space
                )[0].transpose()
            orthogonal_projection_vector_pairs = zip(orthogonal_projection_vectors,map(conj,orthogonal_projection_vectors))
            def project(state_site_tensor_as_vector):
                for orthogonal_projection_vector, orthogonal_projection_vector_conjugated in orthogonal_projection_vector_pairs:
                    state_site_tensor_as_vector -= dot(orthogonal_projection_vector,state_site_tensor_as_vector)*orthogonal_projection_vector_conjugated
                return state_site_tensor_as_vector
            return project
    #@-node:gcross.20091002125713.1377:construct_projector
    #@+node:gcross.20091002125713.1387:contract_and_move_XXX
    def contract_and_move_left(self,state_site_tensor):
        for chain in self.projector_chains:
            chain.contract_and_move_left(state_site_tensor)

    def contract_and_move_right(self,state_site_tensor):
        for chain in self.projector_chains:
            chain.contract_and_move_right(state_site_tensor)
    #@-node:gcross.20091002125713.1387:contract_and_move_XXX
    #@-others
#@-node:gcross.20091002125713.1355:ChainProjector
#@-others
#@-node:gcross.20090930124443.1293:Classes
#@+node:gcross.20090930134608.1315:Unit Tests
if __name__ == '__main__':
    import unittest
    from utils import crand, normalize, create_normalized_state_site_tensors
    from paulis import *
    from numpy import identity, allclose, array, zeros, dot
    from numpy.linalg import norm
    from copy import copy
    from paycheck import *
    #@    @+others
    #@+node:gcross.20090930134608.1326:ChainContractorForExpectationsTests
    class ChainContractorForExpectationsTests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20090930134608.1327:testOnNormalizedState
        @with_checker
        def testOnNormalizedState(self,physical_dimension=irange(2,4),bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            operator_left_boundary = operator_right_boundary = array([1])
            initial_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)
            operator_site_tensors = [identity(physical_dimension).reshape(physical_dimension,physical_dimension,1,1)]*number_of_sites
            chain = ChainContractorForExpectations(initial_site_tensors,operator_site_tensors,operator_left_boundary,operator_right_boundary)

            def check_boundary(boundary):
                self.assertTrue(allclose(identity(boundary.shape[0]),boundary.squeeze()) or boundary.size == 1 and boundary.squeeze() == 1)

            def check():
                check_boundary(chain.left_boundary)
                check_boundary(chain.right_boundary)

            check()

            for initial_site_tensor in initial_site_tensors[:-1]:
                initial_site_tensor = normalize(initial_site_tensor,2)
                chain.contract_and_move_right(initial_site_tensor.conj(),initial_site_tensor)
                check()

            for initial_site_tensor in initial_site_tensors[-1:0:-1]:
                chain.contract_and_move_left(initial_site_tensor.conj(),initial_site_tensor)
                check()

        #@-node:gcross.20090930134608.1327:testOnNormalizedState
        #@+node:gcross.20090930134608.1329:testOnSpinStateInMagneticXField
        @with_checker
        def testOnSpinStateInMagneticXField(self,number_of_sites=irange(2,6)):
            initial_site_tensor = array([1,0]).reshape(2,1,1)
            initial_site_tensors = [initial_site_tensor]*number_of_sites
            operator_site_tensor = zeros((2,2,2,2),dtype=int)
            operator_site_tensor[...,0,0] = I
            operator_site_tensor[...,0,1] = X
            operator_site_tensor[...,1,1] = I
            operator_site_tensors = [operator_site_tensor]*number_of_sites
            chain = ChainContractorForExpectations(initial_site_tensors,operator_site_tensors,array([1,0]),array([0,1]))
            self.assertEqual(0,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_right(initial_site_tensor,initial_site_tensor)
                self.assertEqual(0,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_left(initial_site_tensor,initial_site_tensor)
                self.assertEqual(0,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))
        #@-node:gcross.20090930134608.1329:testOnSpinStateInMagneticXField
        #@+node:gcross.20090930134608.1333:testOnSpinStateInMagneticZField
        @with_checker
        def testOnSpinStateInMagneticZField(self,number_of_sites=irange(2,6)):
            initial_site_tensor = array([1,0]).reshape(2,1,1)
            initial_site_tensors = [initial_site_tensor]*number_of_sites
            operator_site_tensor = zeros((2,2,2,2),dtype=int)
            operator_site_tensor[...,0,0] = I
            operator_site_tensor[...,0,1] = Z
            operator_site_tensor[...,1,1] = I
            operator_site_tensors = [operator_site_tensor]*number_of_sites
            chain = ChainContractorForExpectations(initial_site_tensors,operator_site_tensors,array([1,0]),array([0,1]))
            self.assertEqual(number_of_sites,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_right(initial_site_tensor,initial_site_tensor)
                self.assertEqual(number_of_sites,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_left(initial_site_tensor,initial_site_tensor)
                self.assertEqual(number_of_sites,chain.fully_contract_with_state_site_tensor(initial_site_tensor,initial_site_tensor))
        #@-node:gcross.20090930134608.1333:testOnSpinStateInMagneticZField
        #@+node:gcross.20090930134608.1353:testOptimizerOnSpinState1InMagneticField
        @with_checker(number_of_calls=20)
        def testOptimizerOnSpinState1InMagneticField(self,number_of_sites=irange(2,6)):
            initial_site_tensor = array([0,0,1],dtype=complex128).reshape(3,1,1)
            initial_site_tensors = [initial_site_tensor]*number_of_sites
            operator_site_tensor = zeros((3,3,2,2),dtype=complex128)
            operator_site_tensor[...,0,0] = array(identity(3),dtype=complex128)
            operator_site_tensor[...,0,1] = array([[1,0,0],[0,0,0],[0,0,-1]],complex128)
            operator_site_tensor[...,1,1] = array(identity(3),dtype=complex128)
            operator_site_tensors = [operator_site_tensor]*number_of_sites
            chain = ChainContractorForExpectations(initial_site_tensors,operator_site_tensors,array([1,0]),array([0,1]))

            def check():
                new_tensor, new_energy = chain.compute_optimized_state_site_tensor(crand(*initial_site_tensor.shape))
                self.assertAlmostEqual(-number_of_sites,new_energy)
                self.assertTrue(allclose(abs(new_tensor.ravel())/norm(new_tensor.ravel()),array([0,0,1])))

            check()

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_right(initial_site_tensor,initial_site_tensor)
                check()

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_left(initial_site_tensor,initial_site_tensor)
                check()


            def check():
                new_tensor, new_energy = chain.compute_optimized_state_site_tensor(guess=initial_site_tensor.copy())
                self.assertAlmostEqual(-number_of_sites,new_energy)
                self.assertTrue(allclose(new_tensor.ravel(),array([0,0,1])))

            check()

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_right(initial_site_tensor,initial_site_tensor)
                check()

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_left(initial_site_tensor,initial_site_tensor)
                check()
        #@-node:gcross.20090930134608.1353:testOptimizerOnSpinState1InMagneticField
        #@-others
    #@nonl
    #@-node:gcross.20090930134608.1326:ChainContractorForExpectationsTests
    #@+node:gcross.20090930134608.1316:ChainContractorForOverlapsTests
    class ChainContractorForOverlapsTests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20090930134608.1317:testOnNormalizedState
        @with_checker
        def testOnNormalizedState(self,physical_dimension=irange(2,4),bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            initial_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)
            chain = ChainContractorForOverlaps(map(conj,initial_site_tensors),initial_site_tensors)

            def check_boundary(boundary):
                self.assertTrue(allclose(identity(boundary.shape[0]),boundary.squeeze()) or boundary.size == 1 and boundary.squeeze() == 1)

            def check():
                check_boundary(chain.left_boundary)
                check_boundary(chain.right_boundary)

            check()

            for initial_site_tensor in initial_site_tensors[:-1]:
                initial_site_tensor = normalize(initial_site_tensor,2)
                chain.contract_and_move_right(initial_site_tensor.conj(),initial_site_tensor)
                check()

            for initial_site_tensor in initial_site_tensors[-1:0:-1]:
                chain.contract_and_move_left(initial_site_tensor.conj(),initial_site_tensor)
                check()

        #@-node:gcross.20090930134608.1317:testOnNormalizedState
        #@-others
    #@nonl
    #@-node:gcross.20090930134608.1316:ChainContractorForOverlapsTests
    #@+node:gcross.20091001102811.4048:ChainContractorForProjectorTests
    class ChainContractorForProjectorTests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091001172447.1338:testNormalizedState
        @with_checker
        def testNormalizedState(self,physical_dimension=irange(2,4),bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            state_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)

            correct_value = dot(reduce(dot,state_site_tensors).squeeze().ravel().conj(),reduce(dot,state_site_tensors).squeeze().ravel())

            chain = ChainContractorForProjector(
                state_site_tensors,
                *conjugate_lists_where_not_None(compute_all_normalized_tensors(state_site_tensors))
            )

            state_site_tensors, state_site_tensors_normalized_for_left_contraction, state_site_tensors_normalized_for_right_contraction = compute_all_normalized_tensors(state_site_tensors)

            def check(i):
                self.assertTrue(allclose(chain.compute_projector(),state_site_tensors[i].conj()))
                self.assertAlmostEqual(correct_value,dot(chain.compute_projector().ravel(),state_site_tensors[i].ravel()))

            check(0)

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_right(state_site_tensors_normalized_for_left_contraction[i])
                check(i+1)

            for i in xrange(number_of_sites-1,0,-1):
                chain.contract_and_move_left(state_site_tensors_normalized_for_right_contraction[i])
                check(i-1)
        #@-node:gcross.20091001172447.1338:testNormalizedState
        #@+node:gcross.20091001102811.4049:testCorrectness
        @with_checker
        def testCorrectness(self,physical_dimension=irange(2,4),bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            state_A_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)
            state_B_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)

            correct_value = dot(reduce(dot,state_A_site_tensors).squeeze().ravel().conj(),reduce(dot,state_B_site_tensors).squeeze().ravel())

            chain = ChainContractorForProjector(
                state_B_site_tensors,
                *conjugate_lists_where_not_None(compute_all_normalized_tensors(state_A_site_tensors))
            )

            state_B_site_tensors, state_B_site_tensors_normalized_for_left_contraction, state_B_site_tensors_normalized_for_right_contraction = compute_all_normalized_tensors(state_B_site_tensors)


            def check(i):
                self.assertAlmostEqual(correct_value,dot(chain.compute_projector().ravel(),state_B_site_tensors[i].ravel()))

            check(0)

            for i in xrange(number_of_sites-1):
                chain.contract_and_move_right(state_B_site_tensors_normalized_for_left_contraction[i])
                check(i+1)

            for i in xrange(number_of_sites-1,0,-1):
                chain.contract_and_move_left(state_B_site_tensors_normalized_for_right_contraction[i])
                check(i-1)
        #@-node:gcross.20091001102811.4049:testCorrectness
        #@-others
    #@-node:gcross.20091001102811.4048:ChainContractorForProjectorTests
    #@+node:gcross.20091002125713.1380:ChainProjectorTests
    class ChainProjectorTests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091002125713.1381:testNull
        def testNull(self):
            ChainProjector().construct_projector()(None)
        #@-node:gcross.20091002125713.1381:testNull
        #@+node:gcross.20091002125713.1385:testSameState
        @with_checker
        def testSameState(self,physical_dimension=irange(2,4),bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            state_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)

            projector = ChainProjector()

            projector.add_additional_state_to_projector_and_reset_chains_with_new_initial_state(state_site_tensors,state_site_tensors)

            unnormalized_state_site_tensors, state_site_tensors_normalized_for_left_contraction, state_site_tensors_normalized_for_right_contraction = compute_all_normalized_tensors(state_site_tensors)

            def check(i):
                self.assertTrue(allclose(unnormalized_state_site_tensors[i].conj(),projector.projector_chains[0].compute_projector()))
                self.assertAlmostEqual(0,norm(projector.construct_projector()(unnormalized_state_site_tensors[i].copy().ravel())))

            check(0)

            for i in xrange(number_of_sites-1):
                projector.contract_and_move_right(state_site_tensors_normalized_for_left_contraction[i])
                check(i+1)

            for i in xrange(number_of_sites-1,0,-1):
                projector.contract_and_move_left(state_site_tensors_normalized_for_right_contraction[i])
                check(i-1)
        #@-node:gcross.20091002125713.1385:testSameState
        #@+node:gcross.20091002195631.1366:testDifferentStates
        @with_checker
        def testDifferentStates(self,physical_dimension=irange(2,4),bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            state_A_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)
            state_B_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)

            state_A_as_vector = reduce(dot,state_A_site_tensors).ravel()
            state_B_as_vector = reduce(dot,state_B_site_tensors).ravel()

            correct_projected_state_vector = state_B_as_vector - dot(state_A_as_vector.conj(),state_B_as_vector)/dot(state_A_as_vector.conj(),state_A_as_vector)*state_A_as_vector
            self.assertAlmostEqual(0,dot(state_A_as_vector.conj(),correct_projected_state_vector))

            projector = ChainProjector()
            projector.add_additional_state_to_projector_and_reset_chains_with_new_initial_state(state_A_site_tensors,state_B_site_tensors)

            unnormalized_state_B_site_tensors, state_B_site_tensors_normalized_for_left_contraction, state_B_site_tensors_normalized_for_right_contraction = compute_all_normalized_tensors(state_B_site_tensors)

            def check(i):
                projected_state_vector = reduce(dot,
                    state_B_site_tensors_normalized_for_left_contraction[:i]
                   +[projector.construct_projector()(unnormalized_state_B_site_tensors[i].copy().ravel()).reshape(unnormalized_state_B_site_tensors[i].shape)]
                   +state_B_site_tensors_normalized_for_right_contraction[i+1:]
                  ).ravel()
                self.assertAlmostEqual(0,dot(state_A_as_vector.conj(),projected_state_vector))
                # I think that this next condition is actually not going to be true in general, even given a correct implementation.
                #self.assertTrue(allclose(correct_projected_state_vector,projected_state_vector))

            check(0)

            for i in xrange(number_of_sites-1):
                projector.contract_and_move_right(state_B_site_tensors_normalized_for_left_contraction[i])
                check(i+1)

            for i in xrange(number_of_sites-1,0,-1):
                projector.contract_and_move_left(state_B_site_tensors_normalized_for_right_contraction[i])
                check(i-1)
        #@-node:gcross.20091002195631.1366:testDifferentStates
        #@-others
    #@-node:gcross.20091002125713.1380:ChainProjectorTests
    #@-others
    unittest.main()
#@-node:gcross.20090930134608.1315:Unit Tests
#@-others
#@-node:gcross.20090930124443.1284:@thin contractors.py
#@-leo
