#@+leo-ver=4-thin
#@+node:gcross.20090930124443.1265:@thin utils.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20090930134608.1304:<< Import needed modules >>
import __builtin__
from scipy.linalg import svd, eigh, LinAlgError
from scipy.sparse.linalg.eigen.arpack import eigen
from scipy.sparse.linalg import aslinearoperator
from numpy import prod, dot, tensordot, multiply, sqrt, identity, complex128, argsort, real, isfinite, inner
from numpy.random import rand
from itertools import izip, islice
#@-node:gcross.20090930134608.1304:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20090930124443.1249:Macro building functions
#@+at
# These routines build macros to perform tensor contractions.  They do this
# by construction a string of Python code which performs the contraction,
# and then compiling the code into a function.
#@-at
#@@c

#@+others
#@+node:gcross.20090930124443.1250:n2l
#@+at
# Utility function converting numbers to letters.
#@-at
#@@c
n2l = map(chr,range(ord('A'),ord('Z')+1))
#@-node:gcross.20090930124443.1250:n2l
#@+node:gcross.20090930124443.1251:make_contractor
def make_contractor(tensor_index_labels,index_join_pairs,result_index_labels,name="f"):    # pre-process parameters
    tensor_index_labels = list(map(list,tensor_index_labels))
    index_join_pairs = list(index_join_pairs)
    result_index_labels = list([list(index_group) if hasattr(index_group,"__getitem__") else [index_group] for index_group in result_index_labels])

    assert sum(len(index_group) for index_group in tensor_index_labels) == (sum(len(index_group) for index_group in result_index_labels)+2*len(index_join_pairs))

    function_definition_statements = ["def %s(%s):" % (name,",".join(n2l[:len(tensor_index_labels)]))]

    #@    << def build_statements >>
    #@+node:gcross.20090930124443.1252:<< def build_statements >>
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
            #@+node:gcross.20090930124443.1253:<< Search for all joins between these tensors >>
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

            #@-node:gcross.20090930124443.1253:<< Search for all joins between these tensors >>
            #@nl

            #@        << Build tensor contraction statements >>
            #@+node:gcross.20090930124443.1254:<< Build tensor contraction statements >>
            tensor_vars = [n2l[id] for id in tensor_ids]

            statements = [
                "try:",
                "   %s = tensordot(%s,%s,%s)" % (tensor_vars[0],tensor_vars[0],tensor_vars[1],indices),
                "   del %s" % tensor_vars[1],
                "except ValueError:",
                "   raise ValueError('indices %%s do not match for tensor %%i, shape %%s, and tensor %%i, shape %%s.' %% (%s,%i,%s.shape,%i,%s.shape))" % (indices,tensor_ids[0],tensor_vars[0],tensor_ids[1],tensor_vars[1])
            ]
            #@-node:gcross.20090930124443.1254:<< Build tensor contraction statements >>
            #@nl

            #@        << Delete joins from list and update tensor specifications >>
            #@+node:gcross.20090930124443.1255:<< Delete joins from list and update tensor specifications >>
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
            #@-node:gcross.20090930124443.1255:<< Delete joins from list and update tensor specifications >>
            #@nl

            return statements + build_statements(tensor_index_labels,index_join_pairs,result_index_labels)
    #@-node:gcross.20090930124443.1252:<< def build_statements >>
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
#@-node:gcross.20090930124443.1251:make_contractor
#@+node:gcross.20090930124443.1256:make_contractor_from_implicit_joins
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
#@-node:gcross.20090930124443.1256:make_contractor_from_implicit_joins
#@-others
#@-node:gcross.20090930124443.1249:Macro building functions
#@+node:gcross.20091020183902.1478:Exceptions
#@+node:gcross.20091020183902.1479:class ConvergenceError
class ConvergenceError(Exception):
    pass
#@-node:gcross.20091020183902.1479:class ConvergenceError
#@-node:gcross.20091020183902.1478:Exceptions
#@+node:gcross.20090930134608.1299:Utility functions
#@+node:gcross.20090930134608.1301:crand
def crand(*shape):
    return rand(*shape)*2-1+rand(*shape)*2j-1j
#@-node:gcross.20090930134608.1301:crand
#@+node:gcross.20090930134608.1298:multiply_tensor_by_matrix_at_index
def multiply_tensor_by_matrix_at_index(tensor,matrix,index):
    tensor_new_indices = range(tensor.ndim-1)
    tensor_new_indices.insert(index,tensor.ndim-1)
    return tensordot(tensor,matrix,(index,0)).transpose(tensor_new_indices)
#@nonl
#@-node:gcross.20090930134608.1298:multiply_tensor_by_matrix_at_index
#@+node:gcross.20091002125713.1388:conjugate_if_not_None
def conjugate_if_not_None(x):
    try:
        return x.conj()
    except AttributeError:
        if x is None:
            return x
        else:
            raise
#@-node:gcross.20091002125713.1388:conjugate_if_not_None
#@+node:gcross.20091002125713.1390:conjugate_lists_where_not_None
def conjugate_lists_where_not_None(lists):
    return map(lambda list: map(conjugate_if_not_None,list),lists)
#@-node:gcross.20091002125713.1390:conjugate_lists_where_not_None
#@+node:gcross.20091009120817.1408:compute_bandwidth_dimension_sequence
def compute_bandwidth_dimension_sequence(physical_dimension,bandwidth_dimension,number_of_sites):
    bandwidth_dimension_sequence = [1]
    next_dimension = physical_dimension
    while next_dimension < bandwidth_dimension:
        bandwidth_dimension_sequence.append(next_dimension)
        next_dimension *= physical_dimension

    if len(bandwidth_dimension_sequence)*2 > number_of_sites:
        raise ValueError, "The supplied bandwidth dimension is too large for the given physical dimension and number of sites."

    return bandwidth_dimension_sequence + [bandwidth_dimension]*(number_of_sites-2*len(bandwidth_dimension_sequence)+1) + bandwidth_dimension_sequence[::-1]
#@-node:gcross.20091009120817.1408:compute_bandwidth_dimension_sequence
#@+node:gcross.20091001102811.4044:create_normalized_state_site_tensors
def create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites):
    bandwidth_dimension_sequence = compute_bandwidth_dimension_sequence(physical_dimension,bandwidth_dimension,number_of_sites)

    bandwidth_dimension_iterator = izip(bandwidth_dimension_sequence[:-1],bandwidth_dimension_sequence[1:])

    first_left_bandwidth_dimension, first_right_bandwidth_dimension = bandwidth_dimension_iterator.next()

    return [crand(physical_dimension,first_left_bandwidth_dimension,first_right_bandwidth_dimension)] + \
           [normalize(crand(physical_dimension,left_bandwidth_dimension,right_bandwidth_dimension),1)
                for (left_bandwidth_dimension,right_bandwidth_dimension) in bandwidth_dimension_iterator]
#@-node:gcross.20091001102811.4044:create_normalized_state_site_tensors
#@+node:gcross.20091012135649.1409:convert_old_state_tensors_to_orthogonal_state_information
def convert_old_state_tensors_to_orthogonal_state_information(old_state_site_tensors,active_site_number=0):
    return conjugate_lists_where_not_None(compute_all_normalized_tensors(old_state_site_tensors,active_site_number))
#@-node:gcross.20091012135649.1409:convert_old_state_tensors_to_orthogonal_state_information
#@+node:gcross.20091020183902.1473:compute_optimized_state_site_tensor
def compute_optimized_vector(matvec_,guess,iteration_cap=None,tol=0,energy_raise_threshold=1e-10,ncv=None,sigma_solve=None,which='SR'):
    n = len(guess)
    k = 1
    class matrix(object):
        shape = (n,n)
        dtype = complex128
        @staticmethod
        def matvec(state_site_vector):
            return matvec_(state_site_vector)
    try:
        eigenvalues, eigenvectors = \
            eigen(
                matrix,1,
                which=which,
                ncv=ncv,
                v0=guess,
                maxiter=iteration_cap,
                tol=tol,
                return_eigenvectors=True
            )
    except ValueError, msg:
        raise ConvergenceError, "Problem converging to solution in eigenvalue solver: "+str(msg)

    eigenvalue = eigenvalues[0]
    eigenvector = eigenvectors[:,0]
    del eigenvectors

#@+at
# It's not enough to simply take the eigenvector corresponding to the lowest 
# eigenvalue as our solution, since sometimes for various reasons (due mainly 
# to numerical instability) some or all of the eigenvalues are bogus.  For 
# example, sometimes the algorithm returns th the equivalent of $-\infty$, 
# which generally corresponds to an eigenvector that is very nearly zero.  
# Thus, it is important to filter out all of these bogus solutions before 
# declaring victory.
# 
# A solution $\lambda,\vx$ is acceptable iff:
# 
# \begin{itemize}
# \item $\|\vx\|_\infty > 1\cdot 10^{-10}$, to rule out erroneous ``nearly 
# zero'' solutions that result in infinitely large eigenvalues
# \item $\abs{\text{Im}[\lambda]} < 1\cdot 10^{-10}$, since energies should be 
# \emph{real} as the Hamiltonian is hermitian.  (Small imaginary parts aren't 
# great, but they don't hurt too much as long as they are sufficiently 
# negligible.)
# \item $\abs{x_i}<\infty, \abs{\lambda} < \infty$
# \item $\abs{\frac{\coip{\vx}{\bD}{\vx}}{\coip{\vx}{\bN}{\vx}} - \lam}<1\cdot 
# 10^{-10}$, since sometimes the returned eigenvalue is actually much 
# different from the energy obtained by using the corresponding eigenvector
# \end{itemize}
#@-at
#@@c

    if max(abs(eigenvector))<1e-10:
        raise ConvergenceError, "Unable to find an eigenvector."

    if not isfinite(eigenvalue):
        raise ConvergenceError, "Eigenvalue is infinite."

    if not isfinite(eigenvector).all():
        raise ConvergenceError, "Eigenvector is infinite."

#@+at
# If we've reached this point, then we have at least one solution that isn't 
# atrocious.  However, it's possible that our solution has a higher energy 
# than our current state -- either due to a problem in the eigenvalue solver, 
# or because we filtered out all the lower-energy solutions.  Thus, we need 
# compare the energy of the solution we've found to our old energy to make 
# sure that it's higher.
#@-at
#@@c

    return eigenvector, real(eigenvalue)
#@-node:gcross.20091020183902.1473:compute_optimized_state_site_tensor
#@+node:gcross.20091020183902.1518:peak_bandwidth_of
def peak_bandwidth_of(state_site_tensors):
    return state_site_tensors[len(state_site_tensors)/2].shape[2]
#@-node:gcross.20091020183902.1518:peak_bandwidth_of
#@-node:gcross.20090930134608.1299:Utility functions
#@+node:gcross.20091020183902.1519:Classes
#@+node:gcross.20091020183902.1528:TensorChainState
class TensorChainState(object):
    #@    @+others
    #@+node:gcross.20091020183902.1529:__slots__
    __slots__ = ["number_of_sites","active_site_number","tensors_left_of_active_site","active_site_tensor","tensors_right_of_active_site"]
    #@-node:gcross.20091020183902.1529:__slots__
    #@+node:gcross.20091020183902.1530:__init__
    def __init__(self,tensors_left_of_active_site,active_site_tensor,tensors_right_of_active_site):
        self.number_of_sites = len(tensors_left_of_active_site)+1+len(tensors_right_of_active_site)
        self.active_site_number = len(tensors_left_of_active_site)
        self.tensors_left_of_active_site = tensors_left_of_active_site
        self.active_site_tensor = active_site_tensor
        self.tensors_right_of_active_site = list(reversed(tensors_right_of_active_site))
    #@-node:gcross.20091020183902.1530:__init__
    #@+node:gcross.20091020183902.1531:from_list
    @classmethod
    def from_list(cls,list_of_tensors):
        return cls([],list_of_tensors[0],list_of_tensors[1:])
    #@-node:gcross.20091020183902.1531:from_list
    #@+node:gcross.20091020183902.1532:to_list
    def to_list(self):
        return self.tensors_left_of_active_site + [self.active_site_tensor] + list(reversed(self.tensors_right_of_active_site))
    #@-node:gcross.20091020183902.1532:to_list
    #@+node:gcross.20091020183902.1533:normalize_active_site_in_preparation_for_moving_XXX
    def normalize_active_site_in_preparation_for_moving_left(self):
        self.active_site_tensor, self.tensors_left_of_active_site[-1] = \
            normalize_and_denormalize(self.active_site_tensor,1,self.tensors_left_of_active_site[-1],2)

    def normalize_active_site_in_preparation_for_moving_right(self):
        self.active_site_tensor, self.tensors_right_of_active_site[-1] = \
            normalize_and_denormalize(self.active_site_tensor,2,self.tensors_right_of_active_site[-1],1)
    #@-node:gcross.20091020183902.1533:normalize_active_site_in_preparation_for_moving_XXX
    #@+node:gcross.20091020183902.1534:move_active_site_XXX
    def move_active_site_left(self):
        self.tensors_right_of_active_site.append(self.active_site_tensor)
        self.active_site_tensor = self.tensors_left_of_active_site.pop()

    def move_active_site_right(self):
        self.tensors_left_of_active_site.append(self.active_site_tensor)
        self.active_site_tensor = self.tensors_right_of_active_site.pop()
    #@-node:gcross.20091020183902.1534:move_active_site_XXX
    #@+node:gcross.20091020183902.1535:to_tensor
    def to_tensor(self):
        return reduce(dot,self.to_list()).squeeze()
    #@-node:gcross.20091020183902.1535:to_tensor
    #@-others
#@-node:gcross.20091020183902.1528:TensorChainState
#@-node:gcross.20091020183902.1519:Classes
#@+node:gcross.20091001102811.1311:Normalization
#@+node:gcross.20090930134608.1296:normalize
def normalize(matrix,index):
    new_indices = range(matrix.ndim)
    del new_indices[index]
    new_indices.append(index)

    old_shape = list(matrix.shape)
    del old_shape[index]
    new_shape = (prod(old_shape),matrix.shape[index])
    old_shape.append(matrix.shape[index])

    new_matrix = matrix.transpose(new_indices).reshape(new_shape)
    if new_matrix.shape[1] > new_matrix.shape[0]:
        raise ValueError, "There are not enough degrees of freedom available to normalize the tensor."

    old_indices = range(matrix.ndim-1)
    old_indices.insert(index,matrix.ndim-1)

    try:
        u, s, v = svd(new_matrix,full_matrices=0)
        return dot(u,v).reshape(old_shape).transpose(old_indices)
    except LinAlgError:
        M = dot(new_matrix.conj().transpose(),new_matrix)

        vals, U = eigh(M)
        vals[vals<0] = 0

        dvals = sqrt(vals)
        nonzero_dvals = dvals!=0
        dvals[nonzero_dvals] = 1.0/dvals[nonzero_dvals]
        X = dot(U*dvals,U.conj().transpose())

        return dot(new_matrix,X).reshape(old_shape).transpose(old_indices)

#@-node:gcross.20090930134608.1296:normalize
#@+node:gcross.20091001102811.4021:normalize_and_return_inverse_normalizer
def normalize_and_return_inverse_normalizer(matrix,index):
    new_indices = range(matrix.ndim)
    del new_indices[index]
    new_indices.append(index)

    old_shape = list(matrix.shape)
    del old_shape[index]
    new_shape = (prod(old_shape),matrix.shape[index])
    old_shape.append(matrix.shape[index])

    new_matrix = matrix.transpose(new_indices).reshape(new_shape)

    old_indices = range(matrix.ndim-1)
    old_indices.insert(index,matrix.ndim-1)

    try:
        u, s, v = svd(new_matrix,full_matrices=0)    
        return dot(u,v).reshape(old_shape).transpose(old_indices), dot(v.transpose().conj()*s,v)
    except LinAlgError:
        M = dot(new_matrix.conj().transpose(),new_matrix)

        vals, U = eigh(M)
        vals[vals<0] = 0

        dvals = sqrt(vals)
        nonzero_dvals = dvals!=0
        dvals[nonzero_dvals] = 1.0/dvals[nonzero_dvals]

        return dot(new_matrix,dot(U*dvals,U.conj().transpose())).reshape(old_shape).transpose(old_indices), dot(U*vals,U.conj().transpose())
#@-node:gcross.20091001102811.4021:normalize_and_return_inverse_normalizer
#@+node:gcross.20091001102811.4003:compute_normalizer_and_inverse
def compute_normalizer_and_inverse(matrix,index):
    new_indices = range(matrix.ndim)
    del new_indices[index]
    new_indices.append(index)

    size_of_normalization_dimension = matrix.shape[index]

    old_shape = list(matrix.shape)
    del old_shape[index]
    new_shape = (prod(old_shape),size_of_normalization_dimension)
    old_shape.append(size_of_normalization_dimension)

    new_matrix = matrix.transpose(new_indices).reshape(new_shape)

    old_indices = range(matrix.ndim-1)
    old_indices.insert(index,matrix.ndim-1)

    try:
        u, s, v = svd(new_matrix,full_matrices=0)
        return dot(v.transpose().conj()*(1/s),v), dot(v.transpose().conj()*s,v)
    except LinAlgError:
        M = dot(new_matrix.conj().transpose(),new_matrix)

        vals, U = eigh(M)
        vals[vals<0] = 0

        dvals = sqrt(vals)
        nonzero_dvals = dvals!=0
        dvals[nonzero_dvals] = 1.0/dvals[nonzero_dvals]

        return dot(U*dvals,U.conj().transpose()), dot(U*vals,U.conj().transpose())
#@-node:gcross.20091001102811.4003:compute_normalizer_and_inverse
#@+node:gcross.20091002195631.1353:compute_normalized_tensors
def compute_normalized_tensors(unnormalized_tensor,normalized_tensors,index_to_normalize):
    index_to_unnormalize = 3-index_to_normalize
    unnormalized_tensors = []
    oppositely_normalized_tensors = []

    for normalized_tensor in normalized_tensors:
        oppositely_normalized_tensor, unnormalized_tensor = normalize_and_denormalize(unnormalized_tensor,index_to_normalize,normalized_tensor,index_to_unnormalize)
        oppositely_normalized_tensors.append(oppositely_normalized_tensor)
        unnormalized_tensors.append(unnormalized_tensor)

    return oppositely_normalized_tensors, unnormalized_tensors
#@-node:gcross.20091002195631.1353:compute_normalized_tensors
#@+node:gcross.20091001102811.4005:compute_all_normalized_tensors
def compute_all_normalized_tensors(state_site_tensors,active_site_number=0):
    pivot_tensor = state_site_tensors[active_site_number]

    state_site_tensors_normalized_for_right_contraction, unnormalized_site_tensors = compute_normalized_tensors(pivot_tensor,reversed(state_site_tensors[:active_site_number]),1)
    state_site_tensors_normalized_for_right_contraction.append(None)
    state_site_tensors_normalized_for_right_contraction.reverse()
    state_site_tensors_normalized_for_right_contraction += state_site_tensors[active_site_number+1:]
    unnormalized_site_tensors.reverse()
    unnormalized_site_tensors.append(pivot_tensor)

    state_site_tensors_normalized_for_left_contraction = state_site_tensors[:active_site_number]
    state_site_tensors_normalized_for_left_contraction[len(state_site_tensors_normalized_for_left_contraction):], unnormalized_site_tensors[len(unnormalized_site_tensors):] = compute_normalized_tensors(pivot_tensor,state_site_tensors[active_site_number+1:],2)
    state_site_tensors_normalized_for_left_contraction.append(None)

    return unnormalized_site_tensors, state_site_tensors_normalized_for_left_contraction, state_site_tensors_normalized_for_right_contraction
#@-node:gcross.20091001102811.4005:compute_all_normalized_tensors
#@+node:gcross.20091004090150.1354:normalize_and_denormalize
def normalize_and_denormalize(tensor_to_normalize,index_to_normalize,tensor_to_denormalize,index_to_denormalize):
    normalized_tensor, inverse_normalizer = normalize_and_return_inverse_normalizer(tensor_to_normalize,index_to_normalize)
    unnormalized_tensor = multiply_tensor_by_matrix_at_index(tensor_to_denormalize,inverse_normalizer.transpose(),index_to_denormalize)
    return normalized_tensor, unnormalized_tensor
#@-node:gcross.20091004090150.1354:normalize_and_denormalize
#@-node:gcross.20091001102811.1311:Normalization
#@+node:gcross.20090930124443.1275:Unit Tests
if __name__ == '__main__':
    import unittest
    from numpy import allclose, inner
    from numpy.random import randint, rand
    from paycheck import *
    #@    @+others
    #@+node:gcross.20090930124443.1276:make_contractor_tests
    class make_contractor_tests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20090930124443.1277:testIdentity
        def testIdentity(self):
            arr = rand(3,4,5)
            self.assertTrue(allclose(arr,make_contractor([[0,1,2]],[],[[0],[1],[2]])(arr),rtol=1e-10))
            self.assertTrue(allclose(arr.ravel(),make_contractor([[0,1,2]],[],[[0,1,2]])(arr),rtol=1e-10))
            self.assertTrue(allclose(arr.transpose(2,0,1).ravel(),make_contractor([[0,1,2]],[],[[2,0,1]])(arr),rtol=1e-10))
            self.assertTrue(allclose(arr.transpose(2,0,1).reshape(15,4),make_contractor([[0,1,2]],[],[[2,0],[1]])(arr),rtol=1e-10))
        #@nonl
        #@-node:gcross.20090930124443.1277:testIdentity
        #@+node:gcross.20090930124443.1278:testInnerProduct
        def testInnerProduct(self):
            A = rand(3,5)
            B = rand(5,3)
            MP = make_contractor([[0,1],[2,3]],[[1,2]],[0,3])
            self.assertTrue(allclose(dot(A,B),MP(A,B),rtol=1e-10))
            self.assertTrue(allclose(dot(B,A),MP(B,A),rtol=1e-10))
            TP = make_contractor([[0,1],[2,3]],[[1,2],[3,0]],[])
            self.assertTrue(allclose(inner(A.transpose().ravel(),B.ravel()),TP(A,B),rtol=1e-10))
        #@nonl
        #@-node:gcross.20090930124443.1278:testInnerProduct
        #@+node:gcross.20090930124443.1279:testImplicitJoins
        def testImplicitJoins(self):
            arr = rand(3,4,5)
            self.assertTrue(allclose(arr,make_contractor_from_implicit_joins([[0,1,2]],[[0],[1],[2]])(arr),rtol=1e-10))
            self.assertTrue(allclose(arr.ravel(),make_contractor_from_implicit_joins([[0,1,2]],[[0,1,2]])(arr),rtol=1e-10))
            self.assertTrue(allclose(arr.transpose(2,0,1).ravel(),make_contractor_from_implicit_joins([[0,1,2]],[[2,0,1]])(arr),rtol=1e-10))
            self.assertTrue(allclose(arr.transpose(2,0,1).reshape(15,4),make_contractor_from_implicit_joins([[0,1,2]],[[2,0],[1]])(arr),rtol=1e-10))

            A = rand(3,5)
            B = rand(5,3)
            MP = make_contractor_from_implicit_joins([[0,1],[1,3]],[[0],[3]])
            self.assertTrue(allclose(dot(A,B),MP(A,B),rtol=1e-10))
            self.assertTrue(allclose(dot(B,A),MP(B,A),rtol=1e-10))
            TP = make_contractor_from_implicit_joins([[0,1],[1,0]],[])
            self.assertTrue(allclose(inner(A.transpose().ravel(),B.ravel()),TP(A,B),rtol=1e-10))

            arr = rand(3,4,5)
            self.assertTrue(allclose(arr*5,make_contractor_from_implicit_joins([[0,1,2],[]],[[0],[1],[2]])(arr,5),rtol=1e-10))
            self.assertTrue(allclose(arr*5*6,make_contractor_from_implicit_joins([[0,1,2],[],[]],[[0],[1],[2]])(arr,5,6),rtol=1e-10))
            self.assertTrue(allclose(multiply.outer(arr,arr),make_contractor_from_implicit_joins([[0,1,2],[3,4,5]],[[i] for i in xrange(6)])(arr,arr),rtol=1e-10))
            self.assertTrue(allclose(reduce(multiply.outer,[arr,]*3),make_contractor_from_implicit_joins([[0,1,2],[3,4,5],[6,7,8]],[[i] for i in xrange(9)])(arr,arr,arr),rtol=1e-10))
            A = rand(3,4,5)
            B = rand(3,4,5)
            C = rand(3,4,5)
            self.assertTrue(allclose(reduce(multiply.outer,[A,B,C]),make_contractor_from_implicit_joins([[0,1,2],[3,4,5],[6,7,8]],[[i] for i in xrange(9)])(A,B,C),rtol=1e-10))
        #@nonl
        #@-node:gcross.20090930124443.1279:testImplicitJoins
        #@+node:gcross.20090930124443.1280:testOuterProduct
        def testOuterProduct(self):
            arr = rand(3,4,5)
            self.assertTrue(allclose(arr*5,make_contractor([[0,1,2],[]],[],range(3))(arr,5),rtol=1e-10))
            self.assertTrue(allclose(arr*5*6,make_contractor([[0,1,2],[],[]],[],range(3))(arr,5,6),rtol=1e-10))
            self.assertTrue(allclose(multiply.outer(arr,arr),make_contractor([[0,1,2],[3,4,5]],[],range(6))(arr,arr),rtol=1e-10))
            self.assertTrue(allclose(reduce(multiply.outer,[arr,]*3),make_contractor([[0,1,2],[3,4,5],[6,7,8]],[],range(9))(arr,arr,arr),rtol=1e-10))
            A = rand(3,4,5)
            B = rand(3,4,5)
            C = rand(3,4,5)
            self.assertTrue(allclose(reduce(multiply.outer,[A,B,C]),make_contractor([[0,1,2],[3,4,5],[6,7,8]],[],[[i] for i in xrange(9)])(A,B,C),rtol=1e-10))
        #@nonl
        #@-node:gcross.20090930124443.1280:testOuterProduct
        #@-others
    #@nonl
    #@-node:gcross.20090930124443.1276:make_contractor_tests
    #@+node:gcross.20090930134608.1303:normalize_tests
    class normalize_tests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20090930134608.1305:testCorrectness
        @with_checker
        def testCorrectness(self,number_of_dimensions=irange(2,4),size=irange(2,5)):
            index_to_normalize = randint(0,number_of_dimensions-1)
            normalized_tensor = normalize(crand(*(size,)*number_of_dimensions),index_to_normalize)
            indices_to_sum_over = range(number_of_dimensions)
            del indices_to_sum_over[index_to_normalize]
            should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,(indices_to_sum_over,)*2)
            self.assertTrue(allclose(identity(size),should_be_identity))
        #@-node:gcross.20090930134608.1305:testCorrectness
        #@+node:gcross.20091001172447.1336:testCorrectnessOnRandomShape
        @with_checker
        def testCorrectnessOnRandomShape(self,number_of_dimensions=irange(2,6)):
            index_to_normalize = randint(0,number_of_dimensions-1)

            shape = [randint(1,5) for _ in xrange(number_of_dimensions)]
            try:
                normalized_tensor = normalize(crand(*shape),index_to_normalize)
            except ValueError:
                self.assertTrue(prod(shape[:index_to_normalize])*prod(shape[index_to_normalize+1:]) < shape[index_to_normalize])
                return

            indices_to_sum_over = range(number_of_dimensions)
            del indices_to_sum_over[index_to_normalize]
            should_be_identity = tensordot(normalized_tensor.conj(),normalized_tensor,(indices_to_sum_over,)*2)
            self.assertTrue(allclose(identity(normalized_tensor.shape[index_to_normalize]),should_be_identity))
        #@nonl
        #@-node:gcross.20091001172447.1336:testCorrectnessOnRandomShape
        #@+node:gcross.20091001102811.4004:testMethodAgreement
        @with_checker
        def testMethodAgreement(self,number_of_dimensions=irange(2,4),size=irange(2,5)):
            index_to_normalize = randint(0,number_of_dimensions-1)
            tensor = crand(*(size,)*number_of_dimensions)
            normalized_tensor_1 = normalize(tensor,index_to_normalize)
            normalized_tensor_2 = multiply_tensor_by_matrix_at_index(tensor,compute_normalizer_and_inverse(tensor,index_to_normalize)[0],index_to_normalize)
            self.assertTrue(allclose(normalized_tensor_1,normalized_tensor_2))
        #@-node:gcross.20091001102811.4004:testMethodAgreement
        #@-others
    #@-node:gcross.20090930134608.1303:normalize_tests
    #@+node:gcross.20091001102811.4017:compute_normalizer_and_inverse_tests
    class compute_normalizer_and_inverse_tests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091001102811.4019:testInverse
        @with_checker
        def testInverse(self,number_of_dimensions=irange(2,4),size=irange(2,5)):
            index_to_normalize = randint(0,number_of_dimensions-1)
            tensor = crand(*(size,)*number_of_dimensions)
            normalizer, inverse_normalizer = compute_normalizer_and_inverse(tensor,index_to_normalize)
            self.assertTrue(allclose(identity(size),dot(normalizer,inverse_normalizer)))
        #@-node:gcross.20091001102811.4019:testInverse
        #@+node:gcross.20091002195631.1355:testInverseCancelsNormalizer
        @with_checker
        def testInverseCancelsNormalizer(self,number_of_dimensions=irange(2,4),size=irange(2,5)):
            index_to_normalize = randint(0,number_of_dimensions-1)
            tensor = crand(*(size,)*number_of_dimensions)
            normalizer, inverse_normalizer = compute_normalizer_and_inverse(tensor,index_to_normalize)
            self.assertTrue(allclose(tensor,multiply_tensor_by_matrix_at_index(multiply_tensor_by_matrix_at_index(tensor,normalizer,index_to_normalize),inverse_normalizer,index_to_normalize)))
        #@-node:gcross.20091002195631.1355:testInverseCancelsNormalizer
        #@-others
    #@-node:gcross.20091001102811.4017:compute_normalizer_and_inverse_tests
    #@+node:gcross.20091001102811.4024:normalize_and_return_inverse_normalizer_tests
    class normalize_and_return_inverse_normalizer_tests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091001102811.4027:testMethodAgreement
        @with_checker
        def testMethodAgreement(self,number_of_dimensions=irange(2,4),size=irange(2,5)):
            index_to_normalize = randint(0,number_of_dimensions-1)
            tensor = crand(*(size,)*number_of_dimensions)
            normalized_tensor_1 = normalize(tensor,index_to_normalize)
            normalizer, inverse_normalizer_1 = compute_normalizer_and_inverse(tensor,index_to_normalize)
            normalized_tensor_2, inverse_normalizer_2 = normalize_and_return_inverse_normalizer(tensor,index_to_normalize)
            self.assertTrue(allclose(normalized_tensor_1,normalized_tensor_2))
            self.assertTrue(allclose(inverse_normalizer_1,inverse_normalizer_1))
        #@-node:gcross.20091001102811.4027:testMethodAgreement
        #@-others
    #@-node:gcross.20091001102811.4024:normalize_and_return_inverse_normalizer_tests
    #@+node:gcross.20091002195631.1356:compute_normalized_tensors
    class compute_normalized_tensors_test(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091002195631.1358:testCorrectnessGoingRight
        @with_checker
        def testCorrectnessGoingRight(self,bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            state_site_tensors = create_normalized_state_site_tensors(2,bandwidth_dimension,number_of_sites)
            unnormalized_state_site_tensors = [state_site_tensors[0]]
            normalized_state_site_tensors = state_site_tensors[1:]

            oppositely_normalized_state_site_tensors, unnormalized_state_site_tensors[1:] = compute_normalized_tensors(state_site_tensors[0],normalized_state_site_tensors,2)

            correct_state_tensor = reduce(dot,state_site_tensors).squeeze()

            for i in xrange(number_of_sites):
                state_tensor = reduce(dot,oppositely_normalized_state_site_tensors[:i]+[unnormalized_state_site_tensors[i]]+normalized_state_site_tensors[i:]).squeeze()
                self.assertTrue(allclose(correct_state_tensor,state_tensor))
        #@-node:gcross.20091002195631.1358:testCorrectnessGoingRight
        #@+node:gcross.20091002195631.1360:testCorrectnessGoingLeft
        @with_checker
        def testCorrectnessGoingLeft(self,bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            state_site_tensors = create_normalized_state_site_tensors(2,bandwidth_dimension,number_of_sites)
            state_site_tensors.reverse()
            unnormalized_state_site_tensors = [state_site_tensors[0]]
            normalized_state_site_tensors = state_site_tensors[1:]

            oppositely_normalized_state_site_tensors, unnormalized_state_site_tensors[1:] = compute_normalized_tensors(state_site_tensors[0],normalized_state_site_tensors,1)

            correct_state_tensor = reduce(dot,reversed(state_site_tensors)).squeeze()

            for i in xrange(number_of_sites):
                state_tensor = reduce(dot,reversed(oppositely_normalized_state_site_tensors[:i]+[unnormalized_state_site_tensors[i]]+normalized_state_site_tensors[i:])).squeeze()
                self.assertTrue(allclose(correct_state_tensor,state_tensor))
        #@-node:gcross.20091002195631.1360:testCorrectnessGoingLeft
        #@-others
    #@-node:gcross.20091002195631.1356:compute_normalized_tensors
    #@+node:gcross.20091001102811.4028:compute_all_normalized_tensors_test
    class compute_all_normalized_tensors_test(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091001102811.4033:testCorrectnessGoingRight
        @with_checker
        def testCorrectnessGoingRight(self,bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            state_site_tensors = create_normalized_state_site_tensors(2,bandwidth_dimension,number_of_sites)
            correct_state_tensor = reduce(dot,state_site_tensors).squeeze()

            unnormalized_state_site_tensors, state_site_tensors_normalized_for_left_contraction, state_site_tensors_normalized_for_right_contraction = compute_all_normalized_tensors(state_site_tensors)

            self.assertEqual(len(state_site_tensors),len(unnormalized_state_site_tensors))
            self.assertEqual(len(state_site_tensors),len(state_site_tensors_normalized_for_left_contraction))
            self.assertEqual(len(state_site_tensors),len(state_site_tensors_normalized_for_right_contraction))

            self.assertTrue((unnormalized_state_site_tensors[0]==state_site_tensors[0]).all())

            for i in xrange(number_of_sites):
                self.assertEqual(state_site_tensors[i].shape,unnormalized_state_site_tensors[i].shape)

            for i in xrange(number_of_sites-1):
                self.assertEqual(state_site_tensors[i].shape,state_site_tensors_normalized_for_left_contraction[i].shape)

            for i in xrange(1,number_of_sites):
                self.assertEqual(state_site_tensors[i].shape,state_site_tensors_normalized_for_right_contraction[i].shape)

            for i in xrange(number_of_sites):
                state_tensor = reduce(dot,state_site_tensors_normalized_for_left_contraction[:i]+[unnormalized_state_site_tensors[i]]+state_site_tensors_normalized_for_right_contraction[i+1:]).squeeze()
                self.assertTrue(allclose(correct_state_tensor,state_tensor))
        #@-node:gcross.20091001102811.4033:testCorrectnessGoingRight
        #@+node:gcross.20091002195631.1362:testCorrectnessGoingLeft
        @with_checker
        def testCorrectnessGoingLeft(self,bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            state_site_tensors = create_normalized_state_site_tensors(2,bandwidth_dimension,number_of_sites)
            correct_state_tensor = reduce(dot,state_site_tensors).squeeze()

            unnormalized_state_site_tensors, state_site_tensors_normalized_for_left_contraction, state_site_tensors_normalized_for_right_contraction = compute_all_normalized_tensors(state_site_tensors,number_of_sites-1)

            self.assertEqual(len(state_site_tensors),len(unnormalized_state_site_tensors))
            self.assertEqual(len(state_site_tensors),len(state_site_tensors_normalized_for_left_contraction))
            self.assertEqual(len(state_site_tensors),len(state_site_tensors_normalized_for_right_contraction))

            self.assertTrue((unnormalized_state_site_tensors[-1]==state_site_tensors[-1]).all())

            for i in xrange(number_of_sites):
                self.assertEqual(state_site_tensors[i].shape,unnormalized_state_site_tensors[i].shape)

            for i in xrange(number_of_sites-1):
                self.assertEqual(state_site_tensors[i].shape,state_site_tensors_normalized_for_left_contraction[i].shape)

            for i in xrange(1,number_of_sites):
                self.assertEqual(state_site_tensors[i].shape,state_site_tensors_normalized_for_right_contraction[i].shape)

            for i in xrange(number_of_sites):
                state_tensor = reduce(dot,state_site_tensors_normalized_for_left_contraction[:i]+[unnormalized_state_site_tensors[i]]+state_site_tensors_normalized_for_right_contraction[i+1:]).squeeze()
                self.assertTrue(allclose(correct_state_tensor,state_tensor))
        #@-node:gcross.20091002195631.1362:testCorrectnessGoingLeft
        #@+node:gcross.20091002195631.1364:testCorrectnessGoingBothWays
        @with_checker
        def testCorrectnessGoingBothWays(self,bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            active_site_number = randint(0,number_of_sites-1)
            state_site_tensors = create_normalized_state_site_tensors(2,bandwidth_dimension,number_of_sites)
            for i in xrange(active_site_number):
                state_site_tensors[i] = normalize(state_site_tensors[i],2)
            for i in xrange(active_site_number+1,number_of_sites):
                state_site_tensors[i] = normalize(state_site_tensors[i],1)
            correct_state_tensor = reduce(dot,state_site_tensors).squeeze()

            unnormalized_state_site_tensors, state_site_tensors_normalized_for_left_contraction, state_site_tensors_normalized_for_right_contraction = compute_all_normalized_tensors(state_site_tensors,active_site_number)

            self.assertEqual(len(state_site_tensors),len(unnormalized_state_site_tensors))
            self.assertEqual(len(state_site_tensors),len(state_site_tensors_normalized_for_left_contraction))
            self.assertEqual(len(state_site_tensors),len(state_site_tensors_normalized_for_right_contraction))

            self.assertTrue((unnormalized_state_site_tensors[active_site_number]==state_site_tensors[active_site_number]).all())

            for i in xrange(number_of_sites):
                self.assertEqual(state_site_tensors[i].shape,unnormalized_state_site_tensors[i].shape)

            for i in xrange(number_of_sites-1):
                self.assertEqual(state_site_tensors[i].shape,state_site_tensors_normalized_for_left_contraction[i].shape)

            for i in xrange(1,number_of_sites):
                self.assertEqual(state_site_tensors[i].shape,state_site_tensors_normalized_for_right_contraction[i].shape)

            for i in xrange(number_of_sites):
                state_tensor = reduce(dot,state_site_tensors_normalized_for_left_contraction[:i]+[unnormalized_state_site_tensors[i]]+state_site_tensors_normalized_for_right_contraction[i+1:]).squeeze()
                self.assertTrue(allclose(correct_state_tensor,state_tensor))
        #@-node:gcross.20091002195631.1364:testCorrectnessGoingBothWays
        #@-others
    #@-node:gcross.20091001102811.4028:compute_all_normalized_tensors_test
    #@+node:gcross.20091001172447.1333:create_normalized_state_site_tensors_tests
    class create_normalized_state_site_tensors_tests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091001172447.1334:testNormalization
        @with_checker
        def testNormalization(self,physical_dimension=irange(2,5),bandwidth_dimension=irange(2,4),number_of_sites=irange(4,6)):
            state_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)
            for state_site_tensor in state_site_tensors[1:]:
                self.assertTrue(allclose(identity(state_site_tensor.shape[1]),tensordot(state_site_tensor.conj(),state_site_tensor,((0,2),(0,2)))))
        #@-node:gcross.20091001172447.1334:testNormalization
        #@-others
    #@-node:gcross.20091001172447.1333:create_normalized_state_site_tensors_tests
    #@+node:gcross.20091004090150.1355:normalize_and_denormalize_tests
    class normalize_and_denormalize_tests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091004090150.1356:testEquivalence
        @with_checker
        def testEquivalence(self,number_of_dimensions=irange(2,4),size=irange(2,5)):
            index_to_normalize = randint(0,number_of_dimensions-1)
            index_to_denormalize = randint(0,number_of_dimensions-1)
            tensor_A, tensor_B = crand(2,*(size,)*number_of_dimensions)
            normalized_tensor_A_1, inverse_normalizer = normalize_and_return_inverse_normalizer(tensor_A,index_to_normalize)
            unnormalized_tensor_B_1 = multiply_tensor_by_matrix_at_index(tensor_B,inverse_normalizer.transpose(),index_to_denormalize)
            normalized_tensor_A_2, unnormalized_tensor_B_2 = normalize_and_denormalize(tensor_A,index_to_normalize,tensor_B,index_to_denormalize)
            self.assertTrue(allclose(normalized_tensor_A_1,normalized_tensor_A_2))
            self.assertTrue(allclose(unnormalized_tensor_B_1,unnormalized_tensor_B_2))
        #@-node:gcross.20091004090150.1356:testEquivalence
        #@-others
    #@-node:gcross.20091004090150.1355:normalize_and_denormalize_tests
    #@+node:gcross.20091009120817.1409:compute_bandwidth_dimension_sequence_tests
    class compute_bandwidth_dimension_sequence_tests(unittest.TestCase):
        #@    @+others
        #@+node:gcross.20091009120817.1410:testValidity
        @with_checker
        def testValidity(self,physical_dimension=irange(2,4),bandwidth_dimension=irange(1,8),number_of_sites=irange(10,20)):
            bandwidth_dimension_sequence = compute_bandwidth_dimension_sequence(physical_dimension,bandwidth_dimension,number_of_sites)
            for a, b in zip(bandwidth_dimension_sequence[:-1],bandwidth_dimension_sequence[1:]):
                self.assertTrue(b <= a * physical_dimension)
        #@-node:gcross.20091009120817.1410:testValidity
        #@+node:gcross.20091009120817.1412:testException
        @with_checker
        def testException(self,physical_dimension=irange(2,4),number_of_sites=irange(4,10)):
            bandwidth_dimension = physical_dimension**number_of_sites

            try:
                bandwidth_dimension_sequence = compute_bandwidth_dimension_sequence(physical_dimension,bandwidth_dimension,number_of_sites)
            except ValueError:
                return

            self.fail("Did not throw exepction in response to invalid input!")
        #@-node:gcross.20091009120817.1412:testException
        #@-others
    #@-node:gcross.20091009120817.1409:compute_bandwidth_dimension_sequence_tests
    #@-others
    unittest.main()
#@-node:gcross.20090930124443.1275:Unit Tests
#@-others
#@-node:gcross.20090930124443.1265:@thin utils.py
#@-leo
