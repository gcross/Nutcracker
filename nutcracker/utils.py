# Imports {{{

from collections import defaultdict, namedtuple
from numpy.random import rand

# }}} Imports

# Exceptions {{{

class DimensionMismatchError(ValueError): # {{{
    def __init__(self,left_tensor_number,left_index,left_dimension,right_tensor_number,right_index,right_dimension): # {{{
        self.left_tensor_number = left_tensor_number
        self.left_index = left_index
        self.left_dimension = left_dimension
        self.right_tensor_number = right_tensor_number
        self.right_index = right_index
        self.right_dimension = right_dimension
        ValueError.__init__(self,"tensor {left_tensor_number}'s index {left_index} has dimension {left_dimension}, whereas tensor {right_tensor_number}'s index {right_index} has dimension {right_dimension}".format(**self.__dict__))
    # }}}
# }}}

class UnexpectedTensorRankError(ValueError): # {{{
    def __init__(self,tensor_number,expected_rank,actual_rank): # {{{
        self.tensor_number = tensor_number
        self.expected_rank = expected_rank
        self.actual_rank = actual_rank
        ValueError.__init__(self,"tensor {tensor_number} was expected to have rank {expected_rank} but actually has rank {actual_rank}".format(**self.__dict__))
    # }}}
# }}}

# }}}

# Classes {{{

class Join: # {{{
    def __init__( # {{{
        self
        ,left_tensor_number
        ,left_tensor_indices
        ,right_tensor_number
        ,right_tensor_indices
    ):
        if not hasattr(left_tensor_indices,"__len__"):
            left_tensor_indices = [left_tensor_indices]
        if not hasattr(right_tensor_indices,"__len__"):
            right_tensor_indices = [right_tensor_indices]
        if len(left_tensor_indices) != len(right_tensor_indices):
            raise ValueError("number of left indices does not match number of right indices (len({}) != len({}))".format(left_tensor_indices,right_tensor_indices))
        self.left_tensor_number = left_tensor_number
        self.left_tensor_indices = left_tensor_indices
        self.right_tensor_number = right_tensor_number
        self.right_tensor_indices = right_tensor_indices
        self.checkOrderAndSwap()
    # }}}
    def __repr__(self): # {{{
        return "Join({left_tensor_number},{left_tensor_indices},{right_tensor_number},{right_tensor_indices})".format(**self.__dict__)
    # }}}
    def checkOrderAndSwap(self): # {{{
        if self.left_tensor_number == self.right_tensor_number:
            raise ValueError(
                ("attempted to create a self-join for tensor {} (indices = {},{})"
                ).format(
                    self.left_tensor_number,
                    self.left_tensor_indices,
                    self.right_tensor_indices
                )
            )
        if self.right_tensor_number < self.left_tensor_number:
            right_tensor_number = self.right_tensor_number
            right_tensor_indices = self.right_tensor_indices
            self.right_tensor_number = self.left_tensor_number
            self.right_tensor_indices = self.left_tensor_indices
            self.left_tensor_number = right_tensor_number
            self.left_tensor_indices = right_tensor_indices
    # }}}
    def mergeWith(self,other): # {{{
        assert self.left_tensor_number == other.left_tensor_number
        assert self.right_tensor_number == other.right_tensor_number
        self.left_tensor_indices  += other.left_tensor_indices
        self.right_tensor_indices += other.right_tensor_indices
    # }}}
    def update(self,old_tensor_number,new_tensor_number,index_map): # {{{
        if self.left_tensor_number == old_tensor_number:
            self.left_tensor_number = new_tensor_number
            self.left_tensor_indices = applyIndexMapTo(index_map,self.left_tensor_indices)
            self.checkOrderAndSwap()
        elif self.right_tensor_number == old_tensor_number:
            self.right_tensor_number = new_tensor_number
            self.right_tensor_indices = applyIndexMapTo(index_map,self.right_tensor_indices)
            self.checkOrderAndSwap()
    # }}}
# }}}

# }}}

# Functions {{{

def applyIndexMapTo(index_map,indices): # {{{
    return [index_map[index] for index in indices]
# }}}

def applyPermutation(permutation,values): # {{{
    return [values[i] for i in permutation]
# }}}

def computeLengthAndCheckForGaps(indices,error_message): # {{{
    if len(indices) == 0:
        return 0
    length = max(indices)+1
    unobserved_indices = set(xrange(length))
    for index in indices:
        unobserved_indices.remove(index)
    if unobserved_indices:
        raise ValueError(error_message + ": " + str(unobserved_indices))
    return length
# }}}

def computePostContractionIndexMap(rank,contracted_indices,offset=0): # {{{
    contracted_indices = set(contracted_indices)
    index_map = dict()
    new_index = 0
    for old_index in xrange(rank):
        if old_index not in contracted_indices:
            index_map[old_index] = new_index + offset
            new_index += 1
    return index_map
# }}}

def crand(*shape): # {{{
    return rand(*shape)*2-1+rand(*shape)*2j-1j
# }}}

def invertPermutation(permutation): # {{{
    return [permutation.index(i) for i in xrange(len(permutation))]
# }}}

def makeDataContractor(joins,final_groups,tensor_ranks=None): # {{{
    # Tabulate all of the tensor indices to compute the number of arguments and their ranks {{{
    observed_tensor_indices = defaultdict(lambda: set())
    observed_joins = set()
    def observeTensorIndices(tensor_number,*indices):
        if tensor_number < 0:
            raise ValueError("tensor numbers must be non-negative, not {}".format(tensor_number))
        observed_indices = observed_tensor_indices[tensor_number]
        for index in indices:
            if index in observed_indices:
                raise ValueError("index {} of tensor {} appears twice".format(index,tensor_number))
            observed_indices.add(index)
    for join in joins:
        observeTensorIndices(join.left_tensor_number,*join.left_tensor_indices)
        observeTensorIndices(join.right_tensor_number,*join.right_tensor_indices)
        if (join.left_tensor_number,join.right_tensor_number) in observed_joins:
            raise ValueError("two joins appear between tensors {} and {} (recall that specified joins can contain multiple indices".format(join.left_tensor_number,join.right_tensor_number))
    for group in final_groups:
        for (tensor_number,index) in group:
            observeTensorIndices(tensor_number,index)
    number_of_tensors = computeLengthAndCheckForGaps(
        observed_tensor_indices.keys(),
        "the following tensor numbers were expected but not observed"
    )
    if number_of_tensors == 0:
        raise ValueError("no tensors have been specified to be contracted")
    observed_tensor_ranks = [
        computeLengthAndCheckForGaps(
            observed_tensor_indices[tensor_number],
            "the following indices of tensor {} were expected but not observed".format(tensor_number)
        )
        for tensor_number in xrange(number_of_tensors)
    ]
    if tensor_ranks is None:
        tensor_ranks = observed_tensor_ranks
    else:
        if tensor_ranks != observed_tensor_ranks:
            raise ValueError("the ranks of the arguments were specified to be {}, but inferred to be {}".format(tensor_ranks,observed_tensor_ranks))
    # }}}
    # Build the prelude for the function {{{
    function_lines = [
        "def contract(" + ",".join(["_{}".format(tensor_number) for tensor_number in xrange(number_of_tensors)]) + "):",
    ]
    # Build the documentation string {{{
    function_lines.append('"""')
    for tensor_number in xrange(number_of_tensors):
        function_lines.append("_{} - tensor of rank {}".format(tensor_number,tensor_ranks[tensor_number]))
    function_lines.append('"""')
    # }}}
    # Check that the tensors have the correct ranks {{{
    for tensor_number, expected_rank in enumerate(tensor_ranks):
        function_lines.append("if _{tensor_number}.ndim != {expected_rank}: raise UnexpectedTensorRankError({tensor_number},{expected_rank},_{tensor_number}.ndim)".format(tensor_number=tensor_number,expected_rank=expected_rank))
    # }}}
    # Check that the tensors have matching dimensions {{{
    for join in joins:
        for (left_index,right_index) in zip(join.left_tensor_indices,join.right_tensor_indices):
            function_lines.append('if _{left_tensor_number}.shape[{left_index}] != _{right_tensor_number}.shape[{right_index}]: raise DimensionMismatchError({left_tensor_number},{left_index},_{left_tensor_number}.shape[{left_index}],{right_tensor_number},{right_index},_{right_tensor_number}.shape[{right_index}])'.format(
                left_tensor_number = join.left_tensor_number,
                left_index = left_index,
                right_tensor_number = join.right_tensor_number,
                right_index = right_index,
            ))
    # }}}
    # }}}
    # Build the main part of the function {{{
    next_tensor_number = number_of_tensors
    active_tensor_numbers = set(xrange(number_of_tensors))
    joins.reverse()
    while joins:
        join = joins.pop()
        left_tensor_number = join.left_tensor_number
        left_tensor_indices = join.left_tensor_indices
        right_tensor_number = join.right_tensor_number
        right_tensor_indices = join.right_tensor_indices
        active_tensor_numbers.remove(left_tensor_number)
        active_tensor_numbers.remove(right_tensor_number)
        active_tensor_numbers.add(next_tensor_number)
        tensor_ranks.append(tensor_ranks[left_tensor_number]+tensor_ranks[right_tensor_number]-2*len(left_tensor_indices))
        # Add the lines to the function {{{
        function_lines.append("_{} = _{}.contractWith(_{},{},{})".format(
            next_tensor_number,
            left_tensor_number,
            right_tensor_number,
            left_tensor_indices,
            right_tensor_indices,
        ))
        function_lines.append("del _{}, _{}".format(left_tensor_number,right_tensor_number))
        # }}}
        # Update the remaining joins {{{
        left_index_map = computePostContractionIndexMap(tensor_ranks[left_tensor_number],left_tensor_indices)
        right_index_map = computePostContractionIndexMap(tensor_ranks[right_tensor_number],right_tensor_indices,len(left_index_map))
        i = len(joins)-1
        observed_joins = dict()
        while i >= 0:
            join_to_update = joins[i]
            join_to_update.update(left_tensor_number,next_tensor_number,left_index_map)
            join_to_update.update(right_tensor_number,next_tensor_number,right_index_map)
            observed_join_tensor_numbers = (join_to_update.left_tensor_number,join_to_update.right_tensor_number)
            if observed_join_tensor_numbers in observed_joins:
                observed_joins[observed_join_tensor_numbers].mergeWith(join_to_update)
                del joins[i]
            else:
                observed_joins[observed_join_tensor_numbers] = join_to_update
            i -= 1
        # }}}
        # Update the final groups {{{
        for group in final_groups:
            for i, (tensor_number,tensor_index) in enumerate(group):
                if tensor_number == left_tensor_number:
                    group[i] = (next_tensor_number,left_index_map[tensor_index])
                elif tensor_number == right_tensor_number:
                    group[i] = (next_tensor_number,right_index_map[tensor_index])
        # }}}
        next_tensor_number += 1
    # }}}
    # Build the finale of the function {{{
    # Combine any remaining tensors using outer products {{{
    active_tensor_numbers = list(active_tensor_numbers)
    final_tensor_number = active_tensor_numbers[0]
    for tensor_number in active_tensor_numbers[1:]: 
        function_lines.append("_{final_tensor_number} = _{final_tensor_number}.contractWith(_{tensor_number},[],[])".format(final_tensor_number=final_tensor_number,tensor_number=tensor_number))
        function_lines.append("del _{}".format(tensor_number))
    # }}}
    if len(final_groups) > 0:
        # Compute index map and apply the index map to the final groups {{{
        index_offset = 0
        index_map = {}
        for tensor_number in active_tensor_numbers:
            for index in xrange(tensor_ranks[tensor_number]):
                index_map[(tensor_number,index)] = index+index_offset
            index_offset += tensor_ranks[tensor_number]
        final_groups = [applyIndexMapTo(index_map,group) for group in final_groups]
        # }}}
        function_lines.append("return _{}.join({})".format(final_tensor_number,final_groups))
    else:
        function_lines.append("return _{}.extractScalar()".format(final_tensor_number))
    # }}}
    # Compile and return the function {{{
    function_source = "\n    ".join(function_lines)
    captured_definition = {}
    visible_exceptions = {}
    for exception_name in ["DimensionMismatchError","UnexpectedTensorRankError"]:
        visible_exceptions[exception_name] = globals()[exception_name]
    exec function_source in visible_exceptions, captured_definition
    contract = captured_definition["contract"]
    contract.source = function_source
    return contract
    # }}}
# }}}

# }}} Functions

# Exports {{{
__all__ = [
    # Exceptions {{{
    "DimensionMismatchError",
    "UnexpectedTensorRankError",
    # }}}
    # Classes {{{
    "Join",
    # }}}
    # Functions {{{
    "applyPermutation",
    "crand",
    "invertPermutation",
    "makeDataContractor",
    # }}}
]
# }}}
