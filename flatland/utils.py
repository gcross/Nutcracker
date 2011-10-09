#@+leo-ver=5-thin
#@+node:gcross.20111009135633.1129: * @file utils.py
#@+<< Imports >>
#@+node:gcross.20111009135633.1135: ** << Imports >>
from numpy import tensordot
#@-<< Imports >>

#@+others
#@+node:gcross.20111009135633.1130: ** Functions
#@+node:gcross.20111009135633.1131: *3* formContractor
def formContractor(order,joins,result_joins):
    observed_tensor_indices = {}

    #@+<< Tabulate all of the observed tensor indices >>
    #@+node:gcross.20111009135633.1132: *4* << Tabulate all of the observed tensor indices >>
    for (tensor_id,index) in sum([list(x) for x in joins] + [list(x) for x in result_joins],[]):
        if index < 0:
            raise ValueError("index {} of tensor {} is negative".format(index,tensor_id))
        try:
            observed_indices = observed_tensor_indices[tensor_id]
        except KeyError:
            observed_indices = set()
            observed_tensor_indices[tensor_id] = observed_indices
        if index in observed_indices:
            raise ValueError("index {} of tensor {} appears more than once in the joins".format(index,tensor_id))
        observed_indices.add(index)
    #@-<< Tabulate all of the observed tensor indices >>

    tensor_join_ids = {}

    #@+<< Check the observed tensor indices and initialize the map of tensor joins >>
    #@+node:gcross.20111009135633.1133: *4* << Check the observed tensor indices and initialize the map of tensor joins >>
    for (tensor_id,observed_indices) in observed_tensor_indices.items():
        tensor_dimension = max(observed_indices)+1
        expected_indices = set(range(tensor_dimension))
        invalid_indices = observed_indices - expected_indices
        missing_indices = expected_indices - observed_indices
        if len(invalid_indices) > 0:
            raise ValueError('the invalid indices {} have appeared in joins involving tendor {}'.format(invalid_indices,tensor_id))
        if len(missing_indices) > 0:
            raise ValueError('the expected indices {} no not appear in any of the joins for tensor {}'.format(missing_indices,tensor_id))
        if tensor_id not in order:
            raise ValueError('tensor {} does not appear in the list of arguments'.format(tensor_id))
        tensor_join_ids[tensor_id] = [None]*tensor_dimension
    #@-<< Check the observed tensor indices and initialize the map of tensor joins >>

    result_join_ids = []

    #@+<< Label each join with a unique id >>
    #@+node:gcross.20111009135633.1134: *4* << Label each join with a unique id >>
    current_join_id = 0
    for join in joins:
        for (tensor_id,index) in join:
            tensor_join_ids[tensor_id][index] = current_join_id
        current_join_id += 1
    for join in result_joins:
        join_ids = []
        for (tensor_id,index) in join:
            join_ids.append(current_join_id)
            tensor_join_ids[tensor_id][index] = current_join_id
            current_join_id += 1
        result_join_ids.append(join_ids)
    #@-<< Label each join with a unique id >>

    argument_join_ids = [tensor_join_ids[tensor_id] for tensor_id in order]

    #@+<< Form the contractor function >>
    #@+node:gcross.20111009135633.1136: *4* << Form the contractor function >>
    def contract(*arguments):
        if len(arguments) != len(order):
            raise ValueError("wrong number of arguments;  expected {} but received {}",len(order),len(arguments))
        arguments = list(arguments)
        join_ids_index = -2
        current_tensor = arguments.pop()
        current_join_ids = argument_join_ids[-1]
        while len(arguments) > 0:
            next_tensor = arguments.pop()
            next_join_ids = argument_join_ids[join_ids_index]
            join_ids_index -= 1
            first_axes = []
            second_axes = []
            first_axis_index = 0
            common_join_ids = set()
            for join_id in current_join_ids:
                if join_id in next_join_ids:
                    common_join_ids.add(join_id)
                    first_axes.append(first_axis_index)
                    second_axes.append(next_join_ids.index(join_id))
                first_axis_index += 1
            current_tensor = tensordot(current_tensor,next_tensor,(first_axes,second_axes))
            current_join_ids = [i for i in current_join_ids+next_join_ids if i not in common_join_ids]
        current_tensor = current_tensor.transpose([current_join_ids.index(i) for i in sum([list(x) for x in result_join_ids],[])])
        old_shape = current_tensor.shape
        new_shape = []
        index = 0
        for join in result_join_ids:
            dimension = 1
            for _ in join:
                dimension *= old_shape[index]
                index += 1
            new_shape.append(dimension)
        return current_tensor.reshape(new_shape)
    #@-<< Form the contractor function >>

    return contract
#@-others

#@+<< Exports >>
#@+node:gcross.20111009193003.1160: ** << Exports >>
#@-<< Exports >>
#@-leo
