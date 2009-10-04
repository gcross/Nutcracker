#@+leo-ver=4-thin
#@+node:gcross.20091004090150.1364:@thin simulate-gadget.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20091004090150.1365:<< Import needed modules >>
from utils import crand, normalize_and_return_inverse_normalizer, create_normalized_state_site_tensors, multiply_tensor_by_matrix_at_index, normalize_and_denormalize
from contractors import *
from numpy import array, zeros, multiply, identity
from numpy.linalg import norm
from paulis import *



from numpy import tensordot
from math import sqrt
#@-node:gcross.20091004090150.1365:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091004090150.1366:optimize
def optimize(site_number,next_site_number,normalization_index,chain_move_function,projector_move_function):
    print "\tOptimizing site {0}...".format(site_number+1)
    global current_energy, chain, state_site_tensors

    project = projector.construct_projector()
    try:
        new_site_tensor, new_energy = chain.compute_optimized_state_site_tensor(state_site_tensors[site_number],project)

        if new_energy-current_energy > 1e-7:
            raise ConvergenceError("Variational breakdown: {0} > {1}".format(new_energy,current_energy))
        print "\t\tEnergy =", current_energy

    except ConvergenceError, e:
        print "\t\tConvergence error, skipping site... [{0}]".format(e)

        new_site_tensor = state_site_tensors[site_number]
        new_energy = chain.fully_contract_with_state_site_tensor(new_site_tensor.conj(),new_site_tensor)

    current_energy = new_energy

    state_site_tensors[site_number], state_site_tensors[next_site_number] = normalize_and_denormalize(new_site_tensor,normalization_index,state_site_tensors[next_site_number],3-normalization_index)

    chain_move_function(state_site_tensors[site_number].conj(),state_site_tensors[site_number])
    projector_move_function(state_site_tensors[site_number])
#@-node:gcross.20091004090150.1366:optimize
#@-others

#@<< Define parameters >>
#@+node:gcross.20091004090150.1367:<< Define parameters >>
number_of_sites = 100

bandwidth_dimension = 4

number_of_sweeps = 4

number_of_levels = 4

delta = 0.1
lambda_ = 1
#@-node:gcross.20091004090150.1367:<< Define parameters >>
#@nl

#@<< Define operators >>
#@+node:gcross.20091004090150.1368:<< Define operators >>
left_operator_boundary = array([1])
right_operator_boundary = array([1])

left_operator_site_tensor = zeros((2,2,1,6),complex128)
left_operator_site_tensor[...,0,0] = delta*X
left_operator_site_tensor[...,0,2] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,5] = -lambda_*Z*0

middle_operator_site_tensor = zeros((2,2,6,6),complex128)
middle_operator_site_tensor[...,0,0] = I
middle_operator_site_tensor[...,0,1] = X
middle_operator_site_tensor[...,1,1] = I
middle_operator_site_tensor[...,2,2] = I
middle_operator_site_tensor[...,2,3] = X
middle_operator_site_tensor[...,3,3] = I
middle_operator_site_tensor[...,4,4] = I
middle_operator_site_tensor[...,4,5] = -lambda_*Z
middle_operator_site_tensor[...,5,5] = I

right_operator_site_tensor = zeros((2,2,6,1),complex128)
right_operator_site_tensor[...,1,0] = I
right_operator_site_tensor[...,3,0] = delta*X
right_operator_site_tensor[...,4,0] = -lambda_*Z*0
right_operator_site_tensor[...,5,0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
#@-node:gcross.20091004090150.1368:<< Define operators >>
#@nl

#@<< Initialize system >>
#@+node:gcross.20091004090150.1369:<< Initialize system >>
physical_dimension = 2

state_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)

chain = ChainContractorForExpectations(state_site_tensors,operator_site_tensors,left_operator_boundary,right_operator_boundary)
projector = ChainProjector()

current_energy = chain.fully_contract_with_state_site_tensor(state_site_tensors[0].conj(),state_site_tensors[0])
#@-node:gcross.20091004090150.1369:<< Initialize system >>
#@nl

#@<< Run sweeps >>
#@+node:gcross.20091004090150.1370:<< Run sweeps >>
energy_levels = []

try:
    for level_number in xrange(1,number_of_levels+1):
        for sweep_number in xrange(1,number_of_sweeps+1):
            print "Sweep number {0}:".format(sweep_number)

            for site_number in xrange(number_of_sites-1):
                optimize(site_number,site_number+1,2,chain.contract_and_move_right,projector.contract_and_move_right)

            for site_number in xrange(number_of_sites-1,0,-1):
                optimize(site_number,site_number-1,1,chain.contract_and_move_left,projector.contract_and_move_left)

        print "Level {0} energy = {1}".format(level_number,current_energy)
        energy_levels.append(current_energy)

        if level_number < number_of_levels:
            bandwidth_dimension += 2
            old_state_site_tensors = state_site_tensors
            state_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)
            projector.add_additional_state_to_projector_and_reset_chains_with_new_initial_state(old_state_site_tensors,state_site_tensors)
            projector.orthogonalize(state_site_tensors)
            chain = ChainContractorForExpectations(state_site_tensors,operator_site_tensors,left_operator_boundary,right_operator_boundary)
            current_energy = chain.fully_contract_with_state_site_tensor(state_site_tensors[0].conj(),state_site_tensors[0])
except KeyboardInterrupt:
    pass

print
print "The energy levels of the system are:"
for energy_level in energy_levels:
    print "\t",energy_level
#@-node:gcross.20091004090150.1370:<< Run sweeps >>
#@nl
#@-node:gcross.20091004090150.1364:@thin simulate-gadget.py
#@-leo
