#@+leo-ver=4-thin
#@+node:gcross.20090930212235.1299:@thin simulate-TI.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20090930201628.1301:<< Import needed modules >>
from utils import crand, normalize, create_normalized_state_site_tensors
from contractors import *
from numpy import array, zeros, multiply, identity
from paulis import *





from numpy import tensordot
from numpy.linalg import norm
#@-node:gcross.20090930201628.1301:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20090930201628.1306:optimize
def optimize(site_number,normalization_index,move_function):
    print "\tOptimizing site {0}...".format(site_number+1)
    global current_energy, chain, state_site_tensors

    try:
        new_site_tensor, new_energy = chain.compute_optimized_state_site_tensor(state_site_tensors[site_number])

        if new_energy-current_energy > 1e-7:
            print "\t\tVariational breakdown: {0} > {1}".format(new_energy,current_energy)
            raise ConvergenceError
        print "\t\tEnergy =", current_energy

        state_site_tensors[site_number] = new_site_tensor

    except ConvergenceError:
        print "\t\tConvergence error, skipping site..."
        new_site_tensor = state_site_tensors[site_number]
        new_energy = chain.fully_contract_with_state_site_tensor(new_site_tensor.conj(),new_site_tensor)      

    current_energy = new_energy

    normalized_site_tensor = normalize(new_site_tensor,normalization_index)
    normalize_state_site_tensors[site_number] = normalized_site_tensor

    move_function(normalized_site_tensor.conj(),normalized_site_tensor)
#@-node:gcross.20090930201628.1306:optimize
#@-others

#@<< Define parameters >>
#@+node:gcross.20090930201628.1302:<< Define parameters >>
number_of_sites = 8

bandwidth_dimension = 4

coupling_strength = 0.5

number_of_sweeps = 2
#@-node:gcross.20090930201628.1302:<< Define parameters >>
#@nl

#@<< Define operators >>
#@+node:gcross.20090930201628.1303:<< Define operators >>
left_operator_boundary = array([1,0,0])
right_operator_boundary = array([0,0,1])

operator_site_tensor = zeros((2,2,3,3),complex128)
operator_site_tensor[...,0,0] = I
operator_site_tensor[...,2,2] = I
operator_site_tensor[...,0,2] = -Z
operator_site_tensor[...,0,1] = -coupling_strength * X
operator_site_tensor[...,1,2] = X

operator_site_tensors = [operator_site_tensor] * number_of_sites
#@-node:gcross.20090930201628.1303:<< Define operators >>
#@nl

#@<< Initialize system >>
#@+node:gcross.20090930201628.1304:<< Initialize system >>
physical_dimension = 2

state_site_tensors = create_normalized_state_site_tensors(physical_dimension,bandwidth_dimension,number_of_sites)
normalize_state_site_tensors = [None]*number_of_sites

chain = ChainContractorForExpectations(state_site_tensors,operator_site_tensors,left_operator_boundary,right_operator_boundary)

current_energy = chain.fully_contract_with_state_site_tensor(state_site_tensors[0].conj(),state_site_tensors[0])
#@-node:gcross.20090930201628.1304:<< Initialize system >>
#@nl

#@<< Run sweeps >>
#@+node:gcross.20090930201628.1305:<< Run sweeps >>
print "Initial energy =", current_energy

for sweep_number in xrange(1,number_of_sweeps+1):
    print "Sweep number {0}:".format(sweep_number)

    for site_number in xrange(number_of_sites-1):
        optimize(site_number,2,chain.contract_and_move_right)

    for site_number in xrange(number_of_sites-1,0,-1):
        optimize(site_number,1,chain.contract_and_move_left)
#@-node:gcross.20090930201628.1305:<< Run sweeps >>
#@nl
#@-node:gcross.20090930212235.1299:@thin simulate-TI.py
#@-leo
