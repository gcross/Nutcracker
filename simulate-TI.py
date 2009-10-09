#@+leo-ver=4-thin
#@+node:gcross.20091009120817.1394:@thin simulate-TI.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20091009120817.1395:<< Import needed modules >>
from simulation import *
from contractors import ConvergenceError
from numpy import array, zeros, multiply, identity, complex128
from paulis import *
#@-node:gcross.20091009120817.1395:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091009120817.1396:optimize
from numpy.linalg import norm

def optimize(simulation_move_function):
    print "\tOptimizing site {0}...".format(site_number+1)

    site_tensor = simulation.state.active_site_tensor.copy()
    simulation.projection_chain.construct_projector()(site_tensor)
    overlap = norm(site_tensor-simulation.state.active_site_tensor)
    if overlap > 1e-10:
        print "\t\tStarting overlap =", overlap

    try:
        simulation.optimize_active_site()
        print "\t\tEnergy =", simulation.energy

    except ConvergenceError, e:
        print "\t\tConvergence error, skipping site... [{0}]".format(e)

    site_tensor = simulation.state.active_site_tensor.copy()
    simulation.projection_chain.construct_projector()(site_tensor)
    overlap = norm(site_tensor-simulation.state.active_site_tensor)
    if overlap > 1e-10:
        print "\t\tEnding overlap =", overlap

    pre_move_state_tensor = simulation.state.to_tensor()

    simulation_move_function()

    post_move_state_tensor = simulation.state.to_tensor()
    move_error = norm(post_move_state_tensor-pre_move_state_tensor)

    if move_error > 1e-10:
        print "\t\tMove error =", norm(post_move_state_tensor-pre_move_state_tensor)
#@-node:gcross.20091009120817.1396:optimize
#@-others

#@<< Define parameters >>
#@+node:gcross.20091009120817.1397:<< Define parameters >>
number_of_sites = 8

bandwidth_dimension = 2

number_of_levels = 3

coupling_strength = 0.5
#@-node:gcross.20091009120817.1397:<< Define parameters >>
#@nl

#@<< Define operators >>
#@+node:gcross.20091009120817.1398:<< Define operators >>
middle_operator_site_tensor = zeros((2,2,3,3),complex128)
middle_operator_site_tensor[...,0,0] = I
middle_operator_site_tensor[...,2,2] = I
middle_operator_site_tensor[...,0,2] = -Z
middle_operator_site_tensor[...,0,1] = -coupling_strength * X
middle_operator_site_tensor[...,1,2] = X

left_operator_site_tensor = middle_operator_site_tensor[...,:1,:]
right_operator_site_tensor = middle_operator_site_tensor[...,:,-1:]

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
#@-node:gcross.20091009120817.1398:<< Define operators >>
#@nl

#@<< Initialize system >>
#@+node:gcross.20091009120817.1399:<< Initialize system >>
physical_dimension = 2

simulation = Simulation(number_of_sites,physical_dimension,bandwidth_dimension,operator_site_tensors)

states = []
#@-node:gcross.20091009120817.1399:<< Initialize system >>
#@nl

#@<< Run sweeps >>
#@+node:gcross.20091009120817.1400:<< Run sweeps >>
energy_levels = []

try:
    for level_number in xrange(1,number_of_levels+1):
        sweep_number = 0
        previous_energy = 1e100
        while previous_energy-simulation.energy > 1e-7:
            sweep_number += 1
            previous_energy = simulation.energy
            print "Sweep number {0}:".format(sweep_number)

            for site_number in xrange(number_of_sites-1):
                optimize(simulation.move_active_site_right)

            for site_number in xrange(number_of_sites-1,0,-1):
                optimize(simulation.move_active_site_left)

        print "Level {0} energy = {1}".format(level_number,simulation.energy)
        energy_levels.append(simulation.energy)

        if level_number < number_of_levels:
            bandwidth_dimension += 0
            states.append(simulation.state)
            simulation.add_state_to_projectors_and_reset(bandwidth_dimension)

except KeyboardInterrupt:
    pass

print
print "The energy levels of the system are:"
for energy_level in energy_levels:
    print "\t",energy_level
#@-node:gcross.20091009120817.1400:<< Run sweeps >>
#@nl
#@-node:gcross.20091009120817.1394:@thin simulate-TI.py
#@-leo
