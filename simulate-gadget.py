#@+leo-ver=4-thin
#@+node:gcross.20091004090150.1364:@thin simulate-gadget.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20091004090150.1365:<< Import needed modules >>
from simulation import *
from contractors import ConvergenceError
from numpy import array, zeros, multiply, identity, complex128
from paulis import *
#@-node:gcross.20091004090150.1365:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091004090150.1366:optimize
def optimize(simulation_move_function):
    global number_of_sites_skipped_in_a_row

    #print "\tOptimizing site {0}...".format(site_number+1)

    try:
        simulation.optimize_active_site()
        #print "\t\tEnergy =", simulation.energy
        number_of_sites_skipped_in_a_row = 0

    except ConvergenceError, e:
        number_of_sites_skipped_in_a_row += 1
        if number_of_sites_skipped_in_a_row >= number_of_sites:
            raise
        #print "\t\tConvergence error, skipping site... [{0}]".format(e)

    simulation_move_function()
#@-node:gcross.20091004090150.1366:optimize
#@-others

#@<< Define parameters >>
#@+node:gcross.20091004090150.1367:<< Define parameters >>
number_of_sites = 100

bandwidth_dimension = 2

number_of_levels = 4

x = 0.1
#@-node:gcross.20091004090150.1367:<< Define parameters >>
#@nl

#@<< Define operators >>
#@+node:gcross.20091004090150.1368:<< Define operators >>
number_of_sites += 2

#ProjectorOnto1 = array([[0,0],[0,1]],complex128)

left_operator_site_tensor = zeros((2,2,1,6),complex128)
left_operator_site_tensor[...,0,0] = -x*X/2
left_operator_site_tensor[...,0,2] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,5] = 0

middle_operator_site_tensor = zeros((2,2,6,6),complex128)
middle_operator_site_tensor[...,0,0] = I
middle_operator_site_tensor[...,0,1] = X
middle_operator_site_tensor[...,1,1] = I
middle_operator_site_tensor[...,2,2] = I
middle_operator_site_tensor[...,2,3] = X
middle_operator_site_tensor[...,3,3] = I
middle_operator_site_tensor[...,4,4] = I
middle_operator_site_tensor[...,4,5] = Z #ProjectorOnto1
middle_operator_site_tensor[...,5,5] = I

right_operator_site_tensor = zeros((2,2,6,1),complex128)
right_operator_site_tensor[...,1,0] = I
right_operator_site_tensor[...,3,0] = -x*X/2
right_operator_site_tensor[...,4,0] = 0
right_operator_site_tensor[...,5,0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
#@-node:gcross.20091004090150.1368:<< Define operators >>
#@nl

#@<< Initialize system >>
#@+node:gcross.20091004090150.1369:<< Initialize system >>
physical_dimension = 2

simulation = Simulation(number_of_sites,physical_dimension,bandwidth_dimension,operator_site_tensors)
#@-node:gcross.20091004090150.1369:<< Initialize system >>
#@nl

#@<< Run sweeps >>
#@+node:gcross.20091004090150.1370:<< Run sweeps >>
energy_levels = []

try:
    for level_number in xrange(1,number_of_levels+1):
        trial_number = 0
        previous_trial_energy = 1e100
        while previous_trial_energy-simulation.energy > 1e-7:
            previous_trial_energy = min(previous_trial_energy,simulation.energy)
            if trial_number > 0:
                simulation.restart(bandwidth_dimension)
            trial_number += 1

            current_bandwidth_dimension = bandwidth_dimension - 1
            previous_bandwidth_energy = 1e100
            while previous_bandwidth_energy-simulation.energy > 1e-7:
                current_bandwidth_dimension += 1
                if current_bandwidth_dimension > bandwidth_dimension:
                    simulation.increase_bandwidth_dimension_to(current_bandwidth_dimension)
                previous_bandwidth_energy = simulation.energy

                number_of_sites_skipped_in_a_row = 0
                sweep_number = 0
                previous_sweep_energy = 1e100
                while previous_sweep_energy-simulation.energy > 1e-7:
                    sweep_number += 1
                    previous_sweep_energy = simulation.energy

                    try:
                        for site_number in xrange(number_of_sites-1):
                            optimize(simulation.move_active_site_right)

                        for site_number in xrange(number_of_sites-1,0,-1):
                            optimize(simulation.move_active_site_left)
                    except ConvergenceError:
                        print "TOO MANY CONVERGENCE ERRORS!  Restarting with a new random state..."
                        number_of_sites_skipped_in_a_row = 0
                        sweep_number = 0
                        previous_sweep_energy = 1e100
                        simulation.restart(bandwidth_dimension)

                    print "Level {level_number}, Trial {trial_number}, Bandwidth dimension {current_bandwidth_dimension}, Sweep number {sweep_number}: {0}".format(simulation.energy,**vars())

        print "Level {0} energy = {1}".format(level_number,simulation.energy)
        energy_levels.append(simulation.energy)

        if level_number < number_of_levels:
            simulation.add_state_to_projectors_and_reset(bandwidth_dimension)
            bandwidth_dimension = 3

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
