#@+leo-ver=4-thin
#@+node:gcross.20091004090150.1364:@thin simulate-gadget.py
#@@language Python

import warnings
warnings.simplefilter("ignore",DeprecationWarning)

#@<< Import needed modules >>
#@+node:gcross.20091004090150.1365:<< Import needed modules >>
from numpy import array, zeros, identity, complex128
import sys

from database import VMPSDatabase
from paulis import *
from simulation import compute_level
from utils import convert_old_state_tensors_to_orthogonal_state_information
#@-node:gcross.20091004090150.1365:<< Import needed modules >>
#@nl

#@+others
#@-others

#@<< Read simulation parameters >>
#@+node:gcross.20091019143831.1403:<< Read simulation parameters >>
try:
    number_of_sites = int(sys.argv[1])
    perturbation_coefficient = float(sys.argv[2])
except:
    print "USAGE:  {0} <number of sites> <perturbation coefficient>".format(sys.argv[0])
    sys.exit(1)
#@-node:gcross.20091019143831.1403:<< Read simulation parameters >>
#@nl

#@<< Define parameters >>
#@+node:gcross.20091004090150.1367:<< Define parameters >>
bandwidth_dimension = 2
#@-node:gcross.20091004090150.1367:<< Define parameters >>
#@nl

#@<< Define operators >>
#@+node:gcross.20091004090150.1368:<< Define operators >>
physical_dimension = 2

number_of_sites += 2

left_operator_site_tensor = zeros((2,2,1,6),complex128)
left_operator_site_tensor[...,0,0] = -perturbation_coefficient*X/2
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
middle_operator_site_tensor[...,4,5] = Z
middle_operator_site_tensor[...,5,5] = I

right_operator_site_tensor = zeros((2,2,6,1),complex128)
right_operator_site_tensor[...,1,0] = I
right_operator_site_tensor[...,3,0] = -perturbation_coefficient*X/2
right_operator_site_tensor[...,4,0] = 0
right_operator_site_tensor[...,5,0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
#@-node:gcross.20091004090150.1368:<< Define operators >>
#@nl

#@<< Run sweeps >>
#@+node:gcross.20091004090150.1370:<< Run sweeps >>
energy_levels = []
state_site_tensors_list = []
orthogonal_state_information_list = []

database = VMPSDatabase()
database.cursor.execute("select count(*) from gadget_model_1_simulations where number_of_sites={0} and perturbation_strength={1};".format(number_of_sites-2,perturbation_coefficient))
if database.cursor.fetchone()[0] > 0:
    sys.exit("Model has already been solved for the given parameters.")

try:
    level_number = 1
    while level_number <= 4:

        def sweep_callback(bandwidth_dimension,sweep_number,energy,level_number=level_number):
            print "Level {level_number}, Bandwidth dimension {bandwidth_dimension}, Sweep number {sweep_number}: {energy}".format(**vars())

        def trial_callback(trial_energy,lowest_trial_energy,number_of_occurances,number_of_occurances_needed,level_number=level_number):
            if trial_energy-lowest_trial_energy > 1e-7:
                print "Level {level_number}, REJECTED: {trial_energy} > {lowest_trial_energy}".format(**vars())
            elif number_of_occurances_needed > number_of_occurances:
                print "Level {level_number}, ACCEPTED {trial_energy};  need to see {0} more times.".format(number_of_occurances_needed-number_of_occurances,**vars())

        energy, state_site_tensors = \
            compute_level (
                number_of_sites,
                physical_dimension,
                bandwidth_dimension,
                operator_site_tensors,
                orthogonal_state_information_list,
                sweep_callback=sweep_callback,
                trial_callback=trial_callback,
            )
        print "Level {0} energy = {1}".format(level_number,energy)
        energy_levels.append(energy)
        state_site_tensors_list.append(state_site_tensors)

        if level_number < 4:
            orthogonal_state_information_list.append(convert_old_state_tensors_to_orthogonal_state_information(state_site_tensors))

        def ensure_equal_or_restart(index1,index2):
            global level_number, energy_levels, orthogonal_state_information_list
            if abs(energy_levels[index1]-energy_levels[index2]) > 1e-7:
                print "ERROR:  Lowest two energy levels should be equal;  re-running computation of excited state."
                if energy_levels[index1] < energy_levels[index2]:
                    index_to_delete = index2
                else:
                    index_to_delete = index1
                del energy_levels[index_to_delete]
                del orthogonal_state_information_list[index_to_delete]
                level_number -= 1
            else:
                print "(Levels {0} and {1} are equal, as expected.)".format(index1+1,index2+1)

        if level_number == 2:
            bandwidth_dimension += 1
            ensure_equal_or_restart(0,1)

        if level_number == 4:
            ensure_equal_or_restart(2,3)

        level_number += 1

except KeyboardInterrupt:
    pass

else:
    database.cursor.execute("BEGIN TRANSACTION;")
    try:
        solution_id = database.save_solution(energy_levels,state_site_tensors_list)
        database.cursor.execute("insert into gadget_model_1_simulations (number_of_sites,perturbation_strength,solution_id,energy_gap) values ({0},{1},'{2}',{3})".format(number_of_sites-2,perturbation_coefficient,solution_id,energy_levels[-1]-energy_levels[0]))
    except:
        database.cursor.execute("ROLLBACK;")
        raise
    else:
        database.cursor.execute("COMMIT;")

print
print "The energy levels of the system are:"
for energy_level in energy_levels:
    print "\t",energy_level

#@-node:gcross.20091004090150.1370:<< Run sweeps >>
#@nl
#@-node:gcross.20091004090150.1364:@thin simulate-gadget.py
#@-leo
