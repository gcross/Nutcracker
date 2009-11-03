#@+leo-ver=4-thin
#@+node:gcross.20091023155936.1518:@thin simulate-adiabatic1.py
#@@language Python

import warnings
warnings.simplefilter("ignore",DeprecationWarning)

#@<< Import needed modules >>
#@+node:gcross.20091023155936.1519:<< Import needed modules >>
from numpy import array, zeros, identity, complex128
import sys

from database import VMPSDatabase
from paulis import *
from simulation import *
from utils import convert_old_state_tensors_to_orthogonal_state_information
#@-node:gcross.20091023155936.1519:<< Import needed modules >>
#@nl

#@+others
#@-others

#@<< Read simulation parameters >>
#@+node:gcross.20091023155936.1520:<< Read simulation parameters >>
try:
    number_of_sites = int(sys.argv[1])
    s = float(sys.argv[2])
except:
    print "USAGE:  {0} <number of sites> <s>".format(sys.argv[0])
    sys.exit(1)
#@-node:gcross.20091023155936.1520:<< Read simulation parameters >>
#@nl

#@<< Define parameters >>
#@+node:gcross.20091023155936.1521:<< Define parameters >>
bandwidth_dimension = 6

sweep_convergence_threshold=1e-8
bandwidth_convergence_threshold=1e-7
trial_convergence_threshold=1e-5
level_tolerance = 1e-4
#@-node:gcross.20091023155936.1521:<< Define parameters >>
#@nl

#@<< Define operators >>
#@+node:gcross.20091023155936.1522:<< Define operators >>
physical_dimension = 2

left_operator_site_tensor = zeros((2,2,1,4),complex128)
left_operator_site_tensor[...,0,0] = I
left_operator_site_tensor[...,0,1] = Z
left_operator_site_tensor[...,0,3] = -s*X

middle_operator_site_tensor = zeros((2,2,4,4),complex128)
middle_operator_site_tensor[...,0,0] = I
middle_operator_site_tensor[...,0,1] = Z
middle_operator_site_tensor[...,1,2] = -(1-s)*X
middle_operator_site_tensor[...,2,3] = Z
middle_operator_site_tensor[...,0,3] = -s*X
middle_operator_site_tensor[...,3,3] = I

right_operator_site_tensor = zeros((2,2,4,1),complex128)
right_operator_site_tensor[...,1,0] = -(1-s)*X
right_operator_site_tensor[...,2,0] = Z
right_operator_site_tensor[...,3,0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
#@-node:gcross.20091023155936.1522:<< Define operators >>
#@nl

#@<< Run sweeps >>
#@+node:gcross.20091023155936.1523:<< Run sweeps >>
energy_levels = []
state_site_tensors_list = []
orthogonal_state_information_list = []

database = VMPSDatabase()
database.cursor.execute("select count(*) from adiabatic_model_1_simulations where number_of_sites={0} and s={1};".format(number_of_sites,s))
if database.cursor.fetchone()[0] > 0:
    sys.exit("Model has already been solved for the given parameters.")

try:
    level_number = 1
    while level_number <= 1:

        def sweep_callback(bandwidth_dimension,sweep_number,energy,level_number=level_number):
            print "Level {level_number}, Bandwidth dimension {bandwidth_dimension}, Sweep number {sweep_number}: {energy}".format(**vars())

        def trial_callback(status,trial_energy,lowest_trial_energy,number_of_occurances,number_of_occurances_needed,level_number=level_number):
            if status == trial_ACCEPTED:
                print "Level {level_number}, ACCEPTED {trial_energy};  need to see {0} more times.".format(number_of_occurances_needed-number_of_occurances,**vars())
            elif status == trial_IMPROVED:
                print "Level {level_number}, IMPROVED {trial_energy};  need to see {0} more times.".format(number_of_occurances_needed-number_of_occurances,**vars())
            elif status == trial_REPLACED:
                print "Level {level_number}, REPLACED {trial_energy};  now need to see {0} more times.".format(number_of_occurances_needed,**vars())
            elif status == trial_REJECTED:
                print "Level {level_number}, REJECTED {trial_energy};  still need to see lowest {0} more times.".format(number_of_occurances_needed-number_of_occurances,**vars())

        energy, state_site_tensors = \
            compute_level (
                number_of_sites,
                physical_dimension,
                bandwidth_dimension,
                operator_site_tensors,
                orthogonal_state_information_list,
                sweep_callback=sweep_callback,
                trial_callback=trial_callback,
                sweep_number_to_restart_after=1e100,
                sweep_convergence_threshold=sweep_convergence_threshold,
                bandwidth_convergence_threshold=bandwidth_convergence_threshold,
                trial_convergence_threshold=trial_convergence_threshold,
            )
        print "Level {0} energy = {1}".format(level_number,energy)
        energy_levels.append(energy)
        state_site_tensors_list.append(state_site_tensors)

        if level_number < 4:
            orthogonal_state_information_list.append(convert_old_state_tensors_to_orthogonal_state_information(state_site_tensors))

        def ensure_equal_or_restart(index1,index2):
            global level_number, energy_levels, orthogonal_state_information_list
            if abs(energy_levels[index1]-energy_levels[index2]) > level_tolerance:
                print "ERROR:  Lowest two energy levels should be equal;  re-running computation of excited state."
                if energy_levels[index1] < energy_levels[index2]:
                    index_to_delete = index2
                else:
                    index_to_delete = index1
                del energy_levels[index_to_delete]
                if index_to_delete < len(orthogonal_state_information_list):
                    del orthogonal_state_information_list[index_to_delete]
                level_number -= 1
            else:
                print "(Levels {0} and {1} are equal, as expected.)".format(index1+1,index2+1)

        if level_number == 2:
            bandwidth_dimension += 2
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
        database.cursor.execute("insert into adiabatic_model_1_simulations (number_of_sites,s,solution_id,energy_gap) values ({0},{1},'{2}',{3})".format(number_of_sites,s,solution_id,energy_levels[-1]-energy_levels[0]))
    except:
        database.cursor.execute("ROLLBACK;")
        raise
    else:
        database.cursor.execute("COMMIT;")

print
print "The energy levels of the system are:"
for energy_level in energy_levels:
    print "\t",energy_level
#@-node:gcross.20091023155936.1523:<< Run sweeps >>
#@nl
#@-node:gcross.20091023155936.1518:@thin simulate-adiabatic1.py
#@-leo
