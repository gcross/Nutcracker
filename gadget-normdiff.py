#@+leo-ver=4-thin
#@+node:gcross.20091022120112.1458:@thin gadget-normdiff.py
#@@language Python

import warnings
warnings.simplefilter("ignore",DeprecationWarning)

#@<< Import needed modules >>
#@+node:gcross.20091022120112.1459:<< Import needed modules >>
from numpy import array, zeros, identity, complex128, set_printoptions, log, imag, pi, sqrt
import sys

from database import VMPSDatabase
from paulis import *
from matrix_norm import compute_norm_maximizer
from utils import convert_old_state_tensors_to_orthogonal_state_information, peak_bandwidth_of, ConvergenceError
from contractors import compute_state_overlap, compute_state_norm
#@-node:gcross.20091022120112.1459:<< Import needed modules >>
#@nl

#@+others
#@-others

#@<< Read simulation parameters >>
#@+node:gcross.20091022120112.1460:<< Read simulation parameters >>
try:
    solution_id = sys.argv[1]
except:
    print "USAGE:  {0} <solution id>".format(sys.argv[0])
    sys.exit(1)
#@-node:gcross.20091022120112.1460:<< Read simulation parameters >>
#@nl

#@<< Define parameters >>
#@+node:gcross.20091022120112.1461:<< Define parameters >>
bandwidth_dimension = 2
#@-node:gcross.20091022120112.1461:<< Define parameters >>
#@nl

#@<< Read solution >>
#@+node:gcross.20091022120112.1462:<< Read solution >>
database = VMPSDatabase("reader")
energies, state_site_tensors_list = map(list,zip(*database.load_solution(solution_id)))
if len(state_site_tensors_list) == 0:
    sys.exit("There is no solution with ID {solution_id}.".format(**vars()))
#@-node:gcross.20091022120112.1462:<< Read solution >>
#@nl

#@<< Construct comparison operator >>
#@+node:gcross.20091022120112.1464:<< Construct comparison operator >>
database.cursor.execute("select number_of_sites, perturbation_strength, energy_gap from gadget_model_1_simulations where solution_id='{0}'".format(solution_id))
number_of_sites, perturbation_strength, energy_gap = database.cursor.fetchone()

predicted_effective_coupling_energy_scale = float(perturbation_strength)**2 * number_of_sites / 4.0

q_0_tensor = array([1,0],complex128).reshape(2,1,1)
q_1_tensor = array([0,1],complex128).reshape(2,1,1)
q_m_tensor = (array([1,-1],complex128)/sqrt(2)).reshape(2,1,1)
q_p_tensor = (array([1,+1],complex128)/sqrt(2)).reshape(2,1,1)

middle_state_site_tensors = [q_1_tensor]*number_of_sites

energies = [1,1,-1,-1,-1,-1,1,1]
state_site_tensors_list.append([q_m_tensor] + middle_state_site_tensors + [q_m_tensor])
state_site_tensors_list.append([q_p_tensor] + middle_state_site_tensors + [q_p_tensor])
state_site_tensors_list.append([q_m_tensor] + middle_state_site_tensors + [q_p_tensor])
state_site_tensors_list.append([q_p_tensor] + middle_state_site_tensors + [q_m_tensor])
#@-node:gcross.20091022120112.1464:<< Construct comparison operator >>
#@nl

#@<< Run sweeps >>
#@+node:gcross.20091022120112.1463:<< Run sweeps >>
orthogonal_state_information_list = map(convert_old_state_tensors_to_orthogonal_state_information,state_site_tensors_list)

def sweep_callback(bandwidth_dimension,sweep_number,energy):
    print "Bandwidth dimension {bandwidth_dimension}, Sweep number {sweep_number}: {energy}".format(**vars())

while bandwidth_dimension < 20:
    try:
        norm, compressed_state_site_tensors = \
            compute_norm_maximizer (
                len(state_site_tensors_list[0]),
                state_site_tensors_list[0][0].shape[0],
                bandwidth_dimension,
                energies,
                orthogonal_state_information_list,
                sweep_callback=sweep_callback,
            )

        print "norm =", norm

        sys.exit(0)

    except ConvergenceError:
        if bandwidth_dimension == 20:
            print "Failed to find state with bandwidth {0} that has non-vanishing norm;  giving up!"
            sys.exit(1)
        else:
            print "Failed to find state with bandwidth {0} that has non-vanishing norm;  increasing initial bandwidth to {1}".format(bandwidth_dimension,bandwidth_dimension+1)
            bandwidth_dimension += 1
            continue

print "Should never get here!"
#@-node:gcross.20091022120112.1463:<< Run sweeps >>
#@nl
#@-node:gcross.20091022120112.1458:@thin gadget-normdiff.py
#@-leo
