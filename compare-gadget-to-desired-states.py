#@+leo-ver=4-thin
#@+node:gcross.20091022120112.1458:@thin compare-gadget-to-desired-states.py
#@@language Python

import warnings
warnings.simplefilter("ignore",DeprecationWarning)

#@<< Import needed modules >>
#@+node:gcross.20091022120112.1459:<< Import needed modules >>
from numpy import array, zeros, identity, complex128, set_printoptions, sqrt, allclose, prod
from numpy.linalg import svd
import sys

from database import VMPSDatabase
from paulis import *
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

#@<< Read solution >>
#@+node:gcross.20091022120112.1462:<< Read solution >>
database = VMPSDatabase("reader")
state_site_tensors_list = zip(*database.load_solution(solution_id))[1]
if len(state_site_tensors_list) == 0:
    sys.exit("There is no solution with ID {solution_id}.".format(**vars()))
#@-node:gcross.20091022120112.1462:<< Read solution >>
#@nl

#@<< Construct desired states >>
#@+node:gcross.20091022120112.1464:<< Construct desired states >>
number_of_sites = len(state_site_tensors_list[0])-2

q_0_tensor = array([1,0],complex128).reshape(2,1,1)
q_1_tensor = array([0,1],complex128).reshape(2,1,1)
q_m_tensor = (array([1,-1],complex128)/sqrt(2)).reshape(2,1,1)
q_p_tensor = (array([1,+1],complex128)/sqrt(2)).reshape(2,1,1)

middle_state_site_tensors = [q_1_tensor]*number_of_sites

predicted_state_site_tensors_list = [
    [q_p_tensor] + middle_state_site_tensors + [q_p_tensor],
    [q_m_tensor] + middle_state_site_tensors + [q_m_tensor],
    [q_m_tensor] + middle_state_site_tensors + [q_p_tensor],
    [q_p_tensor] + middle_state_site_tensors + [q_m_tensor],
]
#@-node:gcross.20091022120112.1464:<< Construct desired states >>
#@nl

#@<< Perform comparison >>
#@+node:gcross.20091022120112.1463:<< Perform comparison >>
set_printoptions(linewidth=132)

overlaps = array(
        [
            [
                abs(compute_state_overlap(predicted_state_site_tensors,state_site_tensors))
                for state_site_tensors in state_site_tensors_list
            ]
            for predicted_state_site_tensors in predicted_state_site_tensors_list
        ]
    )

singular_values = svd(overlaps,compute_uv=False)

infidelity = 1-sqrt(prod(singular_values))
#@-node:gcross.20091022120112.1463:<< Perform comparison >>
#@nl

#@<< Write infidelity to the database >>
#@+node:gcross.20091022161927.1462:<< Write infidelity to the database >>
database = VMPSDatabase("updater")
database.cursor.execute("update gadget_model_1_simulations set infidelity = {infidelity} where solution_id='{solution_id}'".format(**vars()))
database.connection.commit()
#@-node:gcross.20091022161927.1462:<< Write infidelity to the database >>
#@nl
#@-node:gcross.20091022120112.1458:@thin compare-gadget-to-desired-states.py
#@-leo
