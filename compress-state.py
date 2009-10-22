#@+leo-ver=4-thin
#@+node:gcross.20091020183902.1512:@thin compress-state.py
#@@language Python

import warnings
warnings.simplefilter("ignore",DeprecationWarning)

#@<< Import needed modules >>
#@+node:gcross.20091020183902.1513:<< Import needed modules >>
from numpy import array, zeros, identity, complex128
import sys

from database import VMPSDatabase
from paulis import *
from matrix_norm import compute_norm_maximizer
from utils import convert_old_state_tensors_to_orthogonal_state_information, peak_bandwidth_of, ConvergenceError
#@-node:gcross.20091020183902.1513:<< Import needed modules >>
#@nl

#@+others
#@-others

#@<< Read simulation parameters >>
#@+node:gcross.20091020183902.1514:<< Read simulation parameters >>
try:
    state_id = sys.argv[1]
except:
    print "USAGE:  {0} <state id>".format(sys.argv[0])
    sys.exit(1)
#@-node:gcross.20091020183902.1514:<< Read simulation parameters >>
#@nl

#@<< Define parameters >>
#@+node:gcross.20091020183902.1515:<< Define parameters >>
bandwidth_dimension = 2
#@-node:gcross.20091020183902.1515:<< Define parameters >>
#@nl

#@<< Read state >>
#@+node:gcross.20091020183902.1516:<< Read state >>
database = VMPSDatabase("reader")
state_site_tensors = database.load_state(state_id)
if len(state_site_tensors) == 0:
    sys.exit("There is no state with ID {state_id}.".format(**vars()))

orthogonal_state_information = convert_old_state_tensors_to_orthogonal_state_information(state_site_tensors)
#@-node:gcross.20091020183902.1516:<< Read state >>
#@nl

#@<< Run sweeps >>
#@+node:gcross.20091020183902.1517:<< Run sweeps >>
def sweep_callback(bandwidth_dimension,sweep_number,energy):
    print "Bandwidth dimension {bandwidth_dimension}, Sweep number {sweep_number}: {energy}".format(**vars())

while bandwidth_dimension < 20:
    try:
        norm, compressed_state_site_tensors = \
            compute_norm_maximizer (
                len(state_site_tensors),
                state_site_tensors[0].shape[0],
                bandwidth_dimension,
                [1],
                [orthogonal_state_information],
                sweep_callback=sweep_callback,
            )

        print "norm =", norm
        print "original bandwidth =", peak_bandwidth_of(state_site_tensors)
        print "compressed bandwidth =", peak_bandwidth_of(compressed_state_site_tensors)

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
#@-node:gcross.20091020183902.1517:<< Run sweeps >>
#@nl
#@-node:gcross.20091020183902.1512:@thin compress-state.py
#@-leo
