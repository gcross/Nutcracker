#@+leo-ver=4-thin
#@+node:gcross.20091026125919.1542:@thin simulate-gadget6.py
#@@language Python

from simulate_gadget_common import *

left_operator_site_tensor = zeros((2,2,1,12),complex128)
left_operator_site_tensor[...,0,0] = -perturbation_coefficient*X/2
left_operator_site_tensor[...,0,2] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,8] = I

middle_operator_site_tensor = zeros((2,2,12,12),complex128)
middle_operator_site_tensor[...,0,0] = I
middle_operator_site_tensor[...,0,1] = X
middle_operator_site_tensor[...,1,1] = I
middle_operator_site_tensor[...,2,2] = I
middle_operator_site_tensor[...,2,3] = X
middle_operator_site_tensor[...,3,3] = I

right_operator_site_tensor = zeros((2,2,12,1),complex128)
right_operator_site_tensor[...,1,0] = I
right_operator_site_tensor[...,3,0] = -perturbation_coefficient*X/2
right_operator_site_tensor[...,5,0] = I
right_operator_site_tensor[...,8,0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
#@-node:gcross.20091026125919.1542:@thin simulate-gadget6.py
#@-leo
