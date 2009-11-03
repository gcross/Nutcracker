#@+leo-ver=4-thin
#@+node:gcross.20091030152136.1545:@thin simulate-gadget7.py
#@@language Python

from simulate_gadget_common import *

left_operator_site_tensor = zeros((2,2,1,14),complex128)
left_operator_site_tensor[...,0,0] = -perturbation_coefficient*X/2
left_operator_site_tensor[...,0,13] = I
left_operator_site_tensor[...,0,4] = I
left_operator_site_tensor[...,0,8] = I

middle_operator_site_tensor = zeros((2,2,14,14),complex128)
middle_operator_site_tensor[..., 0,12] = I
middle_operator_site_tensor[...,12, 0] = I
middle_operator_site_tensor[..., 0, 1] = X
middle_operator_site_tensor[..., 1, 1] = I
middle_operator_site_tensor[..., 2,13] = I
middle_operator_site_tensor[...,13, 2] = I
middle_operator_site_tensor[..., 2, 3] = X
middle_operator_site_tensor[..., 3, 3] = I
middle_operator_site_tensor[..., 4, 7] = I
middle_operator_site_tensor[..., 7, 4] = I
middle_operator_site_tensor[..., 4, 5] = -Z
middle_operator_site_tensor[..., 5, 6] = Z
middle_operator_site_tensor[..., 8,11] = I
middle_operator_site_tensor[...,11, 8] = I
middle_operator_site_tensor[..., 8, 9] = -X
middle_operator_site_tensor[..., 9,10] = X

right_operator_site_tensor = zeros((2,2,14,1),complex128)
right_operator_site_tensor[..., 1, 0] = I
right_operator_site_tensor[..., 3, 0] = -perturbation_coefficient*X/2
right_operator_site_tensor[..., 4, 0] = 0
right_operator_site_tensor[..., 6, 0] = I
right_operator_site_tensor[...,10, 0] = I

operator_site_tensors = [left_operator_site_tensor] + [middle_operator_site_tensor]*(number_of_sites-2) + [right_operator_site_tensor]
#@-node:gcross.20091030152136.1545:@thin simulate-gadget7.py
#@-leo
