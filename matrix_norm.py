#@+leo-ver=4-thin
#@+node:gcross.20091020183902.1500:@thin matrix_norm.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20091020183902.1501:<< Import needed modules >>
from itertools import izip
from numpy import zeros
from numpy.linalg import norm

from utils import *
from contractors import *
#@-node:gcross.20091020183902.1501:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091020183902.1502:Classes
#@+node:gcross.20091020183902.1490:NormMaximizer
class NormMaximizer(object):
    #@    @+others
    #@+node:gcross.20091020183902.1491:__slots__
    __slots__ = [
        "number_of_sites",
        "physical_dimension",
        "weights",
        "orthogonal_state_information_list",
        "state",
        "old_state_site_tensors",
        "projector_terms",
        "active_site_number",
        "norm"
    ]
    #@-node:gcross.20091020183902.1491:__slots__
    #@+node:gcross.20091020183902.1492:__init__
    def __init__(self,number_of_sites,physical_dimension,bandwidth_dimension,weights,orthogonal_state_information_list):
        self.number_of_sites = number_of_sites
        self.active_site_number = 0
        self.physical_dimension = physical_dimension

        self.weights = weights
        self.orthogonal_state_information_list = orthogonal_state_information_list
        self.reinitialize_chains(bandwidth_dimension)

        self.norm = self.compute_norm()
    #@-node:gcross.20091020183902.1492:__init__
    #@+node:gcross.20091020183902.1493:reinitialize_chains
    def reinitialize_chains(self,bandwidth_dimension):
        state_site_tensors = create_normalized_state_site_tensors(self.physical_dimension,bandwidth_dimension,self.number_of_sites)

        self.projector_terms = [
            ChainContractorForProjector(state_site_tensors,*orthogonal_state_information)
            for orthogonal_state_information in self.orthogonal_state_information_list
        ]

        self.state = TensorChainState.from_list(state_site_tensors)

        self.norm = self.compute_norm()
    #@-node:gcross.20091020183902.1493:reinitialize_chains
    #@+node:gcross.20091020183902.1494:optimize_active_site
    def optimize_active_site(self):
        try:
            state_site_tensor_shape = self.state.active_site_tensor.shape

            new_site_tensor = sum(weight*projector_term.compute_projector() for weight, projector_term in izip(self.weights,self.projector_terms)).conj()

            new_site_tensor /= dot(new_site_tensor.ravel().conj(),new_site_tensor.ravel())
            new_norm = self.compute_norm_using_active_site_tensor(new_site_tensor)

            if self.norm-new_norm > 1e-7:
                raise ConvergenceError("Variational breakdown: {0} < {1}".format(new_norm,self.norm))

            n = norm(new_site_tensor)
            if n < 1e-10:
                raise ConvergenceError("Minimized state vector vanished after post-projection (norm = {0})".format(n))

            self.state.active_site_tensor = new_site_tensor
            self.norm = new_norm

        except ConvergenceError, e:
            new_site_tensor = self.state.active_site_tensor.copy()
            n = norm(new_site_tensor)
            if n < 1e-13:
                raise ConvergenceError("Current state vector vanishes after post-projection (norm = {0})".format(n))
            else:
                self.state.active_site_tensor = new_site_tensor
                self.norm = self.compute_norm()
                raise
    #@-node:gcross.20091020183902.1494:optimize_active_site
    #@+node:gcross.20091020183902.1495:move_active_site_XXX
    def move_active_site_left(self):
        self.state.normalize_active_site_in_preparation_for_moving_left()

        active_site_tensor = self.state.active_site_tensor
        for term in self.projector_terms:
            term.contract_and_move_left(active_site_tensor)

        self.state.move_active_site_left()

        self.active_site_number -= 1

    def move_active_site_right(self):
        self.state.normalize_active_site_in_preparation_for_moving_right()

        active_site_tensor = self.state.active_site_tensor
        for term in self.projector_terms:
            term.contract_and_move_right(active_site_tensor)

        self.state.move_active_site_right()

        self.active_site_number += 1
    #@-node:gcross.20091020183902.1495:move_active_site_XXX
    #@+node:gcross.20091020183902.1496:move_active_site_to_leftmost_site
    def move_active_site_to_leftmost_site(self):
        while self.active_site_number > 0:
            self.move_active_site_left()
    #@-node:gcross.20091020183902.1496:move_active_site_to_leftmost_site
    #@+node:gcross.20091020183902.1497:compute_norm
    def compute_norm(self):
        state_site_tensor = self.state.active_site_tensor
        return self.compute_norm_using_active_site_tensor(self.state.active_site_tensor)
    #@-node:gcross.20091020183902.1497:compute_norm
    #@+node:gcross.20091020183902.1542:compute_norm_using_active_site_tensor
    def compute_norm_using_active_site_tensor(self,active_site_tensor):
        return sum(weight*projector_term.compute_overlap(active_site_tensor) for weight, projector_term in izip(self.weights,self.projector_terms))
    #@-node:gcross.20091020183902.1542:compute_norm_using_active_site_tensor
    #@+node:gcross.20091020183902.1498:save_current_and_increase_bandwidth_dimension_to
    def save_current_and_increase_bandwidth_dimension_to(self,new_bandwidth_dimension):
        old_active_site_number = self.active_site_number
        self.move_active_site_to_leftmost_site()

        self.old_state_site_tensors = self.state.to_list()

        physical_dimension = self.physical_dimension
        bandwidth_dimension_sequence = compute_bandwidth_dimension_sequence(physical_dimension,new_bandwidth_dimension,self.number_of_sites)

        state = self.state
        for bandwidth_dimension in bandwidth_dimension_sequence[1:-1]:
            active_site_tensor = state.active_site_tensor
            current_bandwidth_dimension = active_site_tensor.shape[2]
            if current_bandwidth_dimension < bandwidth_dimension:
                new_active_site_tensor = zeros((physical_dimension,active_site_tensor.shape[1],bandwidth_dimension),complex128)
                new_active_site_tensor[:,:,:current_bandwidth_dimension] = active_site_tensor

                right_of_active_site_tensor = state.tensors_right_of_active_site[-1]
                new_right_of_active_site_tensor = zeros((physical_dimension,bandwidth_dimension,right_of_active_site_tensor.shape[2]),complex128)
                new_right_of_active_site_tensor[:,:right_of_active_site_tensor.shape[1],:] = right_of_active_site_tensor

                state.active_site_tensor, state.tensors_right_of_active_site[-1] = normalize_and_denormalize(new_active_site_tensor,2,new_right_of_active_site_tensor,1)
            self.move_active_site_right()

        while self.active_site_number > old_active_site_number:
            self.move_active_site_left()
    #@-node:gcross.20091020183902.1498:save_current_and_increase_bandwidth_dimension_to
    #@+node:gcross.20091020183902.1499:restart
    def restart(self,bandwidth_dimension):
        self.active_site_number = 0
        self.reinitialize_chains(bandwidth_dimension)
    #@-node:gcross.20091020183902.1499:restart
    #@-others
#@-node:gcross.20091020183902.1490:NormMaximizer
#@-node:gcross.20091020183902.1502:Classes
#@+node:gcross.20091020183902.1504:compute_norm_maximizer
def compute_norm_maximizer(
  number_of_sites,
  physical_dimension,
  starting_bandwidth_dimension,
  eigenvalues,
  orthogonal_state_information_list=[],
  sweep_callback=None
  ):
    simulation = NormMaximizer(number_of_sites,physical_dimension,starting_bandwidth_dimension,eigenvalues,orthogonal_state_information_list)
    simulation.state.active_site_tensor /= dot(simulation.state.active_site_tensor.ravel().conj(),simulation.state.active_site_tensor.ravel())

    def optimize(simulation_move_function):
        try:
            simulation.optimize_active_site()
            optimize.number_of_sites_skipped_in_a_row = 0 
        except ConvergenceError, e:
            optimize.number_of_sites_skipped_in_a_row += 1
            if optimize.number_of_sites_skipped_in_a_row >= number_of_sites:
                raise
        simulation_move_function()
    optimize.number_of_sites_skipped_in_a_row = 0

    bandwidth_dimension = starting_bandwidth_dimension-1

    sweep_number = 0
    previous_bandwidth_norm = -1
    while simulation.norm-previous_bandwidth_norm > 1e-7:
        bandwidth_dimension += 1
        if bandwidth_dimension > starting_bandwidth_dimension:
            simulation.save_current_and_increase_bandwidth_dimension_to(bandwidth_dimension)
        previous_bandwidth_norm = simulation.norm

        number_of_sites_skipped_in_a_row = 0
        sweep_number = 0
        previous_sweep_norm = -1
        while simulation.norm-previous_sweep_norm > 1e-7:
            sweep_number += 1
            if sweep_number > 7:
                optimize.number_of_sites_skipped_in_a_row = 0
                sweep_number = 0
                previous_sweep_norm = -1
                simulation.restart(bandwidth_dimension)
                continue
            previous_sweep_norm = simulation.norm

            try:
                for site_number in xrange(number_of_sites-1):
                    optimize(simulation.move_active_site_right)

                for site_number in xrange(number_of_sites-1,0,-1):
                    optimize(simulation.move_active_site_left)

                if sweep_callback:
                    sweep_callback(bandwidth_dimension,sweep_number,simulation.norm)
            except ConvergenceError:
                optimize.number_of_sites_skipped_in_a_row = 0
                sweep_number = 0
                previous_sweep_norm = -1
                simulation.restart(bandwidth_dimension)

    return simulation.norm, simulation.old_state_site_tensors
#@-node:gcross.20091020183902.1504:compute_norm_maximizer
#@-others
#@-node:gcross.20091020183902.1500:@thin matrix_norm.py
#@-leo
