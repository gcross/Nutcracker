#@+leo-ver=4-thin
#@+node:gcross.20091008162221.1380:@thin simulation.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20091008162221.1400:<< Import needed modules >>
from numpy import zeros
from numpy.linalg import norm

from utils import *
from contractors import *
#@-node:gcross.20091008162221.1400:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091008162221.1382:Classes
#@+node:gcross.20091008162221.1377:TensorChainState
class TensorChainState(object):
    #@    @+others
    #@+node:gcross.20091008162221.1374:__slots__
    __slots__ = ["number_of_sites","active_site_number","tensors_left_of_active_site","active_site_tensor","tensors_right_of_active_site"]
    #@-node:gcross.20091008162221.1374:__slots__
    #@+node:gcross.20091008162221.1378:__init__
    def __init__(self,tensors_left_of_active_site,active_site_tensor,tensors_right_of_active_site):
        self.number_of_sites = len(tensors_left_of_active_site)+1+len(tensors_right_of_active_site)
        self.active_site_number = len(tensors_left_of_active_site)
        self.tensors_left_of_active_site = tensors_left_of_active_site
        self.active_site_tensor = active_site_tensor
        self.tensors_right_of_active_site = list(reversed(tensors_right_of_active_site))
    #@-node:gcross.20091008162221.1378:__init__
    #@+node:gcross.20091008162221.1390:from_list
    @classmethod
    def from_list(cls,list_of_tensors):
        return cls([],list_of_tensors[0],list_of_tensors[1:])
    #@-node:gcross.20091008162221.1390:from_list
    #@+node:gcross.20091008162221.1391:to_list
    def to_list(self):
        return self.tensors_left_of_active_site + [self.active_site_tensor] + list(reversed(self.tensors_right_of_active_site))
    #@-node:gcross.20091008162221.1391:to_list
    #@+node:gcross.20091008162221.1392:normalize_active_site_in_preparation_for_moving_XXX
    def normalize_active_site_in_preparation_for_moving_left(self):
        self.active_site_tensor, self.tensors_left_of_active_site[-1] = \
            normalize_and_denormalize(self.active_site_tensor,1,self.tensors_left_of_active_site[-1],2)

    def normalize_active_site_in_preparation_for_moving_right(self):
        self.active_site_tensor, self.tensors_right_of_active_site[-1] = \
            normalize_and_denormalize(self.active_site_tensor,2,self.tensors_right_of_active_site[-1],1)
    #@-node:gcross.20091008162221.1392:normalize_active_site_in_preparation_for_moving_XXX
    #@+node:gcross.20091008162221.1401:move_active_site_XXX
    def move_active_site_left(self):
        self.tensors_right_of_active_site.append(self.active_site_tensor)
        self.active_site_tensor = self.tensors_left_of_active_site.pop()

    def move_active_site_right(self):
        self.tensors_left_of_active_site.append(self.active_site_tensor)
        self.active_site_tensor = self.tensors_right_of_active_site.pop()
    #@-node:gcross.20091008162221.1401:move_active_site_XXX
    #@+node:gcross.20091009120817.1401:to_tensor
    def to_tensor(self):
        return reduce(dot,self.to_list()).squeeze()
    #@-node:gcross.20091009120817.1401:to_tensor
    #@-others
#@-node:gcross.20091008162221.1377:TensorChainState
#@+node:gcross.20091008162221.1381:Simulation
class Simulation(object):
    #@    @+others
    #@+node:gcross.20091008162221.1383:__slots__
    __slots__ = [
        "number_of_sites",
        "physical_dimension",
        "operator_site_tensors",
        "orthogonal_state_information_list",
        "state",
        "old_state_site_tensors",
        "expectation_chain",
        "projection_chain",
        "active_site_number",
        "energy"
    ]
    #@-node:gcross.20091008162221.1383:__slots__
    #@+node:gcross.20091008162221.1398:__init__
    def __init__(self,number_of_sites,physical_dimension,bandwidth_dimension,operator_site_tensors,orthogonal_state_information_list=[]):
        self.number_of_sites = number_of_sites
        self.active_site_number = 0
        self.physical_dimension = physical_dimension
        self.operator_site_tensors = operator_site_tensors

        self.orthogonal_state_information_list = orthogonal_state_information_list
        self.reinitialize_chains(bandwidth_dimension)

        self.energy = self.compute_energy()
    #@-node:gcross.20091008162221.1398:__init__
    #@+node:gcross.20091008162221.1399:reinitialize_chains
    def reinitialize_chains(self,bandwidth_dimension):
        state_site_tensors = create_normalized_state_site_tensors(self.physical_dimension,bandwidth_dimension,self.number_of_sites)

        projection_chain = ChainProjector(state_site_tensors,self.orthogonal_state_information_list)
        projection_chain.orthogonalize(state_site_tensors)

        self.state = TensorChainState.from_list(state_site_tensors)
        self.expectation_chain = ChainContractorForExpectations(state_site_tensors,self.operator_site_tensors)
        self.projection_chain = projection_chain

        self.energy = self.compute_energy()
    #@-node:gcross.20091008162221.1399:reinitialize_chains
    #@+node:gcross.20091008162221.1384:optimize_active_site
    def optimize_active_site(self):

        projector = self.projection_chain.construct_projector()

        try:
            new_site_tensor, new_energy = self.expectation_chain.compute_optimized_state_site_tensor(self.state.active_site_tensor,projector)

            if new_energy-self.energy > 1e-7:
                raise ConvergenceError("Variational breakdown: {0} > {1}".format(new_energy,self.energy))

            projector(new_site_tensor)
            n = norm(new_site_tensor)
            if n < 1e-10:
                raise ConvergenceError("Minimized state vector vanished after post-projection (norm = {0})".format(n))

            self.state.active_site_tensor = new_site_tensor
            self.energy = new_energy

        except ConvergenceError, e:
            new_site_tensor = self.state.active_site_tensor.copy()
            projector(new_site_tensor)
            n = norm(new_site_tensor)
            if n < 1e-10:
                raise ConvergenceError("Current state vector vanishes after post-projection (norm = {0})".format(n))
            else:
                self.state.active_site_tensor = new_site_tensor
                self.energy = self.compute_energy()
                raise
    #@-node:gcross.20091008162221.1384:optimize_active_site
    #@+node:gcross.20091008162221.1385:move_active_site_XXX
    def move_active_site_left(self):
        self.state.normalize_active_site_in_preparation_for_moving_left()

        active_site_tensor = self.state.active_site_tensor
        self.expectation_chain.contract_and_move_left(active_site_tensor.conj(),active_site_tensor)
        self.projection_chain.contract_and_move_left(active_site_tensor)

        self.state.move_active_site_left()

        self.active_site_number -= 1

    def move_active_site_right(self):
        self.state.normalize_active_site_in_preparation_for_moving_right()

        active_site_tensor = self.state.active_site_tensor
        self.expectation_chain.contract_and_move_right(active_site_tensor.conj(),active_site_tensor)
        self.projection_chain.contract_and_move_right(active_site_tensor)

        self.state.move_active_site_right()

        self.active_site_number += 1
    #@-node:gcross.20091008162221.1385:move_active_site_XXX
    #@+node:gcross.20091008162221.1387:move_active_site_to_leftmost_site
    def move_active_site_to_leftmost_site(self):
        while self.active_site_number > 0:
            self.move_active_site_left()
    #@-node:gcross.20091008162221.1387:move_active_site_to_leftmost_site
    #@+node:gcross.20091008162221.1386:add_old_state_to_projectors_and_reset
    def add_old_state_to_projectors_and_reset(self,new_bandwidth_dimension):
        self.move_active_site_to_leftmost_site()
        self.orthogonal_state_information_list.append(convert_old_state_tensors_to_orthogonal_state_information(self.old_state_site_tensors))
        self.reinitialize_chains(new_bandwidth_dimension)
    #@-node:gcross.20091008162221.1386:add_old_state_to_projectors_and_reset
    #@+node:gcross.20091008162221.1397:compute_energy
    def compute_energy(self):
        return real(self.expectation_chain.fully_contract_with_state_site_tensor(self.state.active_site_tensor.conj(),self.state.active_site_tensor))
    #@-node:gcross.20091008162221.1397:compute_energy
    #@+node:gcross.20091009120817.4105:save_current_and_increase_bandwidth_dimension_to
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
    #@-node:gcross.20091009120817.4105:save_current_and_increase_bandwidth_dimension_to
    #@+node:gcross.20091009120817.4108:restart
    def restart(self,bandwidth_dimension):
        self.active_site_number = 0
        self.reinitialize_chains(bandwidth_dimension)
    #@-node:gcross.20091009120817.4108:restart
    #@-others
#@-node:gcross.20091008162221.1381:Simulation
#@-node:gcross.20091008162221.1382:Classes
#@+node:gcross.20091012135649.1410:Functions
#@+node:gcross.20091012135649.1411:run_simulation
def run_simulation(
  number_of_sites,
  physical_dimension,
  starting_bandwidth_dimension,
  operator_site_tensors,
  orthogonal_state_information_list=[],
  sweep_callback=None
  ):
    simulation = Simulation(number_of_sites,physical_dimension,starting_bandwidth_dimension,operator_site_tensors,orthogonal_state_information_list)

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
    previous_bandwidth_energy = 1e100
    while previous_bandwidth_energy-simulation.energy > 1e-7:
        bandwidth_dimension += 1
        if bandwidth_dimension > starting_bandwidth_dimension:
            simulation.save_current_and_increase_bandwidth_dimension_to(bandwidth_dimension)
        previous_bandwidth_energy = simulation.energy

        number_of_sites_skipped_in_a_row = 0
        sweep_number = 0
        previous_sweep_energy = 1e100
        while previous_sweep_energy-simulation.energy > 1e-7:
            sweep_number += 1
            if sweep_number > 15:
                optimize.number_of_sites_skipped_in_a_row = 0
                sweep_number = 0
                previous_sweep_energy = 1e100
                simulation.restart(bandwidth_dimension)
                continue
            previous_sweep_energy = simulation.energy

            try:
                for site_number in xrange(number_of_sites-1):
                    optimize(simulation.move_active_site_right)

                for site_number in xrange(number_of_sites-1,0,-1):
                    optimize(simulation.move_active_site_left)

                if sweep_callback:
                    sweep_callback(bandwidth_dimension,sweep_number,simulation.energy)
            except ConvergenceError:
                optimize.number_of_sites_skipped_in_a_row = 0
                sweep_number = 0
                previous_sweep_energy = 1e100
                simulation.restart(bandwidth_dimension)

    return simulation.energy, simulation.old_state_site_tensors
#@-node:gcross.20091012135649.1411:run_simulation
#@+node:gcross.20091014143858.1489:compute_level
def compute_level(
  number_of_sites,
  physical_dimension,
  starting_bandwidth_dimension,
  operator_site_tensors,
  orthogonal_state_information_list=[],
  number_of_occurances_needed=3,
  sweep_callback=None,
  trial_callback=None,
  ):
    lowest_trial_energy = 1e100
    number_of_occurances = 0
    while number_of_occurances < number_of_occurances_needed:
        trial_energy, trial_state_site_tensors =\
            run_simulation(
                number_of_sites,
                physical_dimension,
                starting_bandwidth_dimension,
                operator_site_tensors,
                orthogonal_state_information_list,
                sweep_callback
            )
        if abs(lowest_trial_energy-trial_energy) < 1e-7:
            number_of_occurances += 1
        elif lowest_trial_energy-trial_energy > 1e-7:
            lowest_trial_energy = trial_energy
            lowest_trial_state_site_tensors = trial_state_site_tensors
            number_of_occurances = 1
        if trial_callback:
            trial_callback(trial_energy,lowest_trial_energy,number_of_occurances,number_of_occurances_needed)
    return lowest_trial_energy, lowest_trial_state_site_tensors
#@-node:gcross.20091014143858.1489:compute_level
#@-node:gcross.20091012135649.1410:Functions
#@-others

__all__ = ["run_simulation"]
#@-node:gcross.20091008162221.1380:@thin simulation.py
#@-leo
