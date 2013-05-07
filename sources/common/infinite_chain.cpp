#include "nutcracker/core.hpp"
#include "nutcracker/infinite_chain.hpp"

namespace Nutcracker {

void InfiniteChain::increaseBandwidthDimension(unsigned int const new_bandwidth_dimension) {{{
    optimized = false;
    StateDimension const
        old_state_dimension = left_expectation_boundary.stateDimension(as_dimension),
        new_state_dimension(new_bandwidth_dimension);
    assert(*new_state_dimension > *old_state_dimension);
    OperatorDimension const operator_dimension = left_expectation_boundary.operatorDimension(as_dimension);
    PhysicalDimension const physical_dimension = state_site.physicalDimension(as_dimension);

    StateSite<Middle> new_state_site(physical_dimension,LeftDimension(*new_state_dimension),RightDimension(*new_state_dimension));
    ExpectationBoundary<Left> new_left_expectation_boundary(operator_dimension,new_state_dimension);
    ExpectationBoundary<Right> new_right_expectation_boundary(operator_dimension,new_state_dimension);

    Core::increase_bandwidth_with_environment(
        *old_state_dimension,
        *operator_dimension,
        state_site.physicalDimension(),
        *new_state_dimension,
        state_site,
        left_expectation_boundary,
        right_expectation_boundary,
        new_state_site,
        new_left_expectation_boundary,
        new_right_expectation_boundary
    );

    state_site = boost::move(new_state_site);
    left_expectation_boundary = boost::move(left_expectation_boundary);
    right_expectation_boundary = boost::move(right_expectation_boundary);
}}}

void InfiniteChain::performOptimizationSweep() {{{
    if(!optimized) optimizeSite();
    move<Right>();
    optimizeSite();
    move<Left>();
    optimizeSite();
    signalSweepPerformed();
}}}

}
