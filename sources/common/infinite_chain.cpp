#include "nutcracker/core.hpp"
#include "nutcracker/infinite_chain.hpp"

namespace Nutcracker {

// Usings {{{
using boost::none;
// }}}

// Constructors {{{
InfiniteChain::InfiniteChain(
    BOOST_RV_REF(InfiniteOperator) op,
    boost::optional<ChainOptions const&> maybe_options
) : BaseChain(
        ExpectationBoundary<Left>(boost::move(op.left_boundary)),
        ExpectationBoundary<Right>(boost::move(op.right_boundary)),
        randomStateSiteMiddle(
            op.middle_site.physicalDimension(as_dimension),
            LeftDimension(1),
            RightDimension(1)
        ),
        maybe_options
    )
  , operator_site(boost::move(op.middle_site))
{
    increaseBandwidthDimension(initial_bandwidth_dimension);
}

InfiniteChain::InfiniteChain(
    BOOST_RV_REF(InfiniteOperator) op,
    BOOST_RV_REF(InfiniteState) state,
    boost::optional<ChainOptions const&> maybe_options
) : BaseChain(
        constructExpectationBoundary(
            state.left_boundary,
            op.left_boundary
        ),
        constructExpectationBoundary(
            state.right_boundary,
            op.right_boundary
        ),
        state.middle_site,
        maybe_options
    )
  , operator_site(boost::move(op.middle_site))
{ }
// }}}

void InfiniteChain::dump() const {{{
    BaseChain::dump();
    std::cerr << "OSM:";  BOOST_FOREACH(complex<double> const& x, operator_site) { std::cerr << ' ' << x; } std::cerr << std::endl;
    std::cerr << "OSI:";  uint32_t const* p = operator_site; for(size_t i = 0; i < 2*operator_site.numberOfMatrices(); ++i, ++p) { std::cerr << ' ' << (*p); }  std::cerr << std::endl;
}}}

void InfiniteChain::increaseBandwidthDimension(unsigned int const new_bandwidth_dimension) {{{
    assert(new_bandwidth_dimension >= bandwidthDimension());
    if(new_bandwidth_dimension == bandwidthDimension()) return;

    optimized = false;
    maybe_convergence_energy = none;
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
    left_expectation_boundary = boost::move(new_left_expectation_boundary);
    right_expectation_boundary = boost::move(new_right_expectation_boundary);
}}}

void InfiniteChain::performOptimizationSweep() {{{
    if(!optimized) optimizeSite();
    move<Right>();
    optimizeSite();
    double const old_energy = getEnergy();
    move<Left>();
    optimizeSite();
    double const new_energy = getEnergy();
    maybe_convergence_energy = new_energy - old_energy;
    signalSweepPerformed();
}}}

}
