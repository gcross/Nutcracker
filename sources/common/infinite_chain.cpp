#include "nutcracker/core.hpp"
#include "nutcracker/infinite_chain.hpp"
#include "nutcracker/states.hpp"

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
    while(bandwidthDimension() < initial_bandwidth_dimension) {
        increaseBandwidthDimension(min(state_site.physicalDimension()*bandwidthDimension(),initial_bandwidth_dimension));
    }
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

    StateSite<Left> current_left_site(normalizeLeft(state_site));
    StateSite<Middle> current_middle_site;
    StateSite<Right> current_right_site(normalizeRight(state_site));

    {
        IncreaseDimensionBetweenResult<Left,Middle> left_middle(
            Unsafe::increaseDimensionBetween<Left,Middle>(
                new_bandwidth_dimension,
                current_left_site,
                state_site
            )
        );
        current_left_site = boost::move(left_middle.state_site_1);
        current_middle_site = boost::move(left_middle.state_site_2);
    }

    {
        IncreaseDimensionBetweenResult<Middle,Right> middle_right(
            Unsafe::increaseDimensionBetween<Middle,Right>(
                new_bandwidth_dimension,
                current_middle_site,
                normalizeRight(state_site)
            )
        );
        current_middle_site = boost::move(middle_right.state_site_1);
        current_right_site = boost::move(middle_right.state_site_2);
    }

    {
        MoveSiteCursorResult<Right> left_result(
            Unsafe::moveSiteCursorRight(
                current_left_site,
                current_middle_site
            )
        );
        current_left_site = boost::move(left_result.other_side_state_site);
        current_middle_site = boost::move(left_result.middle_state_site);
    }

    {
        MoveSiteCursorResult<Left> right_result(
            Unsafe::moveSiteCursorLeft(
                current_middle_site,
                current_right_site
            )
        );
        current_middle_site = boost::move(right_result.middle_state_site);
        current_right_site = boost::move(right_result.other_side_state_site);
    }

    left_expectation_boundary =
        contract<Left>::SOS(
            left_expectation_boundary,
            current_left_site,
            operator_site
        );

    right_expectation_boundary =
        contract<Right>::SOS(
            right_expectation_boundary,
            current_right_site,
            operator_site
        );

    state_site = boost::move(current_middle_site);
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
