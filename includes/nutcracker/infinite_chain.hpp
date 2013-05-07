#ifndef NUTCRACKER_INFINITE_CHAIN_HPP
#define NUTCRACKER_INFINITE_CHAIN_HPP

#include <algorithm>
#include <limits>
#include <boost/optional.hpp>

#include "nutcracker/base_chain.hpp"
#include "nutcracker/boundaries.hpp"

namespace Nutcracker {

class InfiniteChain: public BaseChain { // {{{
protected:
    OperatorSite const& operator_site;
public:
    template<typename RangeType> InfiniteChain( // {{{
        BOOST_RV_REF(OperatorSite) operator_site,
        RangeType left_boundary,
        RangeType right_boundary,
        boost::optional<ChainOptions const&> maybe_options = boost::none
    ) : BaseChain(
            ExpectationBoundary<Left>(
                OperatorDimension(operator_site.leftDimension()),
                StateDimension(1)
            ),
            ExpectationBoundary<Right>(
                OperatorDimension(operator_site.rightDimension()),
                StateDimension(1)
            ),
            randomStateSiteMiddle(
                operator_site.physicalDimension(as_dimension),
                LeftDimension(1),
                RightDimension(1)
            ),
            maybe_options
        )
      , operator_site(operator_site)
    {
        std::copy(left_boundary.begin(),left_boundary.end(),left_expectation_boundary.begin());
        std::copy(right_boundary.begin(),right_boundary.end(),right_expectation_boundary.begin());
    }
    // }}}

    template<typename side> void move();
    unsigned int bandwidthDimension() const { return state_site.leftDimension(); }
    virtual OperatorSite const& getCurrentOperatorSite() const { return operator_site; }
    virtual ProjectorMatrix const& getCurrentProjectorMatrix() const { return ProjectorMatrix::getNull(); }
    virtual unsigned int getCurrentBandwidthDimension() const { return left_expectation_boundary.stateDimension(); }
    virtual unsigned int getMaximumBandwidthDimension() const { return std::numeric_limits<unsigned int>::max(); }
    virtual void performOptimizationSweep();
    virtual void increaseBandwidthDimension(unsigned int const new_bandwidth_dimension);

}; // }}}

// External methods {{{

template<typename side> void InfiniteChain::move() {{{
    optimized = false;

    ExpectationBoundary<side>& expectation_boundary = expectationBoundary<side>();
    ExpectationBoundary<side> new_expectation_boundary(
        contract<side>::SOS_absorb(
             expectation_boundary
            ,state_site
            ,operator_site
        )
    );
    expectationBoundary<side>() = boost::move(new_expectation_boundary);
}}}

// }}}

}

#endif
