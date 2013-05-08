#ifndef NUTCRACKER_INFINITE_CHAIN_HPP
#define NUTCRACKER_INFINITE_CHAIN_HPP

#include <algorithm>
#include <boost/optional.hpp>

#include "nutcracker/base_chain.hpp"

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

    unsigned int bandwidthDimension() const { return state_site.leftDimension(); }
    virtual OperatorSite const& getCurrentOperatorSite() const { return operator_site; }
    virtual ProjectorMatrix const& getCurrentProjectorMatrix() const { return ProjectorMatrix::getNull(); }

}; // }}}

}

#endif
