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
        OperatorSite operator_site,
        RangeType left_boundary,
        RangeType right_boundary,
        boost::optional<ChainOptions const&> maybe_options = boost::none
    ) : BaseChain(maybe_options)
      , operator_site(copyFrom(operator_site))
      , left_expectation_boundary(
            OperatorDimension(operator_site.left_dimension),
            StateDimension(1)
        )
      , right_expectation_boundary(
            OperatorDimension(operator_site.right_dimension),
            StateDimension(1)
        )
      , state_site(randomSiteMiddle(
            operator_site.physicalDimension(as_dimension),
            LeftDimension(1),
            RightDimension(1)
        )
    {
        std::copy(left_boundary.begin(),left_boundary.end(),left_expectation_boundary.begin());
        std::copy(right_boundary.begin(),right_boundary.end(),right_expectation_boundary.begin());
    }
    // }}}

}; // }}}

#endif
