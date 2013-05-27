#ifndef NUTCRACKER_INFINITE_STATE_HPP
#define NUTCRACKER_INFINITE_STATE_HPP

// Includes {{{
#include <boost/move/move.hpp>

#include "nutcracker/tensors.hpp"
// }}}

namespace Nutcracker {

struct InfiniteState { // {{{
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(InfiniteState)
public:
    StateBoundary<Left> left_state_boundary;
    StateSite<Middle> middle_state_site;
    StateBoundary<Right> right_state_boundary;

    InfiniteState(BOOST_RV_REF(InfiniteState) other) // {{{
      : left_state_boundary(boost::move(other.left_state_boundary))
      , middle_state_site(boost::move(other.middle_state_site))
      , right_state_boundary(boost::move(other.right_state_boundary))
    { } // }}}

    InfiniteState( // BOOST_RV_REF(all fields) {{{
          BOOST_RV_REF(StateBoundary<Left>) left_state_boundary
        , BOOST_RV_REF(StateSite<Middle>) middle_state_site
        , BOOST_RV_REF(StateBoundary<Right>) right_state_boundary
    ) : left_state_boundary(left_state_boundary)
      , middle_state_site(middle_state_site)
      , right_state_boundary(right_state_boundary)
    { } // }}}

    void operator=(BOOST_RV_REF(InfiniteState) other) { // {{{
        left_state_boundary = boost::move(other.left_state_boundary);
        middle_state_site = boost::move(other.middle_state_site);
        right_state_boundary = boost::move(other.right_state_boundary);
    } // }}}

    unsigned int physicalDimension() const { return middle_state_site.physicalDimension(); }
    PhysicalDimension physicalDimension(AsDimension const _) const { return middle_state_site.physicalDimension(as_dimension); }

    unsigned int stateDimension() const { return left_state_boundary.stateDimension(); }
    StateDimension stateDimension(AsDimension const _) const { return left_state_boundary.stateDimension(as_dimension); }
}; // }}}

}

#endif
