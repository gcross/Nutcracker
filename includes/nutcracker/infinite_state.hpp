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
    StateBoundary<Left> left_boundary;
    StateSite<Middle> middle_site;
    StateBoundary<Right> right_boundary;

    InfiniteState(BOOST_RV_REF(InfiniteState) other) // {{{
      : left_boundary(boost::move(other.left_boundary))
      , middle_site(boost::move(other.middle_site))
      , right_boundary(boost::move(other.right_boundary))
    { } // }}}

    InfiniteState( // BOOST_RV_REF(all fields) {{{
          BOOST_RV_REF(StateBoundary<Left>) left_boundary
        , BOOST_RV_REF(StateSite<Middle>) middle_site
        , BOOST_RV_REF(StateBoundary<Right>) right_boundary
    ) : left_boundary(left_boundary)
      , middle_site(middle_site)
      , right_boundary(right_boundary)
    { } // }}}

    void operator=(BOOST_RV_REF(InfiniteState) other) { // {{{
        left_boundary = boost::move(other.left_boundary);
        middle_site = boost::move(other.middle_site);
        right_boundary = boost::move(other.right_boundary);
    } // }}}

    unsigned int physicalDimension() const { return middle_site.physicalDimension(); }
    PhysicalDimension physicalDimension(AsDimension const _) const { return middle_site.physicalDimension(as_dimension); }

    unsigned int stateDimension() const { return left_boundary.stateDimension(); }
    StateDimension stateDimension(AsDimension const _) const { return left_boundary.stateDimension(as_dimension); }
}; // }}}

}

#endif
