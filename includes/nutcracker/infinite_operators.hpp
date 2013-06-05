#ifndef NUTCRACKER_INFINITE_OPERATORS_HPP
#define NUTCRACKER_INFINITE_OPERATORS_HPP

// Includes {{{
#include <boost/move/move.hpp>

#include "nutcracker/tensors.hpp"
// }}}

namespace Nutcracker {

struct InfiniteOperator { // {{{
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(InfiniteOperator)
public:
    OperatorBoundary<Left> left_boundary;
    OperatorSite middle_site;
    OperatorBoundary<Right> right_boundary;

    InfiniteOperator(BOOST_RV_REF(InfiniteOperator) other) // {{{
      : left_boundary(boost::move(other.left_boundary))
      , middle_site(boost::move(other.middle_site))
      , right_boundary(boost::move(other.right_boundary))
    { } // }}}

    InfiniteOperator( // BOOST_RV_REF(all fields) {{{
          BOOST_RV_REF(OperatorBoundary<Left>) left_boundary
        , BOOST_RV_REF(OperatorSite) middle_site
        , BOOST_RV_REF(OperatorBoundary<Right>) right_boundary
    ) : left_boundary(left_boundary)
      , middle_site(middle_site)
      , right_boundary(right_boundary)
    { } // }}}

    void operator=(BOOST_RV_REF(InfiniteOperator) other) { // {{{
        left_boundary = boost::move(other.left_boundary);
        middle_site = boost::move(other.middle_site);
        right_boundary = boost::move(other.right_boundary);
    } // }}}

    unsigned int physicalDimension() const { return middle_site.physicalDimension(); }
    PhysicalDimension physicalDimension(AsDimension const _) const { return middle_site.physicalDimension(as_dimension); }

    unsigned int operatorDimension() const { return left_boundary.operatorDimension(); }
    OperatorDimension operatorDimension(AsDimension const _) const { return left_boundary.operatorDimension(as_dimension); }
}; // }}}

InfiniteOperator constructExternalFieldInfiniteOperator(MatrixConstPtr const& matrix);
InfiniteOperator constructTransverseIsingModelInfiniteOperator(double spin_coupling_strength);
InfiniteOperator constructXYModelInfiniteOperator(double spin_coupling_strength);

}

#endif
