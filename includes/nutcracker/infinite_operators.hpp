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
    OperatorBoundary<Left> left_operator_boundary;
    OperatorSite operator_site;
    OperatorBoundary<Right> right_operator_boundary;

    InfiniteOperator(BOOST_RV_REF(InfiniteOperator) other) // {{{
      : left_operator_boundary(boost::move(other.left_operator_boundary))
      , operator_site(boost::move(other.operator_site))
      , right_operator_boundary(boost::move(other.right_operator_boundary))
    { } // }}}

    InfiniteOperator( // BOOST_RV_REF(all fields) {{{
          BOOST_RV_REF(OperatorBoundary<Left>) left_operator_boundary
        , BOOST_RV_REF(OperatorSite) operator_site
        , BOOST_RV_REF(OperatorBoundary<Right>) right_operator_boundary
    ) : left_operator_boundary(left_operator_boundary)
      , operator_site(operator_site)
      , right_operator_boundary(right_operator_boundary)
    { } // }}}

    void operator=(BOOST_RV_REF(InfiniteOperator) other) { // {{{
        left_operator_boundary = boost::move(other.left_operator_boundary);
        operator_site = boost::move(other.operator_site);
        right_operator_boundary = boost::move(other.right_operator_boundary);
    } // }}}
}; // }}}

}

#endif
