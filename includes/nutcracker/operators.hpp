//@+leo-ver=5-thin
//@+node:gcross.20110206185121.1758: * @file operators.hpp
//@@language cplusplus
#ifndef NUTCRACKER_OPERATORS_HPP
#define NUTCRACKER_OPERATORS_HPP

//@+<< Includes >>
//@+node:gcross.20110206185121.1759: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/move/move.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <complex>

#include "nutcracker/tensors.hpp"
#include "nutcracker/utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110206185121.1760: ** << Usings >>
using boost::assign::list_of;
using boost::copy;
using boost::reverse_copy;
//@-<< Usings >>

//@+others
//@+node:gcross.20110827215622.2633: ** Type aliases
typedef Link<MatrixConstPtr> OperatorSiteLink;
//@+node:gcross.20110206185121.1771: ** Functions
OperatorSite constructOperatorSite(
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
    , vector<OperatorSiteLink> const& links
);

Operator constructExternalFieldOperator(
      unsigned int const number_of_sites
    , MatrixConstPtr const& matrix
);

Operator constructTransverseIsingModelOperator(
      unsigned int const number_of_operators
    , double spin_coupling_strength
);

vector<unsigned int> extractPhysicalDimensions(
    Operator const& operator_sites
);

//@+others
//@-others
//@-others

}

#endif
//@-leo
