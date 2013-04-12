#ifndef NUTCRACKER_OPERATORS_HPP
#define NUTCRACKER_OPERATORS_HPP

#include <boost/assign/list_of.hpp>
#include <boost/move/move.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <complex>

#include "nutcracker/tensors.hpp"
#include "nutcracker/utilities.hpp"

namespace Nutcracker {

using boost::assign::list_of;
using boost::copy;
using boost::reverse_copy;

typedef Link<MatrixConstPtr> OperatorSiteLink;
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


}

#endif
