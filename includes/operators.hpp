//@+leo-ver=5-thin
//@+node:gcross.20110206185121.1758: * @file operators.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2012: ** << License >>
//@+at
// Copyright (c) 2011, Gregory Crosswhite
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//@@c
//@-<< License >>

#ifndef NUTCRACKER_OPERATORS_HPP
#define NUTCRACKER_OPERATORS_HPP

//@+<< Includes >>
//@+node:gcross.20110206185121.1759: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/move/move.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <complex>

#include "tensors.hpp"
#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110206185121.1760: ** << Usings >>
using boost::assign::list_of;
using boost::copy;
using boost::reverse_copy;
//@-<< Usings >>

//@+others
//@+node:gcross.20110214164734.1939: ** Classes
//@+node:gcross.20110206185121.1761: *3* OperatorLink
struct OperatorLink {
    unsigned int from, to;
    MatrixConstPtr matrix;

    OperatorLink() {}

    OperatorLink(
          unsigned int const from
        , unsigned int const to
        , MatrixConstPtr const& matrix
    ) : from(from)
      , to(to)
      , matrix(matrix)
    { }

};
//@+node:gcross.20110206185121.1771: ** Functions
OperatorSite constructOperatorSite(
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
    , vector<OperatorLink> const& links
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
