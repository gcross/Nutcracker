//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3080: * @thin optimizeStateSite.hpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3081: ** << License >>
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
//@+<< Includes >>
//@+node:gcross.20110307093706.3082: ** << Includes >>
#include <boost/assign/list_of.hpp>

#include "operators.hpp"
#include "optimizer.hpp"
#include "states.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
using boost::none;
//@-<< Includes >>
static void runTests(unsigned int const physical_dimension) {
    REPEAT(10) {
        StateSite<Middle> state_site(
            randomStateSiteMiddle(
                 PhysicalDimension(physical_dimension)
                ,LeftDimension(1)
                ,RightDimension(1)
            )
        );

        vector<complex<double> > diagonal(physical_dimension,c(1,0));
        diagonal.back() = c(-1,0);
        OperatorSite operator_site(
            constructOperatorSite(
                 PhysicalDimension(physical_dimension)
                ,LeftDimension(1)
                ,RightDimension(1)
                ,list_of(OperatorLink(1,1,diagonalMatrix(diagonal)))
            )

        );

        OptimizerResult optimizer_result(
            optimizeStateSite(
                 ExpectationBoundary<Left>::trivial
                ,state_site
                ,operator_site
                ,ExpectationBoundary<Right>::trivial
                ,ProjectorMatrix()
                ,0
                ,0
                ,10000
            )
        );

        StateSite<Middle> const& new_state_site = optimizer_result.state_site;

        ASSERT_EQUAL(physical_dimension,new_state_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQUAL_TO(1u,new_state_site.leftDimension(as_unsigned_integer));
        ASSERT_EQUAL_TO(1u,new_state_site.rightDimension(as_unsigned_integer));
        BOOST_FOREACH(unsigned int const i, irange(0u,physical_dimension-1)) {
            ASSERT_NEAR_RELATIVE_TO(c(0,0),new_state_site[i],1e-15);
        }
        ASSERT_NEAR_RELATIVE_TO(1,abs(new_state_site[physical_dimension-1]),1e-5);
        ASSERT_NEAR_RELATIVE_TO(-1,optimizer_result.eigenvalue,1e-5);
        ASSERT_EQUAL_TO(0u,optimizer_result.number_of_iterations);
    }
}
//@-leo
