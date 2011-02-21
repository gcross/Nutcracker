//@+leo-ver=5-thin
//@+node:gcross.20110214155808.2016: * @thin optimizer.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2052: ** << License >>
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
//@+node:gcross.20110214155808.2017: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <illuminate.hpp>

#include "operators.hpp"
#include "optimizer.hpp"
#include "states.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
using boost::none;
//@-<< Includes >>

//@+others
//@+node:gcross.20110214155808.2018: ** Tests
TEST_SUITE(Optimizer) {

//@+others
//@+node:gcross.20110214155808.2045: *3* optimizeStateSite
TEST_SUITE(optimizeStateSite) {

//@+others
//@+node:gcross.20110214155808.2046: *4* one site under external field
TEST_SUITE(one_site_under_external_field) {

    void runTests(unsigned int const physical_dimension) {
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

            ASSERT_EQ(physical_dimension,new_state_site.physicalDimension(as_unsigned_integer));
            ASSERT_EQ(1,new_state_site.leftDimension(as_unsigned_integer));
            ASSERT_EQ(1,new_state_site.rightDimension(as_unsigned_integer));
            BOOST_FOREACH(unsigned int const i, irange(0u,physical_dimension-1)) {
                ASSERT_NEAR(c(0,0),new_state_site[i],1e-15);
            }
            ASSERT_NEAR(1,abs(new_state_site[physical_dimension-1]),1e-5);
            ASSERT_NEAR(-1,optimizer_result.eigenvalue,1e-5);
            ASSERT_EQ(0,optimizer_result.number_of_iterations);
        }
    }

    TEST_CASE(physical_dimension_1) { runTests(1); }
    TEST_CASE(physical_dimension_2) { runTests(2); }
    TEST_CASE(physical_dimension_3) { runTests(3); }
    TEST_CASE(physical_dimension_4) { runTests(4); }
}
//@-others

}
//@-others

}
//@-others
//@-leo
