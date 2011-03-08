//@+leo-ver=5-thin
//@+node:gcross.20110307093706.2558: * @thin single_site_squared.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.2559: ** << License >>
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
//@+node:gcross.20110307093706.2560: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/container/vector.hpp>
#include <boost/numeric/ublas/vector_expression.hpp>
#include <boost/range/adaptor/indirected.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/numeric.hpp>

#include "states.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::adaptors::indirected;
using boost::adaptors::transformed;
using boost::assign::list_of;
using boost::container::vector;
using boost::equal;
using boost::inner_product;
using boost::irange;
using boost::numeric::ublas::norm_1;
//@-<< Includes >>
int main() {
    RNG random;

    REPEAT(10) {
        unsigned int const d = random;
        StateSite<None> state_site(
             PhysicalDimension(d)
            ,LeftDimension(1)
            ,RightDimension(1)
            ,fillWithGenerator(random.randomComplexDouble)
        );
        StateVector actual_state_vector = computeStateVector(vector<StateSiteAny const*>(2,&state_site) | indirected);
        StateVector correct_state_vector(d*d);
        BOOST_FOREACH(unsigned int i, irange(0u,d)) {
            BOOST_FOREACH(unsigned int j, irange(0u,d)) {
                correct_state_vector[i*d+j] = (state_site.begin()[i]) * (state_site.begin()[j]);
            }
        }
        ASSERT_EQUAL(correct_state_vector.size(),actual_state_vector.size());
        ASSERT_TRUE(equal(correct_state_vector,actual_state_vector));
    }

    return 0;
}
//@-leo
