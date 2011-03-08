//@+leo-ver=5-thin
//@+node:gcross.20110307093706.2564: * @thin two_trivial_sites.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.2565: ** << License >>
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
//@+node:gcross.20110307093706.2566: ** << Includes >>
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
        unsigned int b = random;
        StateSite<None>
             state_site_1
                (PhysicalDimension(1)
                ,LeftDimension(1)
                ,RightDimension(b)
                ,fillWithGenerator(random.randomComplexDouble)
                )
            ,state_site_2
                (PhysicalDimension(1)
                ,LeftDimension(b)
                ,RightDimension(1)
                ,fillWithGenerator(random.randomComplexDouble)
                )
            ;
        StateVector state_vector = computeStateVector(list_of(&state_site_1)(&state_site_2) | indirected);
        ASSERT_EQUAL_TO(1u,state_vector.size());
        ASSERT_NEAR_RELATIVE(inner_product(state_site_1,state_site_2,c(0,0)),state_vector[0],1e-14);
    }

    return 0;
}
//@-leo
