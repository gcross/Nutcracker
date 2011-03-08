//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3031: * @thin single_site_orthogonal_overlap_is_0.hpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3032: ** << License >>
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
//@+node:gcross.20110307093706.3033: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/range/adaptor/indirected.hpp>

#include "projectors.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::adaptors::indirected;
using boost::assign::list_of;
//@-<< Includes >>

static void runTest(unsigned int const physical_dimension) {
    vector<State> states;  states.reserve(physical_dimension);
    BOOST_FOREACH(unsigned int i, irange(0u,physical_dimension)) {
        vector<complex<double> > data(physical_dimension,c(0,0));
        data[i] = c(1,0);
        StateSite<Middle> first_state_site
            (LeftDimension(1)
            ,RightDimension(1)
            ,fillWithRange(data)
            );
        vector<StateSite<Right> > rest_state_sites;
        states.emplace_back(
             boost::move(first_state_site)
            ,boost::move(rest_state_sites)
        );
    }
    BOOST_FOREACH(unsigned int i, irange(0u,physical_dimension)) {
        BOOST_FOREACH(unsigned int j, irange(i+1,physical_dimension)) {
            ASSERT_NEAR_RELATIVE_TO(
                 c(0,0)
                ,computeStateOverlap(states[i],states[j])
                ,1e-15
            );
        }
    }
}
//@-leo
