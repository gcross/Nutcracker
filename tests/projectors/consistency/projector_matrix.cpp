//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3062: * @thin projector_matrix.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3063: ** << License >>
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
//@+node:gcross.20110307093706.3064: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/range/adaptor/indirected.hpp>

#include "projectors.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::adaptors::indirected;
using boost::assign::list_of;
//@-<< Includes >>
int main() {
    RNG random;

    REPEAT(10) {
        vector<unsigned int> physical_dimensions(random.randomUnsignedIntegerVector(random(1,5)));
        unsigned int const number_of_sites = physical_dimensions.size();
        Projector const projector(computeProjectorFromState(random.randomState(physical_dimensions)));
        State const state(random.randomState(physical_dimensions));
        complex<double> const overlap = computeProjectorOverlap(projector,state);
        vector<OverlapBoundary<Right> > right_boundaries;
        right_boundaries.emplace_back(make_trivial);
        BOOST_FOREACH(unsigned int const i, irange(1u,number_of_sites) | reversed) {
            right_boundaries.push_back(
                contract<Right>::SS(
                     right_boundaries.back()
                    ,projector[i].get<Right>()
                    ,state.getRestSite(i-1)
                )
            );
        }
        OverlapBoundary<Left> left_boundary(make_trivial);
        BOOST_FOREACH(unsigned int const i, irange(0u,number_of_sites)) {
            OverlapBoundary<Right> right_boundary(boost::move(right_boundaries.back()));
            right_boundaries.pop_back();
            ProjectorMatrix projector_matrix(
                formProjectorMatrix(
                     list_of(&left_boundary) | indirected
                    ,list_of(&right_boundary) | indirected
                    ,list_of(&projector[i].get<Middle>()) | indirected
                )
            );
            ASSERT_NEAR_RELATIVE(
                 abs(overlap)
                ,computeOverlapWithProjectors(projector_matrix,state[i])
                ,1e-15
            );
            if(i > 0) {
                left_boundary =
                    Unsafe::contractSSLeft(
                         left_boundary
                        ,projector[i].get<Left>()
                        ,state[i]
                    );
            }
        }
    }

    return 0;
}
//@-leo
