//@+leo-ver=5-thin
//@+node:gcross.20110826235932.2539: * @file states.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110826235932.2540: ** << License >>
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
//@+node:gcross.20110826235932.2541: ** << Includes >>
#include <complex>
#include <illuminate.hpp>

#include "states.hpp"

#include "test_utils.hpp"

using std::abs;
//@-<< Includes >>

//@+others
//@+node:gcross.20110826235932.2542: ** Tests
TEST_SUITE(States) {

//@+others
//@+node:gcross.20110217014932.1935: *3* computeStateOverlap
TEST_SUITE(computeStateOverlap) {

//@+others
//@+node:gcross.20110217014932.1937: *4* self-overlap is 1
TEST_CASE(self_overlap_is_1) {

    RNG random;

    REPEAT(10) {
        State state(random.randomState());
        ASSERT_NEAR_REL(
             c(1,0)
            ,computeStateOverlap(state,state)
            ,1e-15
        )
    }

}
//@+node:gcross.20110218150430.2598: *4* function is hermitian
TEST_CASE(function_is_hermitian) {

    RNG random;

    REPEAT(10) {
        vector<unsigned int> physical_dimensions(random.randomUnsignedIntegerVector(random(1,5)));
        State const state_1(random.randomState(physical_dimensions))
                  , state_2(random.randomState(physical_dimensions))
                  ;
        ASSERT_NEAR_REL(
             conj(computeStateOverlap(state_1,state_2))
            ,computeStateOverlap(state_2,state_1)
            ,1e-14
        )
    }

}
//@+node:gcross.20110218150430.2596: *4* single site orthogonal overlap is zero
TEST_SUITE(single_site_orthogonal_overlap_is_zero) {

    void runTest(unsigned int const physical_dimension) {
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
                ASSERT_NEAR_REL(
                     c(0,0)
                    ,computeStateOverlap(states[i],states[j])
                    ,1e-15
                )
            }
        }
    }

    TEST_CASE(physical_dimension_2) { runTest(2); }
    TEST_CASE(physical_dimension_3) { runTest(3); }
    TEST_CASE(physical_dimension_4) { runTest(4); }

}
//@-others

}
//@-others

}
//@-others
//@-leo
