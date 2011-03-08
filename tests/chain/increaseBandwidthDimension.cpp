//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3202: * @thin increaseBandwidthDimension.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3203: ** << License >>
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
//@+node:gcross.20110307093706.3204: ** << Includes >>
#include <boost/assign.hpp>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/local/function.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/max_element.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/numeric.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>
#include <sstream>
#include <functional>

#include "chain.hpp"
#include "operators.hpp"
#include "utilities.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::accumulate;
using boost::container::vector;
using boost::equal;
using boost::format;
using boost::max_element;
using boost::shared_ptr;

using std::abs;
using std::conj;
using std::ostringstream;
using std::pair;
//@-<< Includes >>
int main() {
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_operators = random+2;
        Chain chain(random.randomOperator(number_of_operators));
        unsigned int const maximum_bandwidth_dimension = min(10u,chain.maximum_bandwidth_dimension);

        State old_state = chain.makeCopyOfState();

        #define VALIDATE_CHAIN_PROPERTIES \
            { \
                ASSERT_NEAR_RELATIVE_TO(1,chain.computeStateNorm(),1e-9); \
                complex<double> const expectation_value = chain.computeExpectationValueAtCurrentSite(); \
                ASSERT_NEAR_RELATIVE_TO(0,expectation_value.imag(),1e-9); \
                ASSERT_NEAR_RELATIVE(chain.getEnergy(),expectation_value.real(),1e-7); \
            }

        BOOST_FOREACH(unsigned int const bandwidth_dimension, irange(1u,maximum_bandwidth_dimension)) {
            chain.increaseBandwidthDimension(bandwidth_dimension);
            State new_state = chain.makeCopyOfState();
            ASSERT_NEAR_RELATIVE_TO(c(1,0),computeStateOverlap(old_state,new_state),1e-13);
            REPEAT(number_of_operators-1) {
                chain.move<Right>();
                VALIDATE_CHAIN_PROPERTIES
            }
            REPEAT(number_of_operators-1) {
                chain.move<Left>();
                VALIDATE_CHAIN_PROPERTIES
            }
        }

    }

    return 0;
}
//@-leo
