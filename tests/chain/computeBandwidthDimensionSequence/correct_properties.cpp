//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3148: * @thin correct_properties.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3149: ** << License >>
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
//@+node:gcross.20110307093706.3150: ** << Includes >>
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
        unsigned int const
             requested_bandwidth_dimension = random(1,100000)
            ,number_of_sites = random(20,1000)
            ;
        vector<unsigned int> physical_dimensions;
        physical_dimensions.reserve(number_of_sites);
        generate_n(
             back_inserter(physical_dimensions)
            ,number_of_sites
            ,random.generateRandomIntegers(2,10)
        );

        vector<unsigned int> const bandwidth_dimensions =
            computeBandwidthDimensionSequence(
                 requested_bandwidth_dimension
                ,physical_dimensions
            );

        ASSERT_EQUAL(number_of_sites+1,bandwidth_dimensions.size());
        ASSERT_EQUAL(requested_bandwidth_dimension,*max_element(bandwidth_dimensions));

        BOOST_FOREACH(unsigned int const i, irange(0u,number_of_sites-1)) {
            ASSERT_LESS_OR_EQUAL(bandwidth_dimensions[i+1], bandwidth_dimensions[i]*physical_dimensions[i]);
            ASSERT_LESS_OR_EQUAL(bandwidth_dimensions[i], bandwidth_dimensions[i+1]*physical_dimensions[i]);
        }
    }

    return 0;
}
//@-leo
