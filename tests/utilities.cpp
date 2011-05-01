//@+leo-ver=5-thin
//@+node:gcross.20110215135633.1848: * @file utilities.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2060: ** << License >>
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
//@+node:gcross.20110215135633.1849: ** << Includes >>
#include <algorithm>
#include <boost/assign/list_of.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <boost/range/numeric.hpp>
#include <complex>
#include <functional>
#include <illuminate.hpp>
#include <iostream>
#include <sstream>

#include "utilities.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::accumulate;
using boost::assign::list_of;
using boost::equal;
using boost::generate;

using std::abs;
using std::cerr;
using std::endl;
using std::generate;
using std::istringstream;
using std::multiplies;
using std::ostringstream;
//@-<< Includes >>

//@+others
//@+node:gcross.20110215135633.1850: ** Tests
TEST_SUITE(Utilities) {

//@+others
//@+node:gcross.20110220093853.1949: *3* index conversion
TEST_SUITE(index_conversion) {

//@+others
//@+node:gcross.20110215135633.1851: *4* flat index round trip
TEST_CASE(flat_index_round_trip) {
    RNG random;

    REPEAT(10) {
        vector<unsigned int> dimensions(random,0);  generate(dimensions,random.randomInteger);
        unsigned long long const flat_index = random(0,accumulate(dimensions,1,multiplies<unsigned int>())-1);
        ASSERT_EQ(flat_index,tensorIndexToFlatIndex(dimensions,flatIndexToTensorIndex(dimensions,flat_index)));
    }
}
//@+node:gcross.20110215135633.1853: *4* tensor index round trip
TEST_CASE(tensor_index_round_trip) {
    RNG random;

    REPEAT(10) {
        vector<unsigned int> dimensions(random,0);  generate(dimensions,random.randomInteger);
        vector<unsigned int> tensor_index; tensor_index.reserve(dimensions.size());
        BOOST_FOREACH(unsigned int const dimension, dimensions) {
            tensor_index.push_back(random(0,dimension-1));
        }
        vector<unsigned int> const round_trip_tensor_index(flatIndexToTensorIndex(dimensions,tensorIndexToFlatIndex(dimensions,tensor_index)));
        ASSERT_TRUE(equal(tensor_index,round_trip_tensor_index));
    }
}
//@+node:gcross.20110215135633.1854: *4* index representation equivalence
TEST_CASE(index_representation_equivalence) {
    RNG random;
    unsigned int tensor_data[2][5][3][7][4], *flat_data = &tensor_data[0][0][0][0][0];
    vector<unsigned int> const dimensions = list_of(2)(5)(3)(7)(4);
    unsigned long long const size = accumulate(dimensions,1,multiplies<unsigned int>());
    generate(flat_data,flat_data+size,random.generateRandomIntegers(0,65535));

    REPEAT(10) {
        unsigned long long const flat_index = random(0,accumulate(dimensions,1,multiplies<unsigned int>()));
        vector<unsigned int> const tensor_index(flatIndexToTensorIndex(dimensions,flat_index));
        ASSERT_EQ(flat_data[flat_index],tensor_data[tensor_index[0]][tensor_index[1]][tensor_index[2]][tensor_index[3]][tensor_index[4]]);
    }

    REPEAT(10) {
        vector<unsigned int> tensor_index; tensor_index.reserve(dimensions.size());
        BOOST_FOREACH(unsigned int const dimension, dimensions) {
            tensor_index.push_back(random(0,dimension-1));
        }
        unsigned long long const flat_index = tensorIndexToFlatIndex(dimensions,tensor_index);
        ASSERT_EQ(flat_data[flat_index],tensor_data[tensor_index[0]][tensor_index[1]][tensor_index[2]][tensor_index[3]][tensor_index[4]]);
    }
}
//@-others

}
//@-others

}
//@-others
//@-leo
