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

using std::cerr;
using std::endl;
using std::generate;
using std::istringstream;
using std::multiplies;
using std::ostringstream;
//@-<< Includes >>

//@+others
//@+node:gcross.20110220093853.1985: ** Tests
//@+node:gcross.20110215135633.1850: *3* Utilities
TEST_SUITE(Utilities) {

//@+others
//@+node:gcross.20110220093853.1949: *4* index conversion
TEST_SUITE(index_conversion) {

//@+others
//@+node:gcross.20110215135633.1851: *5* flat index round trip
TEST_CASE(flat_index_round_trip) {
    RNG random;

    REPEAT(10) {
        vector<unsigned int> dimensions(random,0);  generate(dimensions,random.randomInteger);
        unsigned long long const flat_index = random(0,accumulate(dimensions,1,multiplies<unsigned int>())-1);
        ASSERT_EQ_QUOTED(flat_index,tensorIndexToFlatIndex(dimensions,flatIndexToTensorIndex(dimensions,flat_index)));
    }
}
//@+node:gcross.20110215135633.1853: *5* tensor index round trip
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
//@+node:gcross.20110215135633.1854: *5* index representation equivalence
TEST_CASE(index_representation_equivalence) {
    RNG random;
    unsigned int tensor_data[2][5][3][7][4], *flat_data = &tensor_data[0][0][0][0][0];
    vector<unsigned int> const dimensions = list_of(2)(5)(3)(7)(4);
    unsigned long long const size = accumulate(dimensions,1,multiplies<unsigned int>());
    generate(flat_data,flat_data+size,random.generateRandomIntegers(0,65535));

    REPEAT(10) {
        unsigned long long const flat_index = random(0,accumulate(dimensions,1,multiplies<unsigned int>()));
        vector<unsigned int> const tensor_index(flatIndexToTensorIndex(dimensions,flat_index));
        ASSERT_EQ_QUOTED(flat_data[flat_index],tensor_data[tensor_index[0]][tensor_index[1]][tensor_index[2]][tensor_index[3]][tensor_index[4]]);
    }

    REPEAT(10) {
        vector<unsigned int> tensor_index; tensor_index.reserve(dimensions.size());
        BOOST_FOREACH(unsigned int const dimension, dimensions) {
            tensor_index.push_back(random(0,dimension-1));
        }
        unsigned long long const flat_index = tensorIndexToFlatIndex(dimensions,tensor_index);
        ASSERT_EQ_QUOTED(flat_data[flat_index],tensor_data[tensor_index[0]][tensor_index[1]][tensor_index[2]][tensor_index[3]][tensor_index[4]]);
    }
}
//@-others

}
//@-others

}
//@+node:gcross.20110220093853.1986: *3* I/O
TEST_SUITE(IO) {

//@+others
//@+node:gcross.20110220093853.1950: *4* complex double
TEST_SUITE(complex_double) {

//@+others
//@+node:gcross.20110220093853.1953: *5* decode
TEST_CASE(decode) {

    RNG random;

    REPEAT(10) {
        complex<double> x = random, y;

        ostringstream out;
        out << format("[%|.20|,%|.20|]") % x.real() % x.imag();
        istringstream in(out.str());
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);
        doc >> y;

        ASSERT_EQ(x,y);
    }

}
//@+node:gcross.20110220093853.1951: *5* encode then decode
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        complex<double> x = random, y;

        YAML::Emitter out;
        out << x;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);
        doc >> y;

        ASSERT_EQ(x,y);
    }

}
//@-others

}
//@+node:gcross.20110220141808.1982: *4* real complex double
TEST_SUITE(real_complex_double) {

//@+others
//@+node:gcross.20110220141808.1983: *5* decode
TEST_CASE(decode) {

    RNG random;

    REPEAT(10) {
        double x = random.randomDouble();

        ostringstream out;
        out << format("%|.20|") % x;
        istringstream in(out.str());
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);
        complex<double> y;
        doc >> y;

        ASSERT_EQ(c(x,0),y);
    }

}
//@+node:gcross.20110220141808.1984: *5* encode then decode
TEST_CASE(encode_then_decode) {

    RNG random;

    REPEAT(10) {
        complex<double> x = c(random.randomDouble(),0), y;

        YAML::Emitter out;
        out << x;
        string out_string(out.c_str());

        istringstream in(out_string);
        YAML::Parser parser(in);
        YAML::Node doc;
        parser.GetNextDocument(doc);
        ASSERT_EQ(YAML::CT_SCALAR,doc.GetType());
        doc >> y;

        ASSERT_EQ(x,y);
    }

}
//@-others

}
//@-others

}
//@-others
//@-leo
