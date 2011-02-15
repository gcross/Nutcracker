//@+leo-ver=5-thin
//@+node:gcross.20110215135633.1848: * @thin utilities.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110215135633.1849: ** << Includes >>
#include <algorithm>
#include <boost/assign/list_of.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <boost/range/numeric.hpp>
#include <functional>
#include <illuminate.hpp>

#include "utilities.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::accumulate;
using boost::assign::list_of;
using boost::equal;
using boost::generate;

using std::generate;
using std::multiplies;
//@-<< Includes >>

//@+others
//@+node:gcross.20110215135633.1850: ** Tests
TEST_SUITE(Utilities) {

//@+others
//@+node:gcross.20110215135633.1851: *3* flat index round trip
TEST_CASE(flat_index_round_trip) {
    RNG random;

    REPEAT(10) {
        vector<unsigned int> dimensions(random,0);  generate(dimensions,random.randomInteger);
        unsigned long long const flat_index = random(0,accumulate(dimensions,1,multiplies<unsigned int>())-1);
        ASSERT_EQ_QUOTED(flat_index,tensorIndexToFlatIndex(dimensions,flatIndexToTensorIndex(dimensions,flat_index)));
    }
}
//@+node:gcross.20110215135633.1853: *3* tensor index round trip
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
//@+node:gcross.20110215135633.1854: *3* index representation equivalence
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
//@-leo
