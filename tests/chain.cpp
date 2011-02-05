//@+leo-ver=5-thin
//@+node:gcross.20110130170743.1679: * @thin chain.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110130170743.1680: ** << Includes >>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/range/algorithm/max_element.hpp>
#include <boost/range/irange.hpp>
#include <boost/range/numeric.hpp>
#include <illuminate.hpp>
#include <functional>

#include "chain.hpp"
#include "utilities.hpp"

#include "test_utils.hpp"

using namespace boost;
using namespace Nutcracker;
using namespace std;
//@-<< Includes >>

//@+others
//@+node:gcross.20110130193548.1689: ** Chain
TEST_SUITE(Chain) {

//@+others
//@+node:gcross.20110130193548.1694: *3* computeBandwidthDimensionSequence
TEST_SUITE(computeBandwidthDimensionSequence) {

//@+others
//@+node:gcross.20110130193548.1690: *4* correct properties
TEST_CASE(correct_properties) {
    RNG random;

    REPEAT(10) {
        unsigned int const
             requested_bandwidth_dimension = random(1,100000)()
            ,number_of_sites = random(20,1000)()
            ;
        vector<unsigned int> physical_dimensions;
        physical_dimensions.reserve(number_of_sites);
        generate_n(
             back_inserter(physical_dimensions)
            ,number_of_sites
            ,random(2,10)
        );

        vector<unsigned int> const bandwidth_dimensions =
            computeBandwidthDimensionSequence(
                 requested_bandwidth_dimension
                ,physical_dimensions
            );

        ASSERT_EQ_QUOTED(number_of_sites+1,bandwidth_dimensions.size());
        ASSERT_EQ_QUOTED(requested_bandwidth_dimension,*max_element(bandwidth_dimensions));

        BOOST_FOREACH(unsigned int const i, irange(0u,number_of_sites-1)) {
            ASSERT_LE_QUOTED(bandwidth_dimensions[i+1], bandwidth_dimensions[i]*physical_dimensions[i]);
            ASSERT_LE_QUOTED(bandwidth_dimensions[i], bandwidth_dimensions[i+1]*physical_dimensions[i]);
        }
    }
}
//@+node:gcross.20110130193548.1695: *4* complains if too large
TEST_CASE(complains_if_too_large) {
    RNG random;

    REPEAT(10) {
        unsigned int const number_of_sites = random(10,20)();
        vector<unsigned int> physical_dimensions;
        physical_dimensions.reserve(number_of_sites);
        generate_n(
             back_inserter(physical_dimensions)
            ,number_of_sites
            ,random(2,10)
        );
        unsigned int const requested_bandwidth_dimension = 2*accumulate(physical_dimensions,1u,multiplies<unsigned int>());
        try {
            computeBandwidthDimensionSequence(
                 requested_bandwidth_dimension
                ,physical_dimensions
            );
        } catch(RequestedBandwidthDimensionTooLargeError const& e) {
            ASSERT_EQ_QUOTED(requested_bandwidth_dimension,e.requested_bandwidth_dimension);
            return;
        }
        FATALLY_FAIL("Exception was not thrown!");
    }
}
//@-others

}
//@-others

}
//@-others
//@-leo
