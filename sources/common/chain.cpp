//@+leo-ver=5-thin
//@+node:gcross.20110130170743.1674: * @thin chain.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110130170743.1675: ** << Includes >>
#include <algorithm>
#include <boost/assign.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <iterator>
#include <utility>

#include "chain.hpp"
#include "core.hpp"


#include <iostream>
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110130170743.1676: ** << Usings >>
using namespace boost;
using namespace boost::assign;
using namespace std;
//@-<< Usings >>

//@+others
//@+node:gcross.20110130193548.1686: ** Functions
//@+node:gcross.20110130193548.1688: *3* computeBandwidthDimensionSequence
vector<unsigned int> computeBandwidthDimensionSequence(
    unsigned int const requested_bandwidth_dimension
   ,vector<unsigned int> const& physical_dimensions
) {
    size_t middle_index = (physical_dimensions.size()+1)/2;

    vector<unsigned int> forward_bandwidth_dimensions;
    forward_bandwidth_dimensions.reserve(physical_dimensions.size());
    unsigned int forward_bandwidth_dimension = 1;
    BOOST_FOREACH(
         unsigned int const physical_dimension
        ,make_pair(
             physical_dimensions.begin()
            ,physical_dimensions.begin()+middle_index
        )
    ) {
        forward_bandwidth_dimensions.push_back(forward_bandwidth_dimension);
        forward_bandwidth_dimension *= physical_dimension;
        if(forward_bandwidth_dimension >= requested_bandwidth_dimension) {
            forward_bandwidth_dimension = requested_bandwidth_dimension;
            break;
        }
    }

    vector<unsigned int> reverse_bandwidth_dimensions;
    unsigned int reverse_bandwidth_dimension = 1;
    BOOST_FOREACH(
         unsigned int const physical_dimension
        ,make_pair(
             physical_dimensions.rbegin()
            ,physical_dimensions.rbegin()+middle_index
        )
    ) {
        reverse_bandwidth_dimensions.push_back(reverse_bandwidth_dimension);
        reverse_bandwidth_dimension *= physical_dimension;
        if(reverse_bandwidth_dimension >= requested_bandwidth_dimension) {
            reverse_bandwidth_dimension = requested_bandwidth_dimension;
            break;
        }
    }

    if(forward_bandwidth_dimension < requested_bandwidth_dimension
     ||reverse_bandwidth_dimension < requested_bandwidth_dimension
      ) {
        throw RequestedBandwidthDimensionTooLargeError(
                 requested_bandwidth_dimension
                ,min(forward_bandwidth_dimension,reverse_bandwidth_dimension)
        );
    }

    fill_n(
         back_inserter(forward_bandwidth_dimensions)
        ,physical_dimensions.size()
            - forward_bandwidth_dimensions.size()
            - reverse_bandwidth_dimensions.size()
            + 1
        ,requested_bandwidth_dimension
    );

    reverse_copy(
         reverse_bandwidth_dimensions
        ,back_inserter(forward_bandwidth_dimensions)
    );

    return forward_bandwidth_dimensions;
}
//@-others

}
//@-leo
