//@+leo-ver=5-thin
//@+node:gcross.20110130170743.1665: * @thin chain.hpp
//@@language cplusplus

#ifndef NUTCRACKER_CHAIN_HPP
#define NUTCRACKER_CHAIN_HPP

//@+<< Includes >>
//@+node:gcross.20110130170743.1666: ** << Includes >>
#include <vector>

#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110130170743.1667: ** << Usings >>
using namespace boost;
using namespace std;
//@-<< Usings >>

//@+others
//@+node:gcross.20110130193548.1684: ** Exceptions
//@+node:gcross.20110130193548.1685: *3* RequestedBandwidthDimensionTooLargeError
struct RequestedBandwidthDimensionTooLargeError : public Exception {
    unsigned int const
          requested_bandwidth_dimension
        , greatest_possible_bandwidth_dimension
        ;
    RequestedBandwidthDimensionTooLargeError(
        unsigned int const requested_bandwidth_dimension
      , unsigned int const greatest_possible_bandwidth_dimension
    ) : Exception((
            format("Requested bandwidth dimension %1% is too large;  the highest possible with the given physical dimensions is %2%.")
                % requested_bandwidth_dimension
                % greatest_possible_bandwidth_dimension
        ).str())
      , requested_bandwidth_dimension(requested_bandwidth_dimension)
      , greatest_possible_bandwidth_dimension(greatest_possible_bandwidth_dimension)    
    {}
};
//@+node:gcross.20110130170743.1683: ** Functions
//@+node:gcross.20110130170743.1684: *3* computeBandwidthDimensionSequence
vector<unsigned int> computeBandwidthDimensionSequence(
    unsigned int const requested_bandwidth_dimension
   ,vector<unsigned int> const& physical_dimensions
);
//@-others

}

#endif
//@-leo
