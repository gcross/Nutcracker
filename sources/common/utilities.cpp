//@+leo-ver=5-thin
//@+node:gcross.20110125202132.2160: * @thin utilities.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110125202132.2161: ** << Includes >>
#include <string>

#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110125202132.2162: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110125202132.2163: ** struct Exception
Exception::Exception(string const& message) : message(message) { }

const char* Exception::what() const throw() { return message.c_str(); }

Exception::~Exception() throw() { }
//@+node:gcross.20110217175626.1937: ** Functions
//@+node:gcross.20110217175626.1936: *3* computeBandwidthDimensionSequence
vector<unsigned int> computeBandwidthDimensionSequence(
    unsigned int const requested_bandwidth_dimension
   ,vector<unsigned int> const& physical_dimensions
) {
    unsigned int const middle_index = (physical_dimensions.size()+1)/2;

    vector<unsigned int> forward_bandwidth_dimensions(1,1);
    BOOST_FOREACH(
         unsigned int const d
        ,make_pair(
             makeProductIterator(physical_dimensions.begin())
            ,makeProductIterator(physical_dimensions.begin()+middle_index)
         )
    ) {
        if(d >= requested_bandwidth_dimension) break;
        forward_bandwidth_dimensions.push_back(d);
    }

    vector<unsigned int> reverse_bandwidth_dimensions(1,1);
    BOOST_FOREACH(
         unsigned int const d
        ,make_pair(
             makeProductIterator(physical_dimensions.rbegin())
            ,makeProductIterator(physical_dimensions.rbegin()+middle_index)
         )
    ) {
        if(d >= requested_bandwidth_dimension) break;
        reverse_bandwidth_dimensions.push_back(d);
    }

    if(forward_bandwidth_dimensions.size() == middle_index+1
    || reverse_bandwidth_dimensions.size() == middle_index+1
    ) {
        throw RequestedBandwidthDimensionTooLargeError(
                 requested_bandwidth_dimension
                ,min(forward_bandwidth_dimensions.size() == middle_index+1 ? forward_bandwidth_dimensions.back() : numeric_limits<unsigned int>::max()
                    ,reverse_bandwidth_dimensions.size() == middle_index+1 ? reverse_bandwidth_dimensions.back() : numeric_limits<unsigned int>::max()
                    )
        );
    }

    fill_n(
         back_inserter(forward_bandwidth_dimensions)
        ,physical_dimensions.size()+1
            - forward_bandwidth_dimensions.size()
            - reverse_bandwidth_dimensions.size()
        ,requested_bandwidth_dimension
    );

    reverse_copy(
         reverse_bandwidth_dimensions
        ,back_inserter(forward_bandwidth_dimensions)
    );

    return boost::move(forward_bandwidth_dimensions);
}
//@-others

}
//@-leo
