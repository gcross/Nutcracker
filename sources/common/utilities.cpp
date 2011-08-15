//@+leo-ver=5-thin
//@+node:gcross.20110125202132.2160: * @file utilities.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2042: ** << License >>
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

//@+<< Documentation >>
//@+node:gcross.20110429225820.2527: ** << Documentation >>
/*!
\file utilities.cpp
\brief Utility classes and functions
*/
//@-<< Documentation >>

//@+<< Includes >>
//@+node:gcross.20110125202132.2161: ** << Includes >>
#include <boost/range/numeric.hpp>
#include <boost/range/irange.hpp>
#include <numeric>
#include <string>

#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110125202132.2162: ** << Usings >>
using boost::accumulate;
using boost::irange;

using std::ceil;
using std::multiplies;
//@-<< Usings >>

//@+others
//@+node:gcross.20110217175626.1937: ** Functions
//@+node:gcross.20110219101843.1939: *3* choose
unsigned long long choose(unsigned int const n, unsigned int const k) {
    assert(k <= n);
    return accumulate(irange((n-k)+1,n+1),1,multiplies<unsigned long long>()) / accumulate(irange(1u,k+1),1,multiplies<unsigned long long>());
}
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
//@+node:gcross.20110511190907.3644: *3* computeDigitsOfPrecision
unsigned int computeDigitsOfPrecision(double const tolerance) {
    double const estimated_precision = ceil(-log10(tolerance));
    return estimated_precision > 0 ? (unsigned int)estimated_precision : 1;
}
//@-others

}
//@-leo
