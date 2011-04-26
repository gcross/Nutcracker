//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3068: * @file minimumBandwidthDimensionForProjectorCount.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3069: ** << License >>
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
//@+node:gcross.20110307093706.3070: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/range/adaptor/indirected.hpp>

#include "projectors.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::adaptors::indirected;
using boost::assign::list_of;
//@-<< Includes >>
int main() {
    RNG random;

    REPEAT(10) {
        vector<unsigned int> const physical_dimensions = random.randomUnsignedIntegerVector(random(2,10),2,10);
        unsigned int const
             number_of_projectors = random(2,2*physical_dimensions.size()-1)
            ,minimum_bandwidth_dimension =
                minimumBandwidthDimensionForProjectorCount(
                     physical_dimensions
                    ,number_of_projectors
                )
            ;
        {
            vector<unsigned int> const bandwidth_dimension_sequence =
                computeBandwidthDimensionSequence(
                     minimum_bandwidth_dimension
                    ,physical_dimensions
                );
            for(vector<unsigned int>::const_iterator
                      physical_dimension_iterator = physical_dimensions.begin()
                    , left_dimension_iterator = bandwidth_dimension_sequence.begin()
                    , right_dimension_iterator = bandwidth_dimension_sequence.begin()+1
               ;physical_dimension_iterator != physical_dimensions.end()
               ;++physical_dimension_iterator, ++left_dimension_iterator, ++right_dimension_iterator
            ) {
                if((*physical_dimension_iterator) * (*left_dimension_iterator) * (*right_dimension_iterator) > number_of_projectors) {
                    goto found;
                }
            }

            fail("minimum bandwidth dimension is too small");

            found: ;
        }
        {
            vector<unsigned int> const bandwidth_dimension_sequence =
                computeBandwidthDimensionSequence(
                     minimum_bandwidth_dimension-1
                    ,physical_dimensions
                );
            for(vector<unsigned int>::const_iterator
                      physical_dimension_iterator = physical_dimensions.begin()
                    , left_dimension_iterator = bandwidth_dimension_sequence.begin()
                    , right_dimension_iterator = bandwidth_dimension_sequence.begin()+1
               ;physical_dimension_iterator != physical_dimensions.end()
               ;++physical_dimension_iterator, ++left_dimension_iterator, ++right_dimension_iterator
            ) {
                if((*physical_dimension_iterator) * (*left_dimension_iterator) * (*right_dimension_iterator) > number_of_projectors) {
                    fail("minimum bandwidth dimension is too big");
                }
            }
        }
    }

    return 0;
}
//@-leo
