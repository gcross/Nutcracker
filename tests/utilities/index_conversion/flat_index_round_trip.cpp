//@+leo-ver=5-thin
//@+node:gcross.20110307093706.2354: * @file flat_index_round_trip.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.2356: ** << License >>
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
//@+node:gcross.20110307093706.2358: ** << Includes >>
#include <algorithm>
#include <boost/assign/list_of.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <boost/range/numeric.hpp>
#include <functional>
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
int main() {
    RNG random;

    REPEAT(10) {
        vector<unsigned int> dimensions(random,0);  generate(dimensions,random.randomInteger);
        unsigned long long const flat_index = random(0,accumulate(dimensions,1,multiplies<unsigned int>())-1);
        ASSERT_EQUAL(flat_index,tensorIndexToFlatIndex(dimensions,flatIndexToTensorIndex(dimensions,flat_index)));
    }

    return 0;
}
//@-leo
