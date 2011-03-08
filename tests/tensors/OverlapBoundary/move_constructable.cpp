//@+leo-ver=5-thin
//@+node:gcross.20110307093706.2530: * @thin move_constructable.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.2531: ** << License >>
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
//@+node:gcross.20110307093706.2532: ** << Includes >>
#include <algorithm>
#include <boost/range/algorithm/equal.hpp>
#include <sstream>

#include "boundaries.hpp"
#include "operators.hpp"
#include "states.hpp"
#include "utilities.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::equal;

using std::endl;
using std::equal;
using std::istringstream;
using std::ostringstream;
//@-<< Includes >>
int main() {
    RNG random;

    REPEAT(10) {
        OverlapDimension const overlap_dimension(random);
        StateDimension const state_dimension(random);

        OverlapBoundary<Left> old_boundary
            (overlap_dimension
            ,state_dimension
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_boundary.valid());
        ASSERT_EQUAL(overlap_dimension,old_boundary.overlapDimension());
        ASSERT_EQUAL(state_dimension,old_boundary.stateDimension());

        OverlapBoundary<Left> new_boundary(boost::move(old_boundary));

        ASSERT_FALSE(old_boundary.valid());
        ASSERT_EQUAL_TO(0u,old_boundary.overlapDimension(as_unsigned_integer));
        ASSERT_EQUAL_TO(0u,old_boundary.stateDimension(as_unsigned_integer));

        ASSERT_TRUE(new_boundary.valid());
        ASSERT_EQUAL(overlap_dimension,new_boundary.overlapDimension());
        ASSERT_EQUAL(state_dimension,new_boundary.stateDimension());
    }

    return 0;
}
//@-leo
