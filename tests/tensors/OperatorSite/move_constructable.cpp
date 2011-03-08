//@+leo-ver=5-thin
//@+node:gcross.20110307093706.2510: * @thin move_constructable.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.2511: ** << License >>
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
//@+node:gcross.20110307093706.2512: ** << Includes >>
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
        unsigned int const number_of_matrices = random;
        PhysicalDimension const physical_dimension(random);
        LeftDimension const left_dimension(random);
        RightDimension const right_dimension(random);

        OperatorSite old_site
            (number_of_matrices
            ,physical_dimension
            ,left_dimension
            ,right_dimension
            ,fillWithGenerator(random.generateRandomIndices(left_dimension,right_dimension))
            ,fillWithGenerator(random.randomComplexDouble)
            );

        ASSERT_TRUE(old_site.valid());
        ASSERT_EQUAL(number_of_matrices,old_site.numberOfMatrices());
        ASSERT_EQUAL(physical_dimension,old_site.physicalDimension());
        ASSERT_EQUAL(left_dimension,old_site.leftDimension());
        ASSERT_EQUAL(right_dimension,old_site.rightDimension());

        OperatorSite new_site(boost::move(old_site));

        ASSERT_FALSE(old_site.valid());
        ASSERT_EQUAL_TO(0u,old_site.numberOfMatrices());
        ASSERT_EQUAL_TO(0u,old_site.physicalDimension(as_unsigned_integer));
        ASSERT_EQUAL_TO(0u,old_site.leftDimension(as_unsigned_integer));
        ASSERT_EQUAL_TO(0u,old_site.rightDimension(as_unsigned_integer));

        ASSERT_TRUE(new_site.valid());
        ASSERT_EQUAL(number_of_matrices,new_site.numberOfMatrices());
        ASSERT_EQUAL(physical_dimension,new_site.physicalDimension());
        ASSERT_EQUAL(left_dimension,new_site.leftDimension());
        ASSERT_EQUAL(right_dimension,new_site.rightDimension());
    }

    return 0;
}
//@-leo
