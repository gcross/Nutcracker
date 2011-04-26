//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3412: * @file non_trivial.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3413: ** << License >>
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
//@+node:gcross.20110307093706.3414: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/range/algorithm/equal.hpp>

#include "boundaries.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
using boost::equal;
//@-<< Includes >>
int main() {
    ExpectationBoundary<Left> const left_boundary
        (OperatorDimension(2)
        ,fillWithRange(list_of
            (c(1,2))(c(2,3))(c(3,4))
            (c(4,5))(c(5,6))(c(6,7))
            (c(7,8))(c(8,9))(c(9,10))

            (c(1,9))(c(2,8))(c(3,7))
            (c(4,6))(c(5,5))(c(6,4))
            (c(7,3))(c(8,2))(c(9,1))
        ));

    ExpectationBoundary<Right> const right_boundary
        (OperatorDimension(2)
        ,fillWithRange(list_of
            (c(1,1))(c(2,2))(c(3,3))
            (c(6,6))(c(5,5))(c(4,4))
            (c(7,7))(c(8,8))(c(9,9))

            (c(1,-1))(c(2,-2))(c(3,-3))
            (c(6,-6))(c(5,-5))(c(4,-4))
            (c(7,-7))(c(8,-8))(c(9,-9))
        ));

    ASSERT_EQUAL_TO(c(405,495),
        contractExpectationBoundaries(
             left_boundary
            ,right_boundary
        )
    );

    return 0;
}
//@-leo
