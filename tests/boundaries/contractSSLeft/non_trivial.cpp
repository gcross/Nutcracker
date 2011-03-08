//@+leo-ver=5-thin
//@+node:gcross.20110307093706.3471: * @thin non_trivial.cpp
//@@language cplusplus
//@+<< License >>
//@+node:gcross.20110307093706.3472: ** << License >>
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
//@+node:gcross.20110307093706.3473: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/range/algorithm/equal.hpp>

#include "boundaries.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
using boost::equal;
//@-<< Includes >>
int main() {
    OverlapBoundary<Left> const boundary
        (OverlapDimension(3)
        ,fillWithRange(list_of
            (c(2,0))(c(0,4))
            (c(1,0))(c(0,5))
            (c(3,0))(c(0,6))
        ));
    OverlapSite<Left> const overlap_site
        (RightDimension(2)
        ,LeftDimension(3)
        ,fillWithRange(list_of
            (c( 1,0))(c( 0,-1))(c(0, 1))
            (c( 1,0))(c( 1, 0))(c(0,-1))
        ));
    StateSite<Left> const state_site
        (LeftDimension(2)
        ,RightDimension(5)
        ,fillWithRange(list_of
            (c(1,0))(c(1,0))(c( 1,0))(c(-1, 0))(c(-1,0))
            (c(2,0))(c(0,2))(c(-2,0))(c( 1,-2))(c( 2,0))
        ));
    OverlapBoundary<Left> const actual_boundary(
        contractSSLeft(
             boundary
            ,overlap_site
            ,state_site
        )
    );
    ASSERT_EQUAL_TO(OverlapDimension(2),actual_boundary.overlapDimension());
    ASSERT_EQUAL_TO(StateDimension(5),actual_boundary.stateDimension());
    complex<double> const expected_boundary[] =
        {c(0,10),c(-6,0),c(4,-6),c(5,4),c(-4,6)
        ,c(15,15),c(-15,9),c(-9,-21),c(21,0),c(9,21)
        }
    ;
    complex<double> const *actual = actual_boundary;
    BOOST_FOREACH(complex<double> expected, expected_boundary) {
        ASSERT_EQUAL(expected,*actual);
        ++actual;
    }

    return 0;
}
//@-leo
