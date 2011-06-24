//@+leo-ver=5-thin
//@+node:gcross.20110207005827.1776: * @file operators.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2050: ** << License >>
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
//@+node:gcross.20110207005827.1777: ** << Includes >>
#include <boost/container/vector.hpp>
#include <boost/foreach.hpp>
#include <boost/format.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <complex>
#include <illuminate.hpp>
#include <iostream>
#include <sstream>
#include <utility>

#include "operators.hpp"
#include "utilities.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::container::vector;
using boost::equal;
using boost::format;
using boost::generate;

using std::abs;
using std::cerr;
using std::endl;
using std::istringstream;
using std::ostringstream;
//@-<< Includes >>

//@+others
//@+node:gcross.20110207005827.1778: ** Tests
TEST_SUITE(Operators) {

//@+others
//@+node:gcross.20110207005827.1779: *3* Pauli
TEST_SUITE(Pauli) {

    using namespace Pauli;

    #define TEST_PAULI(Pauli,_00,_01,_10,_11) \
        TEST_CASE(Pauli) { \
            EXPECT_EQ(2u,Pauli.size1()); \
            EXPECT_EQ(2u,Pauli.size2()); \
            EXPECT_EQ(_00,Pauli(0,0)); \
            EXPECT_EQ(_01,Pauli(0,1)); \
            EXPECT_EQ(_10,Pauli(1,0)); \
            EXPECT_EQ(_11,Pauli(1,1)); \
        }

    TEST_PAULI(I,c(1,0),c(0,0),c(0,0),c(1,0))
    TEST_PAULI(X,c(0,0),c(1,0),c(1,0),c(0,0))
    TEST_PAULI(Y,c(0,0),c(0,-1),c(0,1),c(0,0))
    TEST_PAULI(Z,c(1,0),c(0,0),c(0,0),c(-1,0))

    #undef TEST_PAULI
}
//@-others

}
//@-others
//@-leo
