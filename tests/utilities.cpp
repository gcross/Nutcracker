//@+leo-ver=5-thin
//@+node:gcross.20110815001337.2493: * @file utilities.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110815001337.2494: ** << License >>
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
//@+node:gcross.20110815001337.2495: ** << Includes >>
#include <boost/make_shared.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <illuminate.hpp>

#include "utilities.hpp"

#include "test_utils.hpp"

using boost::make_shared;
//@-<< Includes >>

//@+others
//@+node:gcross.20110815001337.2496: ** Tests
TEST_SUITE(Utilities) {
//@+others
//@+node:gcross.20110815001337.2497: *3* MatrixConstPtrComparisonPredicate
TEST_SUITE(MatrixConstPtrComparisonPredicate) {
    //@+others
    //@+node:gcross.20110815001337.2498: *4* x smaller than y
    TEST_CASE(x_smaller_than_y) {
        RNG random;

        std::less<MatrixConstPtr> predicate = std::less<MatrixConstPtr>();

        REPEAT(10) {
            unsigned int dx = random, dy = dx + random(1,5);
            MatrixPtr x(new Matrix(dx,dx)), y(new Matrix(dy,dy));
            boost::generate(x->data(),random.randomComplexDouble);
            boost::generate(y->data(),random.randomComplexDouble);
            ASSERT_TRUE(predicate(x,y));
        }
    }
    //@+node:gcross.20110815001337.2501: *4* x larger than y
    TEST_CASE(x_larger_than_y) {
        RNG random;

        std::less<MatrixConstPtr> predicate = std::less<MatrixConstPtr>();

        REPEAT(10) {
            unsigned int dy = random, dx = dy + random(1,5);
            MatrixPtr x(new Matrix(dx,dx)), y(new Matrix(dy,dy));
            boost::generate(x->data(),random.randomComplexDouble);
            boost::generate(y->data(),random.randomComplexDouble);
            ASSERT_FALSE(predicate(x,y));
        }
    }
    //@+node:gcross.20110815001337.2502: *4* size 1
    TEST_SUITE(size_1) {
    //@+others
    //@+node:gcross.20110815001337.2511: *5* equal
    TEST_CASE(equal) {
        std::less<MatrixConstPtr> predicate = std::less<MatrixConstPtr>();
        ASSERT_FALSE(predicate(make_shared<Matrix>(1,1,c(1,1)),make_shared<Matrix>(1,1,c(1,1))));
        ASSERT_FALSE(predicate(make_shared<Matrix>(1,1,c(2,-1)),make_shared<Matrix>(1,1,c(2,-1))));
    }
    //@+node:gcross.20110815001337.2509: *5* greater
    TEST_CASE(greater) {
        std::less<MatrixConstPtr> predicate = std::less<MatrixConstPtr>();
        ASSERT_FALSE(predicate(make_shared<Matrix>(1,1,c(1,1)),make_shared<Matrix>(1,1,c(1,0))));
        ASSERT_FALSE(predicate(make_shared<Matrix>(1,1,c(2,-1)),make_shared<Matrix>(1,1,c(1,0))));
    }
    //@+node:gcross.20110815001337.2503: *5* less
    TEST_CASE(less) {
        std::less<MatrixConstPtr> predicate = std::less<MatrixConstPtr>();
        ASSERT_TRUE(predicate(make_shared<Matrix>(1,1,c(1,0)),make_shared<Matrix>(1,1,c(1,1))));
        ASSERT_TRUE(predicate(make_shared<Matrix>(1,1,c(1,0)),make_shared<Matrix>(1,1,c(2,-1))));
    }
    //@-others
    }
    //@+node:gcross.20110815001337.2506: *4* size 2
    TEST_SUITE(size_2) {
    //@+others
    //@+node:gcross.20110815001337.2507: *5* less
    TEST_CASE(less) {
        std::less<MatrixConstPtr> predicate = std::less<MatrixConstPtr>();
        MatrixPtr x(new Matrix(2,2)), y(new Matrix(2,2));
        boost::copy(list_of(0)(0)(0)(0),x->data().begin());
        boost::copy(list_of(0)(0)(1)(0),y->data().begin());
        ASSERT_TRUE(predicate(x,y));
    }
    //@+node:gcross.20110815001337.2513: *5* greater
    TEST_CASE(greater) {
        std::less<MatrixConstPtr> predicate = std::less<MatrixConstPtr>();
        MatrixPtr x(new Matrix(2,2)), y(new Matrix(2,2));
        boost::copy(list_of(0)(0)(0)(0),x->data().begin());
        boost::copy(list_of(0)(0)(1)(0),y->data().begin());
        ASSERT_FALSE(predicate(y,x));
    }
    //@+node:gcross.20110815001337.2515: *5* equal
    TEST_CASE(equal) {
        std::less<MatrixConstPtr> predicate = std::less<MatrixConstPtr>();
        MatrixPtr x(new Matrix(2,2)), y(new Matrix(2,2));
        boost::copy(list_of(0)(0)(1)(0),x->data().begin());
        boost::copy(list_of(0)(0)(1)(0),y->data().begin());
        ASSERT_FALSE(predicate(x,y));
        ASSERT_FALSE(predicate(y,x));
    }
    //@-others
    }
    //@-others
}
//@+node:gcross.20110817110920.2494: *3* Pauli
TEST_SUITE(Pauli) {

    using namespace Pauli;

    #define TEST_PAULI(pauli,_00,_01,_10,_11) \
        TEST_CASE(pauli) { \
            EXPECT_EQ(2u,pauli->size1()); \
            EXPECT_EQ(2u,pauli->size2()); \
            EXPECT_EQ(_00,(*pauli)(0,0)); \
            EXPECT_EQ(_01,(*pauli)(0,1)); \
            EXPECT_EQ(_10,(*pauli)(1,0)); \
            EXPECT_EQ(_11,(*pauli)(1,1)); \
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
