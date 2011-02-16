//@+leo-ver=5-thin
//@+node:gcross.20110207005827.1776: * @thin operators.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110207005827.1777: ** << Includes >>
#include <illuminate.hpp>

#include "operators.hpp"
#include "utilities.hpp"

using namespace Nutcracker;
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
            EXPECT_EQ(2,Pauli.size1()); \
            EXPECT_EQ(2,Pauli.size2()); \
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
