#include <boost/make_shared.hpp>
#include <boost/range/algorithm/generate.hpp>
#include <illuminate.hpp>

#include "nutcracker/utilities.hpp"

#include "test_utils.hpp"

using boost::make_shared;

TEST_SUITE(Utilities) {
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
}
