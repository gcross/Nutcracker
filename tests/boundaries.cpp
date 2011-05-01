//@+leo-ver=5-thin
//@+node:gcross.20110127123226.2504: * @file boundaries.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2046: ** << License >>
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
//@+node:gcross.20110127123226.2505: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <illuminate.hpp>

#include "boundaries.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
using boost::equal;
//@-<< Includes >>

//@+others
//@+node:gcross.20110129220506.1653: ** Tests
TEST_SUITE(Boundaries) {

//@+others
//@+node:gcross.20110127123226.2507: *3* computeExpectationValueAtSite
TEST_SUITE(computeExpectationValueAtSite) {

    //@+others
    //@+node:gcross.20110127123226.2518: *4* trivial, all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        complex<double> expected_expectation_value = 1;
        complex<double> actual_expectation_value =
            computeExpectationValueAtSite(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Middle>::trivial
                ,OperatorSite::trivial
                ,ExpectationBoundary<Right>::trivial
            );
        ASSERT_EQ(expected_expectation_value,actual_expectation_value);
    }
    //@+node:gcross.20110127123226.2819: *4* trivial, all dimensions 1, imaginary
    TEST_CASE(trivial_with_all_dimensions_1_and_imaginary_component) {
        OperatorSite const operator_site
            (LeftDimension(1)
            ,RightDimension(1)
            ,fillWithRange(list_of(1)(1))
            ,fillWithRange(list_of(complex<double>(0,1)))
            );

        complex<double> expected_expectation_value(0,1);
        complex<double> actual_expectation_value =
            computeExpectationValueAtSite(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Middle>::trivial
                ,operator_site
                ,ExpectationBoundary<Right>::trivial
            );
        ASSERT_EQ(expected_expectation_value,actual_expectation_value);
    }
    //@+node:gcross.20110127123226.2821: *4* trivial, d = 2
    TEST_SUITE(trivial_with_physical_dimension_2) {

        void runTest(
              complex<double> const a
            , complex<double> const b
            , complex<double> const c
        ) {
            StateSite<Middle> const state_site
                (LeftDimension(1)
                ,RightDimension(1)
                ,fillWithRange(list_of(a)(b))
                );
            OperatorSite const operator_site
                (LeftDimension(1)
                ,RightDimension(1)
                ,fillWithRange(list_of(1)(1))
                ,fillWithRange(list_of(1)(0)(0)(-1))
                );
            complex<double> expectation_value =
                computeExpectationValueAtSite(
                     ExpectationBoundary<Left>::trivial
                    ,state_site
                    ,operator_site
                    ,ExpectationBoundary<Right>::trivial
                );
            ASSERT_EQ(c,expectation_value);
        }

        TEST_CASE(_1_0) { runTest(1,0,1); }
        TEST_CASE(_i_0) { runTest(complex<double>(0,1),0,1); }
        TEST_CASE(_0_1) { runTest(0,1,-1); }
        TEST_CASE(_0_i) { runTest(0,complex<double>(0,1),-1); }
    }
    //@-others

}
//@+node:gcross.20110128131637.1649: *3* contractExpectationBoundaries
TEST_SUITE(contractExpectationBoundaries) {

    //@+others
    //@+node:gcross.20110128131637.1650: *4* trivial, all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        complex<double> expected_value = 1;
        complex<double> actual_value =
            contractExpectationBoundaries(
                 ExpectationBoundary<Left>::trivial
                ,ExpectationBoundary<Right>::trivial
            );
        ASSERT_EQ(expected_value,actual_value);
    }
    //@+node:gcross.20110128131637.1654: *4* non-trivial
    TEST_CASE(non_trivial) {

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

        complex<double> expected_value = c(405,495);
        complex<double> actual_value =
            contractExpectationBoundaries(
                 left_boundary
                ,right_boundary
            );
        ASSERT_EQ(expected_value,actual_value);
    }
    //@-others

}
//@+node:gcross.20110127123226.2822: *3* contractSOSLeft
TEST_SUITE(computeSOSLeft) {

    //@+others
    //@+node:gcross.20110127123226.2823: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        ExpectationBoundary<Left> const new_boundary(
            contractSOSLeft(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Left>::trivial
                ,OperatorSite::trivial
            )
        );
        ASSERT_EQ(OperatorDimension(1),new_boundary.operatorDimension());
        ASSERT_EQ(StateDimension(1),new_boundary.stateDimension());
        ASSERT_TRUE(equal(list_of(complex<double>(1)),new_boundary));
    }
    //@+node:gcross.20110127123226.2827: *4* trivial, c = 2
    TEST_CASE(trivial_with_operator_dimension_2) {
        OperatorSite const operator_site
            (LeftDimension(1)
            ,RightDimension(2)
            ,fillWithRange(list_of(1)(2))
            ,fillWithRange(list_of(complex<double>(0,1)))
            );
        ExpectationBoundary<Left> const new_boundary(
            contractSOSLeft(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Left>::trivial
                ,operator_site
            )
        );
        ASSERT_EQ(OperatorDimension(2),new_boundary.operatorDimension());
        ASSERT_EQ(StateDimension(1),new_boundary.stateDimension());
        ASSERT_TRUE(equal(list_of(complex<double>(0))(complex<double>(0,1)),new_boundary));
    }
    //@+node:gcross.20110127123226.2875: *4* non-trivial
    TEST_CASE(non_trivial) {
        ExpectationBoundary<Left> const boundary
            (OperatorDimension(3)
            ,fillWithRange(list_of
                (c(2,0))(c(0,4))
                (c(1,0))(c(0,5))

                (c(1,0))(c(0,5))
                (c(1,0))(c(1,0))

                (c(3,0))(c(0,6))
                (c(0,2))(c(-2,0))
            ));
        StateSite<Left> const state_site
            (LeftDimension(2)
            ,RightDimension(4)
            ,fillWithRange(list_of
                (c(1,0))(c(1,0))(c(-1,0))(c(0,2))
                (c(1,0))(c(-1,0))(c(2,0))(c(-2,0))
            ));
        OperatorSite const operator_site
            (LeftDimension(3)
            ,RightDimension(2)
            ,fillWithRange(list_of(1)(2)(3)(1))
            ,fillWithRange(list_of(complex<double>(1,0))(complex<double>(0,1)))
            );
        ExpectationBoundary<Left> const actual_boundary(
            contractSOSLeft(
                 boundary
                ,state_site
                ,operator_site
            )
        );
        ASSERT_EQ(OperatorDimension(2),actual_boundary.operatorDimension());
        ASSERT_EQ(StateDimension(4),actual_boundary.stateDimension());
        complex<double> const expected_boundary[] =
            {c(-8.0,1.0),c(4.0,5.0),c(-10.0,-7.0),c(6.0,0.0)
            ,c(-4.0,5.0),c(8.0,1.0),c(-14.0,1.0),c(6.0,0.0)
            ,c(2.0,-7.0),c(-10.0,1.0),c(16.0,-5.0),c(-6.0,0.0)
            ,c(10.0,16.0),c(10.0,-16.0),c(-10.0,32.0),c(0.0,-12.0)

            ,c(3.0,9.0),c(3.0,-9.0),c(-3.0,18.0),c(0.0,-12.0)
            ,c(1.0,-1.0),c(1.0,1.0),c(-1.0,-2.0),c(0.0,4.0)
            ,c(0.0,6.0),c(0.0,-6.0),c(0.0,12.0),c(0.0,-12.0)
            ,c(6.0,-14.0),c(-10.0,6.0),c(18.0,-16.0),c(-8.0,16.0)
            };
        complex<double> const *actual = actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@+node:gcross.20110127123226.2831: *3* contractSOSRight
TEST_SUITE(computeSOSRight) {

    //@+others
    //@+node:gcross.20110127123226.2832: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        ExpectationBoundary<Right> const new_boundary(
            contractSOSRight(
                 ExpectationBoundary<Right>::trivial
                ,StateSite<Right>::trivial
                ,OperatorSite::trivial
            )
        );
        ASSERT_EQ(OperatorDimension(1),new_boundary.operatorDimension());
        ASSERT_EQ(StateDimension(1),new_boundary.stateDimension());
        ASSERT_TRUE(equal(list_of(complex<double>(1)),new_boundary));
    }
    //@+node:gcross.20110127123226.2833: *4* trivial, c = 2
    TEST_CASE(trivial_with_operator_dimension_2) {
        OperatorSite const operator_site
            (LeftDimension(2)
            ,RightDimension(1)
            ,fillWithRange(list_of(2)(1))
            ,fillWithRange(list_of(complex<double>(0,1)))
            );
        ExpectationBoundary<Right> const new_boundary(
            contractSOSRight(
                 ExpectationBoundary<Right>::trivial
                ,StateSite<Right>::trivial
                ,operator_site
            )
        );
        ASSERT_EQ(OperatorDimension(2),new_boundary.operatorDimension());
        ASSERT_EQ(StateDimension(1),new_boundary.stateDimension());
        ASSERT_TRUE(equal(list_of(complex<double>(0))(complex<double>(0,1)),new_boundary));
    }
    //@+node:gcross.20110127123226.2873: *4* non-trivial
    TEST_CASE(non_trivial) {
        ExpectationBoundary<Right> const boundary
            (OperatorDimension(3)
            ,fillWithRange(list_of
                (c(2,0))(c(0,4))
                (c(1,0))(c(0,5))

                (c(1,0))(c(0,5))
                (c(1,0))(c(1,0))

                (c(3,0))(c(0,6))
                (c(0,2))(c(-2,0))
            ));
        StateSite<Right> const state_site
            (LeftDimension(4)
            ,RightDimension(2)
            ,fillWithRange(list_of
                (c(1,0))(c(1,0))
                (c(1,0))(c(-1,0))
                (c(-1,0))(c(2,0))
                (c(0,2))(c(-2,0))
            ));
        OperatorSite const operator_site
            (LeftDimension(2)
            ,RightDimension(3)
            ,fillWithRange(list_of(2)(1)(1)(3))
            ,fillWithRange(list_of(complex<double>(1,0))(complex<double>(0,1)))
            );
        ExpectationBoundary<Right> const actual_boundary(
            contractSOSRight(
                 boundary
                ,state_site
                ,operator_site
            )
        );
        ASSERT_EQ(OperatorDimension(2),actual_boundary.operatorDimension());
        ASSERT_EQ(StateDimension(4),actual_boundary.stateDimension());
        complex<double> const expected_boundary[] =
            {c(-8.0,1.0),c(4.0,5.0),c(-10.0,-7.0),c(18.0,8.0)
            ,c(-4.0,5.0),c(8.0,1.0),c(-14.0,1.0),c(18.0,-8.0)
            ,c(2.0,-7.0),c(-10.0,1.0),c(16.0,-5.0),c(-18.0,16.0)
            ,c(-2.0,-8.0),c(-2.0,8.0),c(2.0,-16.0),c(0.0,20.0)

            ,c(3.0,9.0),c(3.0,-9.0),c(-3.0,18.0),c(0.0,-24.0)
            ,c(1.0,-1.0),c(1.0,1.0),c(-1.0,-2.0),c(0.0,0.0)
            ,c(0.0,6.0),c(0.0,-6.0),c(0.0,12.0),c(0.0,-12.0)
            ,c(-10.0,-6.0),c(6.0,14.0),c(-14.0,-24.0),c(24.0,24.0)
            };
        complex<double> const *actual = actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@+node:gcross.20110127123226.2837: *3* contractSSLeft
TEST_SUITE(computeSSLeft) {

    //@+others
    //@+node:gcross.20110127123226.2838: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        OverlapBoundary<Left> const new_boundary(
            contractSSLeft(
                 OverlapBoundary<Left>::trivial
                ,OverlapSite<Left>::trivial
                ,StateSite<Left>::trivial
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary.overlapDimension());
        ASSERT_EQ(StateDimension(1),new_boundary.stateDimension());
        ASSERT_TRUE(equal(list_of(complex<double>(1)),new_boundary));
    }
    //@+node:gcross.20110127123226.2849: *4* trivial, physical dimension 2
    TEST_CASE(trivial_with_physical_dimension_2) {
        OverlapSite<Left> const overlap_site
            (RightDimension(1)
            ,LeftDimension(1)
            ,fillWithRange(list_of(5)(-1))
            );
        StateSite<Left> const state_site
            (LeftDimension(1)
            ,RightDimension(1)
            ,fillWithRange(list_of(1)(1))
            );
        OverlapBoundary<Left> const new_boundary(
            contractSSLeft(
                 OverlapBoundary<Left>::trivial
                ,overlap_site
                ,state_site
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary.overlapDimension());
        ASSERT_EQ(StateDimension(1),new_boundary.stateDimension());
        ASSERT_TRUE(equal(list_of(complex<double>(4)),new_boundary));
    }
    //@+node:gcross.20110127123226.2851: *4* non-trivial
    TEST_CASE(non_trivial) {
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
        ASSERT_EQ(OverlapDimension(2),actual_boundary.overlapDimension());
        ASSERT_EQ(StateDimension(5),actual_boundary.stateDimension());
        complex<double> const expected_boundary[] =
            {c(0,10),c(-6,0),c(4,-6),c(5,4),c(-4,6)
            ,c(15,15),c(-15,9),c(-9,-21),c(21,0),c(9,21)
            }
        ;
        complex<double> const *actual = actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@+node:gcross.20110127123226.2862: *3* contractSSRight
TEST_SUITE(computeSSRight) {

    //@+others
    //@+node:gcross.20110127123226.2863: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        OverlapBoundary<Right> const new_boundary(
            contractSSRight(
                 OverlapBoundary<Right>::trivial
                ,OverlapSite<Right>::trivial
                ,StateSite<Right>::trivial
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary.overlapDimension());
        ASSERT_EQ(StateDimension(1),new_boundary.stateDimension());
        ASSERT_TRUE(equal(list_of(complex<double>(1)),new_boundary));
    }
    //@+node:gcross.20110127123226.2864: *4* trivial, physical dimension 2
    TEST_CASE(trivial_with_physical_dimension_2) {
        OverlapSite<Right> const overlap_site
            (RightDimension(1)
            ,LeftDimension(1)
            ,fillWithRange(list_of(5)(-1)
            ));
        StateSite<Right> const state_site
            (LeftDimension(1)
            ,RightDimension(1)
            ,fillWithRange(list_of(1)(1)
            ));
        OverlapBoundary<Right> const new_boundary(
            contractSSRight(
                 OverlapBoundary<Right>::trivial
                ,overlap_site
                ,state_site
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary.overlapDimension());
        ASSERT_EQ(StateDimension(1),new_boundary.stateDimension());
        ASSERT_TRUE(equal(list_of(complex<double>(4)),new_boundary));
    }
    //@+node:gcross.20110127123226.2865: *4* non-trivial
    TEST_CASE(non_trivial) {
        OverlapBoundary<Right> const boundary
            (OverlapDimension(2)
            ,fillWithRange(list_of
                (c(2,0))(c(1,0))
                (c(3,0))(c(1,1))
                (c(1,2))(c(0,6))
                (c(0,4))(c(0,5))
                (c(1,1))(c(2,1))
            ));
        OverlapSite<Right> const overlap_site
            (RightDimension(2)
            ,LeftDimension(3)
            ,fillWithRange(list_of
                (c( 1,0))(c( 0,-1))(c(0, 1))
                (c( 1,0))(c( 1, 0))(c(0,-1))
            ));
        StateSite<Right> const state_site
            (LeftDimension(2)
            ,RightDimension(5)
            ,fillWithRange(list_of
                (c(1,0))(c(1,0))(c( 1,0))(c(-1, 0))(c(-1,0))
                (c(2,0))(c(0,2))(c(-2,0))(c( 1,-2))(c( 2,0))
            ));
        OverlapBoundary<Right> const actual_boundary(
            contractSSRight(
                 boundary
                ,overlap_site
                ,state_site
            )
        );
        ASSERT_EQ(OverlapDimension(3),actual_boundary.overlapDimension());
        ASSERT_EQ(StateDimension(2),actual_boundary.stateDimension());
        complex<double> const expected_boundary[] =
            {c(5,-2),c(-3,-4),c(4,5)
            ,c(26,5),c(22,-15),c(-11,-2)
            }
        ;
        complex<double> const *actual = actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@+node:gcross.20110129220506.1651: *3* consistent
TEST_CASE(consistent) {

    RNG random;

    REPEAT(10) {

        unsigned int const
             left_operator_dimension = random
            ,left_state_dimension = random
            ,physical_dimension = random
            ,right_operator_dimension = random
            ,right_state_dimension = random
            ,number_of_matrices = random
            ;
        ExpectationBoundary<Left> const left_boundary
            (OperatorDimension(left_operator_dimension)
            ,StateDimension(left_state_dimension)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        StateSite<Middle> const state_site
            (PhysicalDimension(physical_dimension)
            ,LeftDimension(left_state_dimension)
            ,RightDimension(right_state_dimension)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        StateSite<Left> const left_state_site(copyFrom(state_site));
        StateSite<Right> const right_state_site(copyFrom(state_site));
        OperatorSite const operator_site
            (number_of_matrices
            ,PhysicalDimension(physical_dimension)
            ,LeftDimension(left_operator_dimension)
            ,RightDimension(right_operator_dimension)
            ,fillWithGenerator(random.generateRandomIndices(
                 LeftDimension(left_operator_dimension)
                ,RightDimension(right_operator_dimension)
             ))
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ExpectationBoundary<Right> const right_boundary
            (OperatorDimension(right_operator_dimension)
            ,StateDimension(right_state_dimension)
            ,fillWithGenerator(random.randomComplexDouble)
            );
        ExpectationBoundary<Left> const new_left_boundary(
            contract<Left>::SOS(
                 left_boundary
                ,left_state_site
                ,operator_site
            )
        );
        ExpectationBoundary<Right> const new_right_boundary(
            contract<Right>::SOS(
                 right_boundary
                ,right_state_site
                ,operator_site
            )
        );

        complex<double> const
             result_from_computeExpectationValue =
                computeExpectationValueAtSite(
                     left_boundary
                    ,state_site
                    ,operator_site
                    ,right_boundary
                )
            ,result_from_contractSOSLeft =
                contractExpectationBoundaries(
                     new_left_boundary
                    ,right_boundary
                )
            ,result_from_contractSOSRight =
                contractExpectationBoundaries(
                     left_boundary
                    ,new_right_boundary
                )
            ;
        ASSERT_NEAR_QUOTED(result_from_computeExpectationValue,result_from_contractSOSLeft,1e-10);
        ASSERT_NEAR_QUOTED(result_from_computeExpectationValue,result_from_contractSOSRight,1e-10);
    }
}
//@-others

}
//@-others
//@-leo
