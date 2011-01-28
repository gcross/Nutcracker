//@+leo-ver=5-thin
//@+node:gcross.20110127123226.2504: * @thin core.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110127123226.2505: ** << Includes >>
#include <boost/foreach.hpp>
#include <boost/range/algorithm/equal.hpp>
#include <illuminate.hpp>
#include <iostream>

#include "core.hpp"

using namespace boost;
using namespace Nutcracker;
using namespace std;
//@-<< Includes >>

//@+others
//@+node:gcross.20110127123226.2506: ** Tests
TEST_SUITE(Core) {

//@+others
//@+node:gcross.20110127123226.2507: *3* computeExpectationValue
TEST_SUITE(computeExpectationValue) {

    //@+others
    //@+node:gcross.20110127123226.2518: *4* trivial, all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        complex<double> expected_expectation_value = 1;
        complex<double> actual_expectation_value =
            computeExpectationValue(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Middle>::trivial
                ,OperatorSite::trivial
                ,ExpectationBoundary<Right>::trivial
            );
        ASSERT_EQ(expected_expectation_value,actual_expectation_value);
    }
    //@+node:gcross.20110127123226.2819: *4* trivial, all dimensions 1, imaginary
    TEST_CASE(trivial_with_all_dimensions_1_and_imaginary_component) {
        complex<double> expected_expectation_value(0,1);
        complex<double> actual_expectation_value =
            computeExpectationValue(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Middle>::trivial
                ,OperatorSite
                    (LeftDimension(1)
                    ,RightDimension(1)
                    ,list_of(1)(1)
                    ,list_of(complex<double>(0,1))
                    )
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
            complex<double> expectation_value =
                computeExpectationValue(
                     ExpectationBoundary<Left>::trivial
                    ,StateSite<Middle>
                        (LeftDimension(1)
                        ,RightDimension(1)
                        ,list_of(a)(b)
                        )
                    ,OperatorSite
                        (LeftDimension(1)
                        ,RightDimension(1)
                        ,list_of(1)(1)
                        ,list_of(1)(0)(0)(-1)
                        )
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
//@+node:gcross.20110127123226.2822: *3* computeSOSLeft
TEST_SUITE(computeSOSLeft) {

    //@+others
    //@+node:gcross.20110127123226.2823: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        auto_ptr<ExpectationBoundary<Left> const> new_boundary(
            contractSOSLeft(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Left>::trivial
                ,OperatorSite::trivial
            )
        );
        ASSERT_EQ(OperatorDimension(1),new_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2827: *4* trivial, c = 2
    TEST_CASE(trivial_with_operator_dimension_2) {
        auto_ptr<ExpectationBoundary<Left> const> new_boundary(
            contractSOSLeft(
                 ExpectationBoundary<Left>::trivial
                ,StateSite<Left>::trivial
                ,OperatorSite
                    (LeftDimension(1)
                    ,RightDimension(2)
                    ,list_of(1)(2)
                    ,list_of(complex<double>(0,1))
                    )
            )
        );
        ASSERT_EQ(OperatorDimension(2),new_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(0))(complex<double>(0,1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2875: *4* trivial, varied bandwidth dimensions
    TEST_CASE(trivial_with_varied_bandwidth_dimensions) {
        auto_ptr<ExpectationBoundary<Left> const> actual_boundary(
            contractSOSLeft(
                 ExpectationBoundary<Left>
                    (OperatorDimension(3)
                    ,list_of
                        (c(2,0))(c(0,4))
                        (c(1,0))(c(0,5))

                        (c(1,0))(c(0,5))
                        (c(1,0))(c(1,0))

                        (c(3,0))(c(0,6))
                        (c(0,2))(c(-2,0))
                    )
                ,StateSite<Left>
                    (LeftDimension(2)
                    ,RightDimension(4)
                    ,list_of
                        (c(1,0))(c(1,0))(c(-1,0))(c(0,2))
                        (c(1,0))(c(-1,0))(c(2,0))(c(-2,0))
                    )
                ,OperatorSite
                    (LeftDimension(3)
                    ,RightDimension(2)
                    ,list_of(1)(2)(3)(1)
                    ,list_of(complex<double>(1,0))(complex<double>(0,1))
                    )
            )
        );
        ASSERT_EQ(OperatorDimension(2),actual_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(4),actual_boundary->state_dimension);
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
        complex<double> const *actual = *actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@+node:gcross.20110127123226.2831: *3* computeSOSRight
TEST_SUITE(computeSOSRight) {

    //@+others
    //@+node:gcross.20110127123226.2832: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        auto_ptr<ExpectationBoundary<Right> const> new_boundary(
            contractSOSRight(
                 ExpectationBoundary<Right>::trivial
                ,StateSite<Right>::trivial
                ,OperatorSite::trivial
            )
        );
        ASSERT_EQ(OperatorDimension(1),new_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2833: *4* trivial, c = 2
    TEST_CASE(trivial_with_operator_dimension_2) {
        auto_ptr<ExpectationBoundary<Right> const> new_boundary(
            contractSOSRight(
                 ExpectationBoundary<Right>::trivial
                ,StateSite<Right>::trivial
                ,OperatorSite
                    (LeftDimension(2)
                    ,RightDimension(1)
                    ,list_of(2)(1)
                    ,list_of(complex<double>(0,1))
                    )
            )
        );
        ASSERT_EQ(OperatorDimension(2),new_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(0))(complex<double>(0,1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2873: *4* trivial, varied bandwidth dimensions
    TEST_CASE(trivial_with_varied_bandwidth_dimensions) {
        auto_ptr<ExpectationBoundary<Right> const> actual_boundary(
            contractSOSRight(
                 ExpectationBoundary<Right>
                    (OperatorDimension(3)
                    ,list_of
                        (c(2,0))(c(0,4))
                        (c(1,0))(c(0,5))

                        (c(1,0))(c(0,5))
                        (c(1,0))(c(1,0))

                        (c(3,0))(c(0,6))
                        (c(0,2))(c(-2,0))
                    )
                ,StateSite<Right>
                    (LeftDimension(4)
                    ,RightDimension(2)
                    ,list_of
                        (c(1,0))(c(1,0))
                        (c(1,0))(c(-1,0))
                        (c(-1,0))(c(2,0))
                        (c(0,2))(c(-2,0))
                    )
                ,OperatorSite
                    (LeftDimension(2)
                    ,RightDimension(3)
                    ,list_of(2)(1)(1)(3)
                    ,list_of(complex<double>(1,0))(complex<double>(0,1))
                    )
            )
        );
        ASSERT_EQ(OperatorDimension(2),actual_boundary->operator_dimension);
        ASSERT_EQ(StateDimension(4),actual_boundary->state_dimension);
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
        complex<double> const *actual = *actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@+node:gcross.20110127123226.2837: *3* computeSSLeft
TEST_SUITE(computeSSLeft) {

    //@+others
    //@+node:gcross.20110127123226.2838: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        auto_ptr<OverlapBoundary<Left> const> new_boundary(
            contractSSLeft(
                 OverlapBoundary<Left>::trivial
                ,OverlapSite<Left>::trivial
                ,StateSite<Left>::trivial
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2849: *4* trivial, physical dimension 2
    TEST_CASE(trivial_with_physical_dimension_2) {
        auto_ptr<OverlapBoundary<Left> const> new_boundary(
            contractSSLeft(
                 OverlapBoundary<Left>::trivial
                ,OverlapSite<Left>
                    (RightDimension(1)
                    ,LeftDimension(1)
                    ,list_of(5)(-1)
                    )
                ,StateSite<Left>
                    (LeftDimension(1)
                    ,RightDimension(1)
                    ,list_of(1)(1)
                    )
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(4)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2851: *4* trivial, varied bandwidth dimensions
    TEST_CASE(trivial_with_varied_bandwidth_dimensions) {
        auto_ptr<OverlapBoundary<Left> const> actual_boundary(
            contractSSLeft(
                 OverlapBoundary<Left>
                    (OverlapDimension(3)
                    ,list_of
                        (c(2,0))(c(0,4))
                        (c(1,0))(c(0,5))
                        (c(3,0))(c(0,6))
                    )
                ,OverlapSite<Left>
                    (RightDimension(2)
                    ,LeftDimension(3)
                    ,list_of
                        (c( 1,0))(c( 0,-1))(c(0, 1))
                        (c( 1,0))(c( 1, 0))(c(0,-1))
                    )
                ,StateSite<Left>
                    (LeftDimension(2)
                    ,RightDimension(5)
                    ,list_of
                        (c(1,0))(c(1,0))(c( 1,0))(c(-1, 0))(c(-1,0))
                        (c(2,0))(c(0,2))(c(-2,0))(c( 1,-2))(c( 2,0))
                    )
            )
        );
        ASSERT_EQ(OverlapDimension(2),actual_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(5),actual_boundary->state_dimension);
        complex<double> const expected_boundary[] =
            {c(0,10),c(-6,0),c(4,-6),c(5,4),c(-4,6)
            ,c(15,15),c(-15,9),c(-9,-21),c(21,0),c(9,21)
            }
        ;
        complex<double> const *actual = *actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@+node:gcross.20110127123226.2862: *3* computeSSRight
TEST_SUITE(computeSSRight) {

    //@+others
    //@+node:gcross.20110127123226.2863: *4* trivial with all dimensions 1
    TEST_CASE(trivial_with_all_dimensions_1) {
        auto_ptr<OverlapBoundary<Right> const> new_boundary(
            contractSSRight(
                 OverlapBoundary<Right>::trivial
                ,OverlapSite<Right>::trivial
                ,StateSite<Right>::trivial
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(1)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2864: *4* trivial, physical dimension 2
    TEST_CASE(trivial_with_physical_dimension_2) {
        auto_ptr<OverlapBoundary<Right> const> new_boundary(
            contractSSRight(
                 OverlapBoundary<Right>::trivial
                ,OverlapSite<Right>
                    (RightDimension(1)
                    ,LeftDimension(1)
                    ,list_of(5)(-1)
                    )
                ,StateSite<Right>
                    (LeftDimension(1)
                    ,RightDimension(1)
                    ,list_of(1)(1)
                    )
            )
        );
        ASSERT_EQ(OverlapDimension(1),new_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(1),new_boundary->state_dimension);
        ASSERT_TRUE(equal(list_of(complex<double>(4)),*new_boundary));
    }
    //@+node:gcross.20110127123226.2865: *4* trivial, varied bandwidth dimensions
    TEST_CASE(trivial_with_varied_bandwidth_dimensions) {
        auto_ptr<OverlapBoundary<Right> const> actual_boundary(
            contractSSRight(
                 OverlapBoundary<Right>
                    (OverlapDimension(2)
                    ,list_of
                        (c(2,0))(c(1,0))
                        (c(3,0))(c(1,1))
                        (c(1,2))(c(0,6))
                        (c(0,4))(c(0,5))
                        (c(1,1))(c(2,1))
                    )
                ,OverlapSite<Right>
                    (RightDimension(2)
                    ,LeftDimension(3)
                    ,list_of
                        (c( 1,0))(c( 0,-1))(c(0, 1))
                        (c( 1,0))(c( 1, 0))(c(0,-1))
                    )
                ,StateSite<Right>
                    (LeftDimension(2)
                    ,RightDimension(5)
                    ,list_of
                        (c(1,0))(c(1,0))(c( 1,0))(c(-1, 0))(c(-1,0))
                        (c(2,0))(c(0,2))(c(-2,0))(c( 1,-2))(c( 2,0))
                    )
            )
        );
        ASSERT_EQ(OverlapDimension(3),actual_boundary->overlap_dimension);
        ASSERT_EQ(StateDimension(2),actual_boundary->state_dimension);
        complex<double> const expected_boundary[] =
            {c(5,-2),c(-3,-4),c(4,5)
            ,c(26,5),c(22,-15),c(-11,-2)
            }
        ;
        complex<double> const *actual = *actual_boundary;
        BOOST_FOREACH(complex<double> expected, expected_boundary) {
            ASSERT_EQ(expected,*actual);
            ++actual;
        }
    }
    //@-others

}
//@-others

}
//@-others
//@-leo
