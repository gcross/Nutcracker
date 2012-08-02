//@+leo-ver=5-thin
//@+node:gcross.20110214155808.2016: * @file optimizer.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110214155808.2017: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <complex>
#include <illuminate.hpp>

#include "nutcracker/operators.hpp"
#include "nutcracker/optimizer.hpp"
#include "nutcracker/states.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
using boost::none;

using std::abs;
//@-<< Includes >>

//@+others
//@+node:gcross.20110214155808.2018: ** Tests
TEST_SUITE(Optimizer) {

//@+others
//@+node:gcross.20110214155808.2045: *3* optimizeStateSite
TEST_SUITE(optimizeStateSite) {

//@+others
//@+node:gcross.20110214155808.2046: *4* one site under external field
TEST_SUITE(one_site_under_external_field) {

    void runTests(unsigned int const physical_dimension) {
        REPEAT(10) {
            StateSite<Middle> state_site(
                randomStateSiteMiddle(
                     PhysicalDimension(physical_dimension)
                    ,LeftDimension(1)
                    ,RightDimension(1)
                )
            );

            vector<complex<double> > diagonal(physical_dimension,c(1,0));
            diagonal.back() = c(-1,0);
            OperatorSite operator_site(
                constructOperatorSite(
                     PhysicalDimension(physical_dimension)
                    ,LeftDimension(1)
                    ,RightDimension(1)
                    ,list_of(OperatorSiteLink(1,1,diagonalMatrix(diagonal)))
                )

            );

            OptimizerResult optimizer_result(
                optimizeStateSite(
                     ExpectationBoundary<Left>::trivial
                    ,state_site
                    ,operator_site
                    ,ExpectationBoundary<Right>::trivial
                    ,ProjectorMatrix()
                    ,0
                    ,0
                    ,10000
                )
            );

            StateSite<Middle> const& new_state_site = optimizer_result.state_site;

            ASSERT_EQ(physical_dimension,new_state_site.physicalDimension());
            ASSERT_EQ(1u,new_state_site.leftDimension());
            ASSERT_EQ(1u,new_state_site.rightDimension());
            BOOST_FOREACH(unsigned int const i, irange(0u,physical_dimension-1)) {
                ASSERT_NEAR_REL(c(0,0),new_state_site[i],1e-15);
            }
            ASSERT_NEAR_REL(1,abs(new_state_site[physical_dimension-1]),1e-5);
            ASSERT_NEAR_REL(-1,optimizer_result.eigenvalue,1e-5);
            ASSERT_EQ(0u,optimizer_result.number_of_iterations);
        }
    }

    TEST_CASE(physical_dimension_1) { runTests(1); }
    TEST_CASE(physical_dimension_2) { runTests(2); }
    TEST_CASE(physical_dimension_3) { runTests(3); }
    TEST_CASE(physical_dimension_4) { runTests(4); }
}
//@+node:gcross.20110518200233.5044: *4* optimizer modes
TEST_SUITE(optimizer_modes) {

    void runTests(
        string const& name
      , double v
      , complex<double> a
      , complex<double> b
      , complex<double> c
      , complex<double> d
    ) {
        REPEAT(10) {
            StateSite<Middle> state_site(
                randomStateSiteMiddle(
                     PhysicalDimension(4)
                    ,LeftDimension(1)
                    ,RightDimension(1)
                )
            );

            OperatorSite operator_site(
                constructOperatorSite(
                     PhysicalDimension(4)
                    ,LeftDimension(1)
                    ,RightDimension(1)
                    ,list_of(OperatorSiteLink(1,1,diagonalMatrix(list_of(a)(b)(c)(d))))
                )

            );

            OptimizerResult optimizer_result(
                optimizeStateSite(
                     ExpectationBoundary<Left>::trivial
                    ,state_site
                    ,operator_site
                    ,ExpectationBoundary<Right>::trivial
                    ,ProjectorMatrix()
                    ,0
                    ,0
                    ,10000
                    ,OptimizerMode::lookupName(name)
                )
            );

            StateSite<Middle> const& new_state_site = optimizer_result.state_site;

            ASSERT_NEAR_REL(0,abs(new_state_site[0]),1e-15);
            ASSERT_NEAR_REL(0,abs(new_state_site[1]),1e-15);
            ASSERT_NEAR_REL(0,abs(new_state_site[2]),1e-15);
            ASSERT_NEAR_REL(1,abs(new_state_site[3]),1e-5);
            ASSERT_NEAR_REL(v,optimizer_result.eigenvalue,1e-5);
            ASSERT_EQ(0u,optimizer_result.number_of_iterations);
        }
    }

    TEST_CASE(least_value) { runTests("<v",-2,4,0,-1,-2); }
    TEST_CASE(greatest_value) { runTests(">v",2,-4,0,1,2); }
    TEST_SUITE(largest_magnitude) {
        TEST_CASE(negative) { runTests(">m",-4,2,0,1,-4); }
        TEST_CASE(positive) { runTests(">m",4,-2,0,-1,4); }
    }
}
//@-others

}
//@-others

}
//@-others
//@-leo
