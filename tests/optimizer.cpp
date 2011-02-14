//@+leo-ver=5-thin
//@+node:gcross.20110214155808.2016: * @thin optimizer.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110214155808.2017: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <illuminate.hpp>

#include "optimizer.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
using boost::none;
//@-<< Includes >>

//@+others
//@+node:gcross.20110214155808.2018: ** Tests
TEST_SUITE(Optimizer) {

//@+others
//@+node:gcross.20110214155808.2045: *3* optimizeStateSite
TEST_SUITE(optimizeStateSite) {

//@+others
//@+node:gcross.20110214155808.2046: *4* trivial, d = 4
TEST_CASE(trivial_with_physical_dimension_4) {
    RNG rng;

    StateSite<Middle> state_site(
         PhysicalDimension(4)
        ,LeftDimension(1)
        ,RightDimension(1)
        ,fillWithGenerator(rng.randomComplexDouble)
    );

    OperatorSite operator_site(
         LeftDimension(1)
        ,RightDimension(1)
        ,fillWithRange(list_of(1)(1))
        ,fillWithRange(list_of
            ( 1)( 0)( 0)( 0)
            ( 0)( 1)( 0)( 0)
            ( 0)( 0)( 1)( 0)
            ( 0)( 0)( 0)(-1)
         )
    );

    OptimizerResult optimizer_result(
        optimizeStateSite(
             ExpectationBoundary<Left>::trivial
            ,state_site
            ,operator_site
            ,ExpectationBoundary<Right>::trivial
            ,none
            ,0
            ,10000
        )
    );

    StateSite<Middle> const& new_state_site = optimizer_result.state_site;

    ASSERT_EQ(4,new_state_site.physicalDimension(as_unsigned_integer));
    ASSERT_EQ(1,new_state_site.leftDimension(as_unsigned_integer));
    ASSERT_EQ(1,new_state_site.rightDimension(as_unsigned_integer));
    ASSERT_NEAR(c(0,0),new_state_site[0],1e-7);
    ASSERT_NEAR(c(0,0),new_state_site[1],1e-7);
    ASSERT_NEAR(c(0,0),new_state_site[2],1e-7);
    ASSERT_NEAR(1,abs(new_state_site[3]),1e-7);
    ASSERT_NEAR(-1,optimizer_result.eigenvalue,1e-7);
    ASSERT_EQ(0,optimizer_result.number_of_iterations);
}
//@-others

}
//@-others

}
//@-others
//@-leo
