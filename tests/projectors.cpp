//@+leo-ver=5-thin
//@+node:gcross.20110213233103.2811: * @thin projectors.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110213233103.2812: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <illuminate.hpp>

#include "projectors.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::assign::list_of;
//@-<< Includes >>

//@+others
//@+node:gcross.20110213233103.2813: ** Tests
TEST_SUITE(Projectors) {

//@+others
//@+node:gcross.20110213233103.2827: *3* projection makes overlap vanish
TEST_CASE(projection_makes_overlap_vanish) {

    RNG random;

    REPEAT(10) {

        unsigned int const
              physical_dimension = random
            , left_dimension = random
            , right_dimension = random
            , projector_length =
                 physical_dimension
                *left_dimension
                *right_dimension
            , number_of_projectors = random(1,projector_length-1)
            ;

        StateSite<Middle> const state_site(
             PhysicalDimension(physical_dimension)
            ,LeftDimension(left_dimension)
            ,RightDimension(right_dimension)
            ,fillWithGenerator(random.randomComplexDouble)
        );

        ProjectorMatrix const projector_matrix(
            randomProjectorMatrix(
                 projector_length
                ,number_of_projectors
            )
        );

        StateSite<Middle> const projected_state_site(
            applyProjectorMatrix(
                 projector_matrix
                ,state_site
            )
        );

        double const overlap =
            computeOverlapWithProjectors(
                 projector_matrix
                ,projected_state_site
            );

        ASSERT_EQ_QUOTED(
             physical_dimension
            ,projected_state_site.physicalDimension(as_unsigned_integer)
        );
        ASSERT_EQ_QUOTED(
             left_dimension
            ,projected_state_site.leftDimension(as_unsigned_integer)
        );
        ASSERT_EQ_QUOTED(
             right_dimension
            ,projected_state_site.rightDimension(as_unsigned_integer)
        );

        ASSERT_NEAR(0,overlap,1e-12);

    }

}
//@+node:gcross.20110213233103.2820: *3* formProjectorMatrix
TEST_SUITE(formProjectorMatrix) {

//@+others
//@+node:gcross.20110213233103.2821: *4* Test overlap sites
static OverlapSite<Middle> const
     test_overlap_site_1(
         RightDimension(1)
        ,LeftDimension(1)
        ,fillWithRange(list_of(1)(1)(1)(1))
     )
    ,test_overlap_site_2(
         RightDimension(1)
        ,LeftDimension(1)
        ,fillWithRange(list_of(1)(-1)(1)(1))
     )
    ,test_overlap_site_3(
         RightDimension(1)
        ,LeftDimension(1)
        ,fillWithRange(list_of(1)(-1)(-1)(1))
     )
    ;
//@+node:gcross.20110213233103.2822: *4* trivial
TEST_CASE(trivial) {
    ProjectorMatrix const projector_matrix(
        formProjectorMatrix(
            list_of(
                OverlapVectorTrio(
                     OverlapBoundary<Left>::trivial
                    ,OverlapBoundary<Right>::trivial
                    ,OverlapSite<Middle>::trivial
                )
            )
        )
    );
    ASSERT_EQ(1,projector_matrix.numberOfProjectors());
    ASSERT_EQ(1,projector_matrix.projectorLength());
    ASSERT_EQ(1,projector_matrix.numberOfReflectors());
    ASSERT_EQ(0,projector_matrix.orthogonalSubspaceDimension());
}
//@+node:gcross.20110213233103.2823: *4* physical dimension 4, one projector
TEST_CASE(physical_dimension_4_with_one_projector) {
    ProjectorMatrix const projector_matrix(
        formProjectorMatrix(list_of
            (OverlapVectorTrio(
                 OverlapBoundary<Left>::trivial
                ,OverlapBoundary<Right>::trivial
                ,test_overlap_site_1
            ))
        )
    );
    ASSERT_EQ(1,projector_matrix.numberOfProjectors());
    ASSERT_EQ(4,projector_matrix.projectorLength());
    ASSERT_EQ(1,projector_matrix.numberOfReflectors());
    ASSERT_EQ(3,projector_matrix.orthogonalSubspaceDimension());
}
//@+node:gcross.20110213233103.2824: *4* physical dimension 4, two projectors
TEST_CASE(physical_dimension_4_with_two_projectors) {
    ProjectorMatrix const projector_matrix(
        formProjectorMatrix(list_of
            (OverlapVectorTrio(
                 OverlapBoundary<Left>::trivial
                ,OverlapBoundary<Right>::trivial
                ,test_overlap_site_1
            ))
            (OverlapVectorTrio(
                 OverlapBoundary<Left>::trivial
                ,OverlapBoundary<Right>::trivial
                ,test_overlap_site_2
            ))
        )
    );
    ASSERT_EQ(2,projector_matrix.numberOfProjectors());
    ASSERT_EQ(4,projector_matrix.projectorLength());
    ASSERT_EQ(2,projector_matrix.numberOfReflectors());
    ASSERT_EQ(2,projector_matrix.orthogonalSubspaceDimension());
}
//@+node:gcross.20110213233103.2825: *4* physical dimension 4, three projectors
TEST_CASE(physical_dimension_4_with_three_projectors) {
    ProjectorMatrix const projector_matrix(
        formProjectorMatrix(list_of
            (OverlapVectorTrio(
                 OverlapBoundary<Left>::trivial
                ,OverlapBoundary<Right>::trivial
                ,test_overlap_site_1
            ))
            (OverlapVectorTrio(
                 OverlapBoundary<Left>::trivial
                ,OverlapBoundary<Right>::trivial
                ,test_overlap_site_2
            ))
            (OverlapVectorTrio(
                 OverlapBoundary<Left>::trivial
                ,OverlapBoundary<Right>::trivial
                ,test_overlap_site_3
            ))
        )
    );
    ASSERT_EQ(3,projector_matrix.numberOfProjectors());
    ASSERT_EQ(4,projector_matrix.projectorLength());
    ASSERT_EQ(3,projector_matrix.numberOfReflectors());
    ASSERT_EQ(1,projector_matrix.orthogonalSubspaceDimension());
}
//@-others

}
//@-others

}
//@-others
//@-leo
