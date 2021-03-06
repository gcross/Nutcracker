#include <boost/assign/list_of.hpp>
#include <boost/range/adaptor/indirected.hpp>
#include <complex>
#include <illuminate.hpp>

#include "nutcracker/projectors.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::adaptors::indirected;
using boost::assign::list_of;

using std::abs;

TEST_SUITE(Projectors) {

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

        ASSERT_EQ(
             physical_dimension
            ,projected_state_site.physicalDimension()
        );
        ASSERT_EQ(
             left_dimension
            ,projected_state_site.leftDimension()
        );
        ASSERT_EQ(
             right_dimension
            ,projected_state_site.rightDimension()
        );

        ASSERT_NEAR_REL(0,overlap,1e-12);

    }

}
TEST_SUITE(formProjectorMatrix) {

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
TEST_CASE(trivial) {
    ProjectorMatrix const projector_matrix(
        formProjectorMatrix(
             list_of(&OverlapBoundary<Left>::trivial) | indirected
            ,list_of(&OverlapBoundary<Right>::trivial) | indirected
            ,list_of(&OverlapSite<Middle>::trivial) | indirected
        )
    );
    ASSERT_EQ(1u,projector_matrix.numberOfProjectors());
    ASSERT_EQ(1u,projector_matrix.projectorLength());
    ASSERT_EQ(1u,projector_matrix.numberOfReflectors());
    ASSERT_EQ(0u,projector_matrix.orthogonalSubspaceDimension());
}
TEST_CASE(physical_dimension_4_with_one_projector) {
    ProjectorMatrix const projector_matrix(
        formProjectorMatrix(
             list_of(&OverlapBoundary<Left>::trivial) | indirected
            ,list_of(&OverlapBoundary<Right>::trivial) | indirected
            ,list_of(&test_overlap_site_1) | indirected
        )
    );
    ASSERT_EQ(1u,projector_matrix.numberOfProjectors());
    ASSERT_EQ(4u,projector_matrix.projectorLength());
    ASSERT_EQ(1u,projector_matrix.numberOfReflectors());
    ASSERT_EQ(3u,projector_matrix.orthogonalSubspaceDimension());
}
TEST_CASE(physical_dimension_4_with_two_projectors) {
    ProjectorMatrix const projector_matrix(
        formProjectorMatrix(
             list_of(&OverlapBoundary<Left>::trivial)(&OverlapBoundary<Left>::trivial) | indirected
            ,list_of(&OverlapBoundary<Right>::trivial)(&OverlapBoundary<Right>::trivial) | indirected
            ,list_of(&test_overlap_site_1)(&test_overlap_site_2) | indirected
        )
    );
    ASSERT_EQ(2u,projector_matrix.numberOfProjectors());
    ASSERT_EQ(4u,projector_matrix.projectorLength());
    ASSERT_EQ(2u,projector_matrix.numberOfReflectors());
    ASSERT_EQ(2u,projector_matrix.orthogonalSubspaceDimension());
}
TEST_CASE(physical_dimension_4_with_three_projectors) {
    ProjectorMatrix const projector_matrix(
        formProjectorMatrix(
             list_of(&OverlapBoundary<Left>::trivial)(&OverlapBoundary<Left>::trivial)(&OverlapBoundary<Left>::trivial) | indirected
            ,list_of(&OverlapBoundary<Right>::trivial)(&OverlapBoundary<Right>::trivial)(&OverlapBoundary<Right>::trivial) | indirected
            ,list_of(&test_overlap_site_1)(&test_overlap_site_2)(&test_overlap_site_3) | indirected
        )
    );
    ASSERT_EQ(3u,projector_matrix.numberOfProjectors());
    ASSERT_EQ(4u,projector_matrix.projectorLength());
    ASSERT_EQ(3u,projector_matrix.numberOfReflectors());
    ASSERT_EQ(1u,projector_matrix.orthogonalSubspaceDimension());
}

}
TEST_SUITE(computeProjectorOverlap) {

TEST_CASE(self_overlap_is_1) {

    RNG random;

    REPEAT(10) {
        State state(random.randomState());
        ASSERT_NEAR_REL(
             c(1,0)
            ,computeProjectorOverlap(computeProjectorFromState(state),state)
            ,1e-14
        )
    }

}

}
TEST_SUITE(consistency) {

TEST_CASE(overlaps) {

    RNG random;

    REPEAT(10) {
        vector<unsigned int> physical_dimensions(random.randomUnsignedIntegerVector(random(1,5)));
        State const state_1(random.randomState(physical_dimensions))
                  , state_2(random.randomState(physical_dimensions))
                  ;
        complex<double> const state_overlap = computeStateOverlap(state_1,state_2);
        Projector projector = computeProjectorFromState(state_1);
        BOOST_FOREACH(unsigned int const active_site_number, irange(0u,(unsigned int)physical_dimensions.size())) {
            ASSERT_NEAR_REL(
                 state_overlap
                ,computeProjectorOverlap(projector,state_2,active_site_number)
                ,1e-12
            )
        }
    }

}
TEST_CASE(projector_matrix) {

    RNG random;

    REPEAT(10) {
        vector<unsigned int> physical_dimensions(random.randomUnsignedIntegerVector(random(1,5)));
        unsigned int const number_of_sites = physical_dimensions.size();
        Projector const projector(computeProjectorFromState(random.randomState(physical_dimensions)));
        State const state(random.randomState(physical_dimensions));
        complex<double> const overlap = computeProjectorOverlap(projector,state);
        vector<OverlapBoundary<Right> > right_boundaries;
        right_boundaries.emplace_back(make_trivial);
        BOOST_FOREACH(unsigned int const i, irange(1u,number_of_sites) | reversed) {
            right_boundaries.push_back(
                contract<Right>::VS(
                     right_boundaries.back()
                    ,projector[i].get<Right>()
                    ,state.getRestSite(i-1)
                )
            );
        }
        OverlapBoundary<Left> left_boundary(make_trivial);
        BOOST_FOREACH(unsigned int const i, irange(0u,number_of_sites)) {
            OverlapBoundary<Right> right_boundary(boost::move(right_boundaries.back()));
            right_boundaries.pop_back();
            ProjectorMatrix projector_matrix(
                formProjectorMatrix(
                     list_of(&left_boundary) | indirected
                    ,list_of(&right_boundary) | indirected
                    ,list_of(&projector[i].get<Middle>()) | indirected
                )
            );
            ASSERT_NEAR_REL(
                 abs(overlap)
                ,computeOverlapWithProjectors(projector_matrix,state[i])
                ,1e-12
            );
            if(i < number_of_sites-1) {
                left_boundary =
                    Unsafe::contractVSLeft(
                         left_boundary
                        ,projector[i].get<Left>()
                        ,state[i]
                    );
            }
        }
    }

}

}
TEST_CASE(minimumBandwidthDimensionForProjectorCount) {
    RNG random;

    REPEAT(10) {
        vector<unsigned int> const physical_dimensions = random.randomUnsignedIntegerVector(random(2,10),2,10);
        unsigned int const
             number_of_projectors = random(2,2*physical_dimensions.size()-1)
            ,minimum_bandwidth_dimension =
                minimumBandwidthDimensionForProjectorCount(
                     physical_dimensions
                    ,number_of_projectors
                )
            ;
        {
            vector<unsigned int> const bandwidth_dimension_sequence =
                computeBandwidthDimensionSequence(
                     minimum_bandwidth_dimension
                    ,physical_dimensions
                );
            for(vector<unsigned int>::const_iterator
                      physical_dimension_iterator = physical_dimensions.begin()
                    , left_dimension_iterator = bandwidth_dimension_sequence.begin()
                    , right_dimension_iterator = bandwidth_dimension_sequence.begin()+1
               ;physical_dimension_iterator != physical_dimensions.end()
               ;++physical_dimension_iterator, ++left_dimension_iterator, ++right_dimension_iterator
            ) {
                if((*physical_dimension_iterator) * (*left_dimension_iterator) * (*right_dimension_iterator) > number_of_projectors) {
                    goto found;
                }
            }

            FATALLY_FAIL("minimum bandwidth dimension is too small");

            found: ;
        }
        {
            vector<unsigned int> const bandwidth_dimension_sequence =
                computeBandwidthDimensionSequence(
                     minimum_bandwidth_dimension-1
                    ,physical_dimensions
                );
            for(vector<unsigned int>::const_iterator
                      physical_dimension_iterator = physical_dimensions.begin()
                    , left_dimension_iterator = bandwidth_dimension_sequence.begin()
                    , right_dimension_iterator = bandwidth_dimension_sequence.begin()+1
               ;physical_dimension_iterator != physical_dimensions.end()
               ;++physical_dimension_iterator, ++left_dimension_iterator, ++right_dimension_iterator
            ) {
                if((*physical_dimension_iterator) * (*left_dimension_iterator) * (*right_dimension_iterator) > number_of_projectors) {
                    FATALLY_FAIL("minimum bandwidth dimension is too big");
                }
            }
        }
    }
}

}
