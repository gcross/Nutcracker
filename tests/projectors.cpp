//@+leo-ver=5-thin
//@+node:gcross.20110213233103.2811: * @file projectors.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2054: ** << License >>
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
//@+node:gcross.20110213233103.2812: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/range/adaptor/indirected.hpp>
#include <complex>
#include <illuminate.hpp>

#include "projectors.hpp"

#include "test_utils.hpp"

using namespace Nutcracker;

using boost::adaptors::indirected;
using boost::assign::list_of;

using std::abs;
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
//@+node:gcross.20110213233103.2823: *4* physical dimension 4, one projector
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
//@+node:gcross.20110213233103.2824: *4* physical dimension 4, two projectors
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
//@+node:gcross.20110213233103.2825: *4* physical dimension 4, three projectors
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
//@-others

}
//@+node:gcross.20110217014932.1935: *3* computeStateOverlap
TEST_SUITE(computeStateOverlap) {

//@+others
//@+node:gcross.20110217014932.1937: *4* self-overlap is 1
TEST_CASE(self_overlap_is_1) {

    RNG random;

    REPEAT(10) {
        State state(random.randomState());
        ASSERT_NEAR_REL(
             c(1,0)
            ,computeStateOverlap(state,state)
            ,1e-15
        )
    }

}
//@+node:gcross.20110218150430.2598: *4* function is hermitian
TEST_CASE(function_is_hermitian) {

    RNG random;

    REPEAT(10) {
        vector<unsigned int> physical_dimensions(random.randomUnsignedIntegerVector(random(1,5)));
        State const state_1(random.randomState(physical_dimensions))
                  , state_2(random.randomState(physical_dimensions))
                  ;
        ASSERT_NEAR_REL(
             conj(computeStateOverlap(state_1,state_2))
            ,computeStateOverlap(state_2,state_1)
            ,1e-14
        )
    }

}
//@+node:gcross.20110218150430.2596: *4* single site orthogonal overlap is zero
TEST_SUITE(single_site_orthogonal_overlap_is_zero) {

    void runTest(unsigned int const physical_dimension) {
        vector<State> states;  states.reserve(physical_dimension);
        BOOST_FOREACH(unsigned int i, irange(0u,physical_dimension)) {
            vector<complex<double> > data(physical_dimension,c(0,0));
            data[i] = c(1,0);
            StateSite<Middle> first_state_site
                (LeftDimension(1)
                ,RightDimension(1)
                ,fillWithRange(data)
                );
            vector<StateSite<Right> > rest_state_sites;
            states.emplace_back(
                 boost::move(first_state_site)
                ,boost::move(rest_state_sites)
            );
        }
        BOOST_FOREACH(unsigned int i, irange(0u,physical_dimension)) {
            BOOST_FOREACH(unsigned int j, irange(i+1,physical_dimension)) {
                ASSERT_NEAR_REL(
                     c(0,0)
                    ,computeStateOverlap(states[i],states[j])
                    ,1e-15
                )
            }
        }
    }

    TEST_CASE(physical_dimension_2) { runTest(2); }
    TEST_CASE(physical_dimension_3) { runTest(3); }
    TEST_CASE(physical_dimension_4) { runTest(4); }

}
//@-others

}
//@+node:gcross.20110217175626.1924: *3* computeProjectorOverlap
TEST_SUITE(computeProjectorOverlap) {

//@+others
//@+node:gcross.20110217175626.1925: *4* self-overlap is 1
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
//@-others

}
//@+node:gcross.20110217175626.1927: *3* consistency
TEST_SUITE(consistency) {

//@+others
//@+node:gcross.20110217014932.1924: *4* overlaps
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
                ,1e-14
            )
        }
    }

}
//@+node:gcross.20110217175626.1929: *4* projector matrix
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
                contract<Right>::SS(
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
                ,1e-14
            );
            if(i < number_of_sites-1) {
                left_boundary =
                    Unsafe::contractSSLeft(
                         left_boundary
                        ,projector[i].get<Left>()
                        ,state[i]
                    );
            }
        }
    }

}
//@-others

}
//@+node:gcross.20110217175626.1933: *3* minimumBandwidthDimensionForProjectorCount
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
//@-others

}
//@-others
//@-leo
