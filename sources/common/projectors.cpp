//@+leo-ver=5-thin
//@+node:gcross.20110213233103.2776: * @file projectors.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2036: ** << License >>
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
//@+node:gcross.20110213233103.2777: ** << Includes >>
#include <boost/lambda/lambda.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/range/algorithm/for_each.hpp>

#include "core.hpp"
#include "projectors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110213233103.2778: ** << Usings >>
namespace lambda = boost::lambda;

using boost::for_each;
//@-<< Usings >>

//@+others
//@+node:gcross.20110213233103.2806: ** Functions
//@+node:gcross.20110213233103.2807: *3* applyProjectorMatrix
StateSite<Middle> applyProjectorMatrix(
      ProjectorMatrix const& projector_matrix
    , StateSite<Middle> const& old_state_site
) {
    assert(projector_matrix.orthogonalSubspaceDimension() > 0);
    StateSite<Middle> new_state_site(dimensionsOf(old_state_site));
    Core::filter_components_outside_orthog(
         projector_matrix | old_state_site
        ,projector_matrix.numberOfProjectors()
        ,projector_matrix.numberOfReflectors()
        ,projector_matrix.orthogonalSubspaceDimension()
        ,projector_matrix.reflectorData()
        ,projector_matrix.coefficientData()
        ,projector_matrix.swapData()
        ,old_state_site
        ,new_state_site
    );
    assert(new_state_site.norm() > 1e-7);
    for_each(new_state_site,lambda::_1 /= new_state_site.norm());
    return boost::move(new_state_site);
}
//@+node:gcross.20110126102637.2188: *3* computeOverlapSiteFromStateSite
template<typename side>
static OverlapSite<side> implComputeOverlapSiteFromStateSite(StateSiteAny const& state_site) {
    OverlapSite<side> overlap_site(dimensionsOf(state_site));
    Core::form_overlap_site_tensor(
         state_site.rightDimension()
        ,state_site.leftDimension()
        ,state_site.physicalDimension()
        ,state_site
        ,overlap_site
    );
    return boost::move(overlap_site);
}

OverlapSite<Middle> computeOverlapSiteFromStateSite(StateSite<Middle> const& state_site) {
    return implComputeOverlapSiteFromStateSite<Middle>(state_site);
}

OverlapSite<None> computeOverlapSiteFromStateSite(StateSiteAny const& state_site) {
    return implComputeOverlapSiteFromStateSite<None>(state_site);
}
//@+node:gcross.20110126102637.2189: *3* computeOverlapSitesFromStateSitesAndNormalize
OverlapSitesFromStateSitesAndNormalizeResult computeOverlapSitesFromStateSitesAndNormalize(
      StateSite<Middle> const& middle_state_site
     ,StateSite<Right> const& right_state_site
) {
    OverlapSite<Left> left_overlap_site_from_middle_state_site(dimensionsOf(middle_state_site));
    OverlapSite<Middle> middle_overlap_site_from_middle_state_site(dimensionsOf(middle_state_site));
    StateSite<Middle> middle_state_site_from_right_state_site(dimensionsOf(right_state_site));
    OverlapSite<Right> right_overlap_site_from_right_state_site(dimensionsOf(right_state_site));
    Core::form_norm_overlap_tensors(
         middle_state_site.leftDimension()
        ,middle_state_site | right_state_site
        ,right_state_site.rightDimension()
        ,middle_state_site.physicalDimension()
        ,right_state_site.physicalDimension()
        ,middle_state_site
        ,right_state_site
        ,left_overlap_site_from_middle_state_site
        ,middle_overlap_site_from_middle_state_site
        ,middle_state_site_from_right_state_site
        ,right_overlap_site_from_right_state_site
    );
    return
        OverlapSitesFromStateSitesAndNormalizeResult(
             boost::move(left_overlap_site_from_middle_state_site)
            ,boost::move(middle_overlap_site_from_middle_state_site)
            ,boost::move(middle_state_site_from_right_state_site)
            ,boost::move(right_overlap_site_from_right_state_site)
        );
}
//@+node:gcross.20110213233103.2808: *3* computeOverlapWithProjectors
double computeOverlapWithProjectors(
     ProjectorMatrix const& projector_matrix
    ,StateSiteAny const& state_site
) {
    if(projector_matrix.invalid()) return 0;
    else return
    abs(Core::compute_overlap_with_projectors(
         projector_matrix.numberOfProjectors()
        ,projector_matrix.numberOfReflectors()
        ,projector_matrix.reflectorData()
        ,projector_matrix.coefficientData()
        ,projector_matrix.swapData()
        ,projector_matrix | state_site
        ,state_site
    ));
}
//@+node:gcross.20110217014932.1932: *3* computeProjectorFromState
Projector computeProjectorFromState(State const& state) {
    return computeProjectorFromStateSites(state.getFirstSite(),state.getRestSites());
}
//@+node:gcross.20110217175626.1932: *3* minimumBandwidthDimensionForProjectorCount
unsigned int minimumBandwidthDimensionForProjectorCount(
      vector<unsigned int> const& physical_dimensions
    , unsigned int const number_of_projectors
) {
    for(unsigned int bandwidth_dimension = 1; true; ++bandwidth_dimension) {
        vector<unsigned int> const bandwidth_dimension_sequence =
            computeBandwidthDimensionSequence(
                 bandwidth_dimension
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
                return bandwidth_dimension;
            }
        }
    }
}
//@+node:gcross.20110213233103.2810: *3* randomProjectorMatrix
ProjectorMatrix randomProjectorMatrix(
     unsigned int const vector_length
    ,unsigned int const number_of_projectors
) {
    unsigned int const number_of_reflectors = min(number_of_projectors,vector_length);
    complex<double>* reflectors
        = new complex<double>[number_of_projectors*vector_length];
    complex<double>* coefficients
        = new complex<double>[number_of_reflectors];
    uint32_t* swaps
        = new uint32_t[number_of_reflectors];
    unsigned const int subspace_dimension =
    Core::random_projector_matrix(
         vector_length
        ,number_of_projectors
        ,reflectors
        ,coefficients
        ,swaps
    );
    return ProjectorMatrix(
             number_of_projectors
            ,vector_length
            ,number_of_reflectors
            ,vector_length-subspace_dimension
            ,reflectors
            ,coefficients
            ,swaps
    );
}
//@-others

}
//@-leo
