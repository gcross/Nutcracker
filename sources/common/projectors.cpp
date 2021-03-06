// Includes {{{
#include <boost/lambda/lambda.hpp>
#include <boost/numeric/ublas/vector.hpp>
#include <boost/range/algorithm/for_each.hpp>

#include "nutcracker/core.hpp"
#include "nutcracker/projectors.hpp"
// }}}

namespace Nutcracker {

// Usings {{{
namespace lambda = boost::lambda;

using boost::for_each;
// }}}

StateSite<Middle> applyProjectorMatrix( // {{{
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
} // }}}

template<typename side> static OverlapSite<side> implComputeOverlapSiteFromStateSite(StateSiteAny const& state_site) {{{
    OverlapSite<side> overlap_site(dimensionsOf(state_site));
    Core::form_overlap_site_tensor(
         state_site.rightDimension()
        ,state_site.leftDimension()
        ,state_site.physicalDimension()
        ,state_site
        ,overlap_site
    );
    return boost::move(overlap_site);
}}}

OverlapSite<Middle> computeOverlapSiteFromStateSite(StateSite<Middle> const& state_site) {{{
    return implComputeOverlapSiteFromStateSite<Middle>(state_site);
}}}

OverlapSite<None> computeOverlapSiteFromStateSite(StateSiteAny const& state_site) {{{
    return implComputeOverlapSiteFromStateSite<None>(state_site);
}}}

OverlapSitesFromStateSitesAndNormalizeResult computeOverlapSitesFromStateSitesAndNormalize( // {{{
      StateSite<Middle> const& middle_state_site
     ,StateSite<Right> const& right_state_site
) {
    middle_state_site.assertCanBeRightNormalized();
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
} // }}}

double computeOverlapWithProjectors( // {{{
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
} // }}}

Projector computeProjectorFromState(State const& state) {{{
    return computeProjectorFromStateSites(state.getFirstSite(),state.getRestSites());
}}}

ProjectorMatrix const& ProjectorMatrix::getNull() {{{
    static ProjectorMatrix const null_projector_matrix;
    return null_projector_matrix;
}}}

unsigned int minimumBandwidthDimensionForProjectorCount( // {{{
      vector<unsigned int> const& physical_dimensions
    , unsigned int const number_of_projectors
) {
    if(number_of_projectors == 0) return 1;
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
} // }}}

ProjectorMatrix randomProjectorMatrix( // {{{
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
} // }}}

}
