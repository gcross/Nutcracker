//@+leo-ver=5-thin
//@+node:gcross.20110213233103.2776: * @thin projectors.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110213233103.2777: ** << Includes >>
#include "connectors.hpp"
#include "core.hpp"
#include "projectors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110213233103.2778: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110213233103.2806: ** Functions
//@+node:gcross.20110213233103.2807: *3* applyProjectorMatrix
StateSite<Middle> applyProjectorMatrix(
      ProjectorMatrix const& projector_matrix
    , StateSite<Middle> const& old_state_site
) {
    StateSite<Middle> new_state_site(dimensionsOf(old_state_site));
    Core::filter_components_outside_orthog(
         projector_matrix || old_state_site
        ,projector_matrix.numberOfProjectors()
        ,projector_matrix.numberOfReflectors()
        ,projector_matrix.orthogonalSubspaceDimension()
        ,projector_matrix.reflectorData()
        ,projector_matrix.coefficientData()
        ,projector_matrix.swapData()
        ,old_state_site
        ,new_state_site
    );
    return boost::move(new_state_site);
}
//@+node:gcross.20110213233103.2808: *3* computeOverlapWithProjectors
double computeOverlapWithProjectors(
     ProjectorMatrix const& projector_matrix
    ,StateSite<Middle> const& state_site
) {
    return abs(Core::compute_overlap_with_projectors(
         projector_matrix.numberOfProjectors()
        ,projector_matrix.numberOfReflectors()
        ,projector_matrix.reflectorData()
        ,projector_matrix.coefficientData()
        ,projector_matrix.swapData()
        ,projector_matrix || state_site
        ,state_site
    ));
}
//@+node:gcross.20110213233103.2809: *3* formProjectorMatrix
ProjectorMatrix formProjectorMatrix(
    vector<OverlapVectorTrio> const& overlaps
) {
    unsigned int const
         number_of_projectors = overlaps.size()
        ,state_physical_dimension = overlaps[0].middle_site->physicalDimension(as_unsigned_integer)
        ,state_left_dimension = overlaps[0].left_boundary->stateDimension(as_unsigned_integer)
        ,state_right_dimension = overlaps[0].right_boundary->stateDimension(as_unsigned_integer)
        ,overlap_vector_length = state_physical_dimension*state_left_dimension*state_right_dimension
        ,number_of_reflectors = min(overlap_vector_length,number_of_projectors)
        ;
    complex<double>* overlap_vectors
        = new complex<double>[number_of_projectors*overlap_vector_length];
    complex<double>* overlap_vector = overlap_vectors;
    BOOST_FOREACH(
         OverlapVectorTrio const& overlap
        ,overlaps
    ) {
        Core::form_overlap_vector(
             (*overlap.left_boundary) || (*overlap.middle_site)
            ,(*overlap.middle_site) || (*overlap.right_boundary)
            ,state_left_dimension
            ,state_right_dimension
            ,state_physical_dimension
            ,*overlap.left_boundary
            ,*overlap.right_boundary
            ,*overlap.middle_site
            ,overlap_vector
        );
        overlap_vector += overlap_vector_length;
    }
    complex<double>* coefficients = new complex<double>[number_of_reflectors];
    uint32_t* swaps = new uint32_t[number_of_reflectors];
    unsigned int const subspace_dimension =
    Core::convert_vectors_to_reflectors(
         overlap_vector_length
        ,number_of_projectors
        ,overlap_vectors
        ,coefficients
        ,swaps
    );
    return ProjectorMatrix(
             number_of_projectors
            ,overlap_vector_length
            ,number_of_reflectors
            ,overlap_vector_length-subspace_dimension
            ,overlap_vectors
            ,coefficients
            ,swaps
    );
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
