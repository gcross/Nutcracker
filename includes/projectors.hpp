//@+leo-ver=5-thin
//@+node:gcross.20110213233103.2782: * @thin projectors.hpp
//@@language cplusplus

#ifndef NUTCRACKER_PROJECTORS_HPP
#define NUTCRACKER_PROJECTORS_HPP

//@+<< Includes >>
//@+node:gcross.20110213233103.2783: ** << Includes >>
#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110213233103.2784: ** << Usings >>
//@-<< Usings >>

//@+others
//@+node:gcross.20110214164734.1919: ** Tensors
//@+node:gcross.20110214164734.1921: *3* ProjectorMatrix
class ProjectorMatrix {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(ProjectorMatrix)
    unsigned int
          number_of_projectors
        , projector_length
        , number_of_reflectors
        , orthogonal_subspace_dimension
        ;
    complex<double> *reflector_data, *coefficient_data;
    uint32_t* swap_data;

public:
    ProjectorMatrix()
      : number_of_projectors(0)
      , projector_length(0)
      , number_of_reflectors(0)
      , orthogonal_subspace_dimension(0)
      , reflector_data(NULL)
      , coefficient_data(NULL)
      , swap_data(NULL)
    { }

    ~ProjectorMatrix() {
        if(valid()) {
            delete[] reflector_data;
            delete[] coefficient_data;
            delete[] swap_data;
        }
    }

    ProjectorMatrix(BOOST_RV_REF(ProjectorMatrix) other)
      : number_of_projectors(copyAndReset(other.number_of_projectors))
      , projector_length(copyAndReset(other.projector_length))
      , number_of_reflectors(copyAndReset(other.number_of_reflectors))
      , orthogonal_subspace_dimension(copyAndReset(other.orthogonal_subspace_dimension))
      , reflector_data(copyAndReset(other.reflector_data))
      , coefficient_data(copyAndReset(other.coefficient_data))
      , swap_data(copyAndReset(other.swap_data))
    { }

    ProjectorMatrix(
          unsigned int const number_of_projectors
        , unsigned int const projector_length
        , unsigned int const number_of_reflectors
        , unsigned int const orthogonal_subspace_dimension
        , complex<double>* reflector_data
        , complex<double>* coefficient_data
        , uint32_t* swap_data
    ) : number_of_projectors(number_of_projectors)
      , projector_length(projector_length)
      , number_of_reflectors(number_of_reflectors)
      , orthogonal_subspace_dimension(orthogonal_subspace_dimension)
      , reflector_data(reflector_data)
      , coefficient_data(coefficient_data)
      , swap_data(swap_data)
    { }

    ProjectorMatrix& operator=(BOOST_RV_REF(ProjectorMatrix) other) {
        if(this == &other) return *this;
        number_of_projectors = copyAndReset(other.number_of_projectors);
        projector_length = copyAndReset(other.projector_length);
        number_of_reflectors = copyAndReset(other.number_of_reflectors);
        orthogonal_subspace_dimension = copyAndReset(other.orthogonal_subspace_dimension);
        moveArrayToFrom(reflector_data,other.reflector_data);
        moveArrayToFrom(coefficient_data,other.coefficient_data);
        moveArrayToFrom(swap_data,other.swap_data);
        return *this;
    }

    void swap(ProjectorMatrix& other) {
        if(this == &other) return;
        std::swap(number_of_projectors,other.number_of_projectors);
        std::swap(projector_length,other.projector_length);
        std::swap(number_of_reflectors,other.number_of_reflectors);
        std::swap(orthogonal_subspace_dimension,other.orthogonal_subspace_dimension);
        std::swap(reflector_data,other.reflector_data);
        std::swap(coefficient_data,other.coefficient_data);
        std::swap(swap_data,other.swap_data);
    }

    unsigned int numberOfProjectors() const { return number_of_projectors; }
    unsigned int projectorLength() const { return projector_length; }
    unsigned int numberOfReflectors() const { return number_of_reflectors; }
    unsigned int orthogonalSubspaceDimension() const { return orthogonal_subspace_dimension; }

    complex<double>* reflectorData() { if(invalid()) throw InvalidTensorException(); return reflector_data; }
    complex<double> const* reflectorData() const { if(invalid()) throw InvalidTensorException(); return reflector_data; }

    complex<double>* coefficientData() { if(!*this) throw InvalidTensorException(); return coefficient_data; }
    complex<double> const* coefficientData() const { if(invalid()) throw InvalidTensorException(); return coefficient_data; }

    uint32_t* swapData() { if(invalid()) throw InvalidTensorException(); return swap_data; }
    uint32_t const* swapData() const { if(invalid()) throw InvalidTensorException(); return swap_data; }

    bool valid() const { return reflector_data && coefficient_data && swap_data; }
    bool invalid() const { return !valid(); }

    operator bool() const { return valid(); }

    unsigned int operator|(StateSite<Middle> const& state_site) const {
        return connectDimensions(
             "state site size"
            ,state_site.size()
            ,"projector length"
            ,projectorLength()
        );
    }
};
//@+node:gcross.20110213233103.2794: ** Classes
//@+node:gcross.20110213233103.2796: *3* OverlapVectorTrio
struct OverlapVectorTrio {
    OverlapBoundary<Left> const* left_boundary;
    OverlapBoundary<Right> const* right_boundary;
    OverlapSite<Middle> const* middle_site;

    OverlapVectorTrio(
          OverlapBoundary<Left> const& left_boundary
        , OverlapBoundary<Right> const& right_boundary
        , OverlapSite<Middle> const& middle_site
    ) : left_boundary(&left_boundary)
      , right_boundary(&right_boundary)
      , middle_site(&middle_site)
    {}
};
//@+node:gcross.20110213233103.2786: ** Functions
StateSite<Middle> applyProjectorMatrix(
      ProjectorMatrix const& projector_matrix
    , StateSite<Middle> const& old_state_site
);

double computeOverlapWithProjectors(
     ProjectorMatrix const& projector_matrix
    ,StateSite<Middle> const& state_site
);

ProjectorMatrix formProjectorMatrix(
    vector<OverlapVectorTrio> const& overlaps
);

ProjectorMatrix randomProjectorMatrix(
     unsigned int const vector_length
    ,unsigned int const number_of_projectors
);
//@-others

}

#endif
//@-leo
