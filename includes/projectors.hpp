//@+leo-ver=5-thin
//@+node:gcross.20110213233103.2782: * @thin projectors.hpp
//@@language cplusplus

#ifndef NUTCRACKER_PROJECTORS_HPP
#define NUTCRACKER_PROJECTORS_HPP

//@+<< Includes >>
//@+node:gcross.20110213233103.2783: ** << Includes >>
#include <boost/concept_check.hpp>
#include <boost/range/concepts.hpp>
#include <boost/tuple/tuple.hpp>
#include <utility>

#include "boundaries.hpp"
#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110213233103.2784: ** << Usings >>
using boost::make_tuple;
using boost::SinglePassRangeConcept;
using boost::tuple;

using std::make_pair;
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
//@+node:gcross.20110216193817.1913: *3* ProjectorSite
class ProjectorSite {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(ProjectorSite)

    OverlapSite<Left> left;
    OverlapSite<Middle> middle;
    OverlapSite<Right> right;
public:
    ProjectorSite() {}

    ProjectorSite(BOOST_RV_REF(ProjectorSite) other)
      : left(boost::move(left))
      , middle(boost::move(middle))
      , right(boost::move(right))
    { }

    ProjectorSite(
          BOOST_RV_REF(OverlapSite<Left>) left
        , BOOST_RV_REF(OverlapSite<Middle>) middle
        , BOOST_RV_REF(OverlapSite<Right>) right
    ) : left(left)
      , middle(middle)
      , right(right)
    { }

    void operator=(BOOST_RV_REF(ProjectorSite) other) {
        left = boost::move(other.left);
        middle = boost::move(other.middle);
        right = boost::move(other.right);
    }

    void swap(ProjectorSite& other) {
        left.swap(other.left);
        middle.swap(other.middle);
        right.swap(other.right);
    }

    template<typename side> OverlapSite<side> const& get() const {
        throw BadLabelException("OverlapSite::get",typeid(side));
    }
};
template<> inline OverlapSite<Left> const& ProjectorSite::get<Left>() const { return left; }
template<> inline OverlapSite<Middle> const& ProjectorSite::get<Middle>() const { return middle; }
template<> inline OverlapSite<Right> const& ProjectorSite::get<Right>() const { return right; }
//@+node:gcross.20110213233103.2794: ** Classes
//@+node:gcross.20110216193817.1917: *3* OverlapSitesFromStateSitesAndNormalizeResult
class OverlapSitesFromStateSitesAndNormalizeResult {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapSitesFromStateSitesAndNormalizeResult)
public:
    OverlapSite<Left> left_overlap_site_from_middle_state_site;
    OverlapSite<Middle> middle_overlap_site_from_middle_state_site;
    StateSite<Middle> middle_state_site_from_right_state_site;
    OverlapSite<Right> right_overlap_site_from_right_state_site;

    OverlapSitesFromStateSitesAndNormalizeResult(
          BOOST_RV_REF(OverlapSitesFromStateSitesAndNormalizeResult) other
    ) : left_overlap_site_from_middle_state_site(boost::move(other.left_overlap_site_from_middle_state_site))
      , middle_overlap_site_from_middle_state_site(boost::move(other.middle_overlap_site_from_middle_state_site))
      , middle_state_site_from_right_state_site(boost::move(other.middle_state_site_from_right_state_site))
      , right_overlap_site_from_right_state_site(boost::move(other.right_overlap_site_from_right_state_site))
    {}

    OverlapSitesFromStateSitesAndNormalizeResult(
          BOOST_RV_REF(OverlapSite<Left>) left_overlap_site_from_middle_state_site
        , BOOST_RV_REF(OverlapSite<Middle>) middle_overlap_site_from_middle_state_site
        , BOOST_RV_REF(StateSite<Middle>) middle_state_site_from_right_state_site
        , BOOST_RV_REF(OverlapSite<Right>) right_overlap_site_from_right_state_site
    ) : left_overlap_site_from_middle_state_site(left_overlap_site_from_middle_state_site)
      , middle_overlap_site_from_middle_state_site(middle_overlap_site_from_middle_state_site)
      , middle_state_site_from_right_state_site(middle_state_site_from_right_state_site)
      , right_overlap_site_from_right_state_site(right_overlap_site_from_right_state_site)
    {}
};
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

OverlapSite<Middle> computeOverlapSiteFromStateSite(StateSite<Middle> const& state_site);
OverlapSite<None> computeOverlapSiteFromStateSite(StateSiteAny const& state_site);

OverlapSitesFromStateSitesAndNormalizeResult computeOverlapSitesFromStateSitesAndNormalize(
      StateSite<Middle> const& middle_state_site
     ,StateSite<Right> const& right_state_site
);

double computeOverlapWithProjectors(
     ProjectorMatrix const& projector_matrix
    ,StateSite<Middle> const& state_site
);

Projector computeProjectorFromState(State const& state);

ProjectorMatrix formProjectorMatrix(
    vector<OverlapVectorTrio> const& overlaps
);

ProjectorMatrix randomProjectorMatrix(
     unsigned int const vector_length
    ,unsigned int const number_of_projectors
);


//@+node:gcross.20110216193817.1915: *3* computeProjectorFromStateSites
template<typename StateSiteRightRange>
Projector computeProjectorFromStateSites(
      StateSite<Middle> const& first_state_site
    , StateSiteRightRange const& rest_state_sites
) {
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRightRange>));
    Projector projector;
    StateSite<Middle> current_state_site(copyFrom(first_state_site));
    OverlapSite<Right> current_right_overlap_site;
    BOOST_FOREACH(
         StateSite<Right> const& next_state_site
        ,rest_state_sites
    ) {
        OverlapSitesFromStateSitesAndNormalizeResult result(
            computeOverlapSitesFromStateSitesAndNormalize(
                 current_state_site
                ,next_state_site
            )
        );
        projector.emplace_back(
             boost::move(result.left_overlap_site_from_middle_state_site)
            ,boost::move(result.middle_overlap_site_from_middle_state_site)
            ,boost::move(current_right_overlap_site)
        );
        current_state_site = boost::move(result.middle_state_site_from_right_state_site);
        current_right_overlap_site = boost::move(result.right_overlap_site_from_right_state_site);
    }
    OverlapSite<Left> current_left_overlap_site;
    OverlapSite<Middle> current_middle_overlap_site(computeOverlapSiteFromStateSite(current_state_site));
    projector.emplace_back(
         boost::move(current_left_overlap_site)
        ,boost::move(current_middle_overlap_site)
        ,boost::move(current_right_overlap_site)
    );
    return boost::move(projector);
}
//@+node:gcross.20110216193817.1918: *3* computeProjectorOverlap
template<typename StateSiteRange>
complex<double> computeProjectorOverlap(
      Projector const& projector
    , StateSiteRange const& state_sites
    , unsigned int const active_site_number=0
) {
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRange>));
    OverlapBoundary<Left> left_boundary(make_trivial);
    unsigned int i = 0;
    BOOST_FOREACH(
         StateSiteAny const& state_site
        ,state_sites
    ) {
        if(i < active_site_number) {
            left_boundary =
                Unsafe::contractSSLeft(
                     left_boundary
                    ,projector[i].get<Left>()
                    ,state_site
                );
        } else if(i == active_site_number) {
            left_boundary =
                Unsafe::contractSSLeft(
                     left_boundary
                    ,projector[i].get<Middle>()
                    ,state_site
                );
        } else {
            left_boundary =
                Unsafe::contractSSLeft(
                     left_boundary
                    ,projector[i].get<Right>()
                    ,state_site
                );
        }
        ++i;
    }
    assert(left_boundary.size() == 1);
    return left_boundary[0];
}
//@+node:gcross.20110217014932.1923: *3* computeStateOverlap
template<
     typename StateSiteRange1
    ,typename StateSiteRange2
> complex<double> computeStateOverlap(
      StateSiteRange1 const& state_sites_1
    , StateSiteRange2 const& state_sites_2
) {
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRange1>));
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRange2>));
    OverlapBoundary<Left> left_boundary(make_trivial);
    typedef tuple<
             StateSiteAny const&
            ,StateSiteAny const&
            > StateSiteAnyPair;
    BOOST_FOREACH(
         StateSiteAnyPair const state_site_pair
        ,make_pair(
             make_zip_iterator(make_tuple(state_sites_1.begin(),state_sites_2.begin()))
            ,make_zip_iterator(make_tuple(state_sites_1.end(),state_sites_2.end()))
         )
    ) {
        OverlapSite<None> overlap_site(computeOverlapSiteFromStateSite(state_site_pair.get<0>()));
        left_boundary =
            Unsafe::contractSSLeft(
                 left_boundary
                ,overlap_site
                ,state_site_pair.get<1>()
            );
    }
    assert(left_boundary.size() == 1);
    return left_boundary[0];
}
//@-others

}

#endif
//@-leo
