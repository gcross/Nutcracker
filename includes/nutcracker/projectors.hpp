#ifndef NUTCRACKER_PROJECTORS_HPP
#define NUTCRACKER_PROJECTORS_HPP

// Includes {{{
#include <boost/concept_check.hpp>
#include <boost/range/concepts.hpp>
#include <boost/tuple/tuple.hpp>
#include <utility>

#include "nutcracker/boundaries.hpp"
#include "nutcracker/core.hpp"
#include "nutcracker/states.hpp"
#include "nutcracker/tensors.hpp"
// }}}

namespace Nutcracker {

// Usings {{{
using boost::make_tuple;
using boost::RandomAccessRangeConcept;
using boost::SinglePassRangeConcept;
using boost::tuple;

using std::make_pair;
// }}}

class OverlapSitesFromStateSitesAndNormalizeResult { // {{{
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
}; // }}}

// struct Projector {{{
//! Vector of projector sites
/*!
This class subclasses boost::container::vector, so it implements move semantics but not copy semantics.
*/
class ProjectorSite;
struct Projector : vector<ProjectorSite> {
    private:

    typedef vector<ProjectorSite> Base;
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(Projector)
    public:

    //! Moves the contents of \c other into \c this.  (\c other will be empty after this.)
    Projector& operator=(BOOST_RV_REF(Projector) other) {
        if(this == &other) return *this;
        Base::operator=(boost::move(static_cast<Base&>(other)));
        return *this;
    }

    //! Swaps the contents of \c other and \c this.
    void swap(Projector& other) {
        Base::swap(other);
    }
    public:

    //! Construct an empty projector.
    Projector() {}

    //! Construct this projector by moving the contents of \c other into \c this.  (\c other will be empty after this.)
    Projector(BOOST_RV_REF(Projector) other)
      : Base(boost::move(static_cast<Base&>(other)))
    {}
}; // }}}

class ProjectorMatrix { // {{{
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
    ProjectorMatrix() // {{{
      : number_of_projectors(0)
      , projector_length(0)
      , number_of_reflectors(0)
      , orthogonal_subspace_dimension(0)
      , reflector_data(NULL)
      , coefficient_data(NULL)
      , swap_data(NULL)
    { } // }}}

    ~ProjectorMatrix() { // {{{
        if(valid()) {
            delete[] reflector_data;
            delete[] coefficient_data;
            delete[] swap_data;
        }
    } // }}}

    ProjectorMatrix(BOOST_RV_REF(ProjectorMatrix) other) // {{{
      : number_of_projectors(copyAndReset(other.number_of_projectors))
      , projector_length(copyAndReset(other.projector_length))
      , number_of_reflectors(copyAndReset(other.number_of_reflectors))
      , orthogonal_subspace_dimension(copyAndReset(other.orthogonal_subspace_dimension))
      , reflector_data(copyAndReset(other.reflector_data))
      , coefficient_data(copyAndReset(other.coefficient_data))
      , swap_data(copyAndReset(other.swap_data))
    { } // }}}

    ProjectorMatrix( // {{{
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
    { } // }}}

    ProjectorMatrix& operator=(BOOST_RV_REF(ProjectorMatrix) other) { // {{{
        if(this == &other) return *this;
        number_of_projectors = copyAndReset(other.number_of_projectors);
        projector_length = copyAndReset(other.projector_length);
        number_of_reflectors = copyAndReset(other.number_of_reflectors);
        orthogonal_subspace_dimension = copyAndReset(other.orthogonal_subspace_dimension);
        moveArrayToFrom(reflector_data,other.reflector_data);
        moveArrayToFrom(coefficient_data,other.coefficient_data);
        moveArrayToFrom(swap_data,other.swap_data);
        return *this;
    } // }}}

    void swap(ProjectorMatrix& other) { // {{{
        if(this == &other) return;
        std::swap(number_of_projectors,other.number_of_projectors);
        std::swap(projector_length,other.projector_length);
        std::swap(number_of_reflectors,other.number_of_reflectors);
        std::swap(orthogonal_subspace_dimension,other.orthogonal_subspace_dimension);
        std::swap(reflector_data,other.reflector_data);
        std::swap(coefficient_data,other.coefficient_data);
        std::swap(swap_data,other.swap_data);
    } // }}}

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

    unsigned int operator|(StateSiteAny const& state_site) const { // {{{
        return connectDimensions(
             "state site size"
            ,state_site.size()
            ,"projector length"
            ,projectorLength()
        );
    } // }}}

}; // }}}

// class ProjectorSite {{{
//! A projector site contains a left-, middle-, and right- normalized version of the same overlap site.
/*!
This class is meant to be used when one is sweeping left and right through a chain, so that it makes sense to compute all normalizations of the overlap sites at once rather than to recompute tham at each sweep.

\note This class is moveable but not copyable, and uses Boost.Move to implement these semantics.
*/
class ProjectorSite {
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(ProjectorSite)
    //! @name Assignment
    //! @{

    public:

    //! Moves the overlap site tensors from \c other to \c this and invalidates \c other.
    void operator=(BOOST_RV_REF(ProjectorSite) other) {
        left = boost::move(other.left);
        middle = boost::move(other.middle);
        right = boost::move(other.right);
    }

    //! Swaps the overlap site tensors between \c this and \c other.
    void swap(ProjectorSite& other) {
        left.swap(other.left);
        middle.swap(other.middle);
        right.swap(other.right);
    }

    //! @}
    //! @name Constructors

    //! @{

    public:

    //! Construct an invalid projector site (presumably into which you will eventually move data from elsewhere).
    ProjectorSite() {}

    //! Move the overlap sites from \c other into \c this and invalidate \c other.
    ProjectorSite(BOOST_RV_REF(ProjectorSite) other)
      : left(boost::move(other.left))
      , middle(boost::move(other.middle))
      , right(boost::move(other.right))
    { }

    //! Construct this projector site from the given overlap sites.
    ProjectorSite(
          BOOST_RV_REF(OverlapSite<Left>) left
        , BOOST_RV_REF(OverlapSite<Middle>) middle
        , BOOST_RV_REF(OverlapSite<Right>) right
    ) : left(left)
      , middle(middle)
      , right(right)
    { }

    //! @}
    private:

    OverlapSite<Left> left;
    OverlapSite<Middle> middle;
    OverlapSite<Right> right;
    //! @name Overlap site retrieval

    //! @{

    public:

    //! Retrives the overlap site tensor specified by the compile-time template (type tag) parameter \c side.
    /*!
    \tparam side a template argument specifying the normalization of the site tensor to fetch;  must be either \c Left, \c Middle, or \c Right, or \c BadLabelException is thrown
    \return Calling get<side>() returns the \c side overlap site tensor, i.e. get<Left>() returns the left overlap site tensor
    */
    template<typename side> OverlapSite<side> const& get() const {
        throw BadLabelException("OverlapSite::get",typeid(side));
    }
};

// External methods {{{
//! \cond
template<> inline OverlapSite<Left> const& ProjectorSite::get<Left>() const { return left; }
template<> inline OverlapSite<Middle> const& ProjectorSite::get<Middle>() const { return middle; }
template<> inline OverlapSite<Right> const& ProjectorSite::get<Right>() const { return right; }
//! \endcond
// }}}
// }}}

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
    ,StateSiteAny const& state_site
);

Projector computeProjectorFromState(State const& state);

unsigned int minimumBandwidthDimensionForProjectorCount(
      vector<unsigned int> const& physical_dimensions
    , unsigned int const number_of_projectors
);

ProjectorMatrix randomProjectorMatrix(
     unsigned int const vector_length
    ,unsigned int const number_of_projectors
);


template<typename StateSiteRightRange>
Projector computeProjectorFromStateSites(
      StateSite<Middle> const& first_state_site
    , StateSiteRightRange const& rest_state_sites
) {{{
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRightRange const>));
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
}}}

template<typename StateSiteRange>
complex<double> computeProjectorOverlap(
      Projector const& projector
    , StateSiteRange const& state_sites
    , unsigned int const active_site_number=0
) {{{
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRange const>));
    OverlapBoundary<Left> left_boundary(make_trivial);
    unsigned int i = 0;
    BOOST_FOREACH(
         StateSiteAny const& state_site
        ,state_sites
    ) {
        if(i < active_site_number) {
            left_boundary =
                Unsafe::contractVSLeft(
                     left_boundary
                    ,projector[i].get<Left>()
                    ,state_site
                );
        } else if(i == active_site_number) {
            left_boundary =
                Unsafe::contractVSLeft(
                     left_boundary
                    ,projector[i].get<Middle>()
                    ,state_site
                );
        } else {
            left_boundary =
                Unsafe::contractVSLeft(
                     left_boundary
                    ,projector[i].get<Right>()
                    ,state_site
                );
        }
        ++i;
    }
    assert(left_boundary.size() == 1);
    return left_boundary[0];
}}}

template<
     typename OverlapBoundaryLeftRange
    ,typename OverlapBoundaryRightRange
    ,typename OverlapSiteMiddleRange
> ProjectorMatrix formProjectorMatrix(
     OverlapBoundaryLeftRange const& left_boundaries
    ,OverlapBoundaryRightRange const& right_boundaries
    ,OverlapSiteMiddleRange const& overlap_sites
) {{{
    BOOST_CONCEPT_ASSERT((RandomAccessRangeConcept<OverlapBoundaryLeftRange const>));
    BOOST_CONCEPT_ASSERT((RandomAccessRangeConcept<OverlapBoundaryRightRange const>));
    BOOST_CONCEPT_ASSERT((RandomAccessRangeConcept<OverlapSiteMiddleRange const>));
    assert(left_boundaries.size() == right_boundaries.size());
    assert(left_boundaries.size() == (unsigned int)overlap_sites.size());
    unsigned int const number_of_projectors = left_boundaries.size();
    if(number_of_projectors == 0u) return ProjectorMatrix();
    unsigned int const
         state_physical_dimension = overlap_sites.begin()->physicalDimension()
        ,state_left_dimension = left_boundaries.begin()->stateDimension()
        ,state_right_dimension = right_boundaries.begin()->stateDimension()
        ,overlap_vector_length = state_physical_dimension*state_left_dimension*state_right_dimension
        ,number_of_reflectors = min(overlap_vector_length,number_of_projectors)
        ;
    complex<double>* overlap_vectors
        = new complex<double>[number_of_projectors*overlap_vector_length];
    complex<double>* overlap_vector = overlap_vectors;
    typedef tuple<
             OverlapBoundary<Left> const&
            ,OverlapBoundary<Right> const&
            ,OverlapSite<Middle> const&
            > OverlapVectorTriplet;
    BOOST_FOREACH(
         OverlapVectorTriplet const overlap_vector_triplet
        ,make_pair(
             make_zip_iterator(make_tuple(
                 left_boundaries.begin()
                ,right_boundaries.begin()
                ,overlap_sites.begin()
             ))
            ,make_zip_iterator(make_tuple(
                 left_boundaries.end()
                ,right_boundaries.end()
                ,overlap_sites.end()
             ))
         )
    ) {
        OverlapBoundary<Left> const& left_boundary = overlap_vector_triplet.get<0>();
        OverlapBoundary<Right> const& right_boundary = overlap_vector_triplet.get<1>();
        OverlapSite<Middle> const& overlap_site = overlap_vector_triplet.get<2>();
        assert(left_boundary.stateDimension() == state_left_dimension);
        assert(right_boundary.stateDimension() == state_right_dimension);
        Core::form_overlap_vector(
             left_boundary | overlap_site
            ,overlap_site  | right_boundary
            ,state_left_dimension
            ,state_right_dimension
            ,state_physical_dimension
            ,left_boundary
            ,right_boundary
            ,overlap_site
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
}}}

}

#endif
