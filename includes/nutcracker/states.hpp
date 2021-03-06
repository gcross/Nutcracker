#ifndef NUTCRACKER_STATES_HPP
#define NUTCRACKER_STATES_HPP

// Includes {{{
#include <boost/concept_check.hpp>
#include <boost/container/vector.hpp>
#include <boost/iterator/zip_iterator.hpp>
#include <boost/range/algorithm/reverse.hpp>
#include <boost/range/concepts.hpp>
#include <boost/tuple/tuple.hpp>

#include "nutcracker/boundaries.hpp"
#include "nutcracker/core.hpp"
#include "nutcracker/flat.hpp"
#include "nutcracker/tensors.hpp"
// }}}

namespace Nutcracker {

// Usings {{{
using boost::container::vector;
using boost::make_tuple;
using boost::make_zip_iterator;
using boost::SinglePassRangeConcept;
using boost::tuple;

using std::make_pair;
// }}}

typedef Link<VectorConstPtr> StateSiteLink;

struct NormalizationError : public std::runtime_error { // {{{
    int const info;
    NormalizationError(int info);
}; // }}}

// Result classes // {{{
template<typename side1,typename side2> class IncreaseDimensionBetweenResult { // {{{
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(IncreaseDimensionBetweenResult)
public:
    StateSite<side1> state_site_1;
    StateSite<side2> state_site_2;

    IncreaseDimensionBetweenResult(BOOST_RV_REF(IncreaseDimensionBetweenResult) other)
      : state_site_1(boost::move(other.state_site_1))
      , state_site_2(boost::move(other.state_site_2))
    {}

    IncreaseDimensionBetweenResult(
          BOOST_RV_REF(StateSite<side1>) state_site_1
        , BOOST_RV_REF(StateSite<side2>) state_site_2
    ) : state_site_1(state_site_1)
      , state_site_2(state_site_2)
    {}
}; // }}}
template<typename side> class MoveSiteCursorResult { // {{{
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(MoveSiteCursorResult)
    typedef typename Other<side>::value other_side;
public:
    StateSite<Middle> middle_state_site;
    StateSite<other_side> other_side_state_site;

    MoveSiteCursorResult(BOOST_RV_REF(MoveSiteCursorResult) other)
      : middle_state_site(boost::move(other.middle_state_site))
      , other_side_state_site(boost::move(other.other_side_state_site))
    {}

    MoveSiteCursorResult(
          BOOST_RV_REF(StateSite<Middle>) middle_state_site
        , BOOST_RV_REF(StateSite<other_side>) other_side_state_site
    ) : middle_state_site(middle_state_site)
      , other_side_state_site(other_side_state_site)
    {}
}; // }}}
// }}}

// class State {{{
//! Matrix product state in canonical form.
/*!
Specifically, this class contains an immutable matrix product state such that the first site is unnormalized and all sites after it are right-normalized.

\note This class is moveable but not copyable, and uses Boost.Move to implement these semantics.
*/
class State : boost::noncopyable {
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(State)
    public:

    //! Moves the matrix product state contained in \c other to \c this and invalidates \c other.
    void operator=(BOOST_RV_REF(State) other) {
        first_site = boost::move(other.first_site);
        rest_sites = boost::move(other.rest_sites);
    }

    //! Swaps the matrix product state contained in \c other and \c this.
    void swap(State& other) {
        first_site.swap(other.first_site);
        rest_sites.swap(other.rest_sites);
    }
    //! @name Constructors

    public:

    //! Construct an invalid matrix product state (presumably into which you will eventually move data from elsewhere).
    State() {}

    //! Moves the matrix product state contained in \c other into \c this.
    State(BOOST_RV_REF(State) other)
      : first_site(boost::move(other.first_site))
      , rest_sites(boost::move(other.rest_sites))
    {}

    //! Constructs \c this with the given sites.
    /*!
    \param first_site the left-most site in the matrix product state
    \param rest_sites all the sites to the right of the left-most site in the matrix product state
    */
    State(
          BOOST_RV_REF(StateSite<Middle>) first_site
        , BOOST_RV_REF(vector<StateSite<Right> >) rest_sites
    ) : first_site(first_site)
      , rest_sites(rest_sites)
    { }

    template<typename StateSiteRange> State(CopyFrom<StateSiteRange> sites);
    protected:

    //! The left-most site in the matrix product state.
    StateSite<Middle> first_site;

    //! Vector of sites excluding the left-most site in the matrix product state.
    vector<StateSite<Right> > rest_sites;
    /*! @name Flattening
    Convenience functions for working with the flat vector representation of this state.
    */

    //! @{

    public:

    //! Computes the flat vector representation of this state.
    /*!
    \note Since this function necesarily has exponential running time, if you only need a few components of this vector then you should consider calling computeComponent().
    */
    Vector computeVector() {
        return computeStateVector(*this);
    }

    //! Computes the value of a single component of this state given the list of observed qudit values.
    /*!
    \note If you need the entire state vector then it is more efficient to call computeStateVector().

    \param observed_values the observation value of each qudit in the matrix product state, which together specify the component of the quantum state whose value is to be computed
    \returns the amplitude of the requested component
    */
    complex<double> computeVectorComponent(vector<unsigned int> const& observed_values) {
        return computeStateVectorComponent(*this,observed_values);
    }

    //!  Computes the value of a single component of this state given the index of the component in the flat representation.
    /*!
    \note If you need the entire state vector then it is more efficient to call computeStateVector().

    \param component the index of the desired component (in the flat vector representation of the state)
    \returns the amplitude of the requested component
    */
    complex<double> computeVectorComponent(unsigned long long const component) {
        return computeStateVectorComponent(*this,component);
    }

    //! Computes the number of components of the flat vector representation of this state.
    /*!
    \returns the number of components in the flat vector representation of the state
    */
    unsigned long long computeVectorLength() {
        return computeStateVectorLength(*this);
    }

    //! @}
    //! @name Informational

    //! @{

    public:

    //! Returns the number of sites in this matrix product state;
    unsigned int numberOfSites() const { return 1+rest_sites.size(); }

    //! @}
    /*! @name Iteration support
    The State class supports iterating over the sites.  Since the sites have different normalizations, the iteration value type is the base class StateSiteAny.
    */

    //! @{

    public:

    // const_iterator {{{
    //! Support class used for iteration over the sites in State.
    /*!
    \note The value type is StateSiteAny since the left-most site has a different normalization from the rest of the sites.
    */
    class const_iterator :
        public iterator_facade<
             const_iterator
            ,StateSiteAny const
            ,random_access_traversal_tag
        >
    {
        //! Pointer to the state class.
        State const* state;
        //! Current location in the matrix product state.
        unsigned int index;
    public:
        const_iterator() {}

        //! Construct a new iterator using the given state and index.
        const_iterator(
              State const* state
            , unsigned int const index
        ) : state(state)
          , index(index)
        {}

        //! Access the site at the current index.
        StateSiteAny const& dereference() const {
            return (*state)[index];
        }

        //! Chech whether two iterators are equal.
        bool equal(const_iterator const& other) const {
            return (state == other.state) && (index == other.index);
        }

        //! Increment the iterator.
        void increment() { ++index; }
        //! Decrement the iterator.
        void decrement() { --index; }

        //! Advance the iterator by \c n sites.
        void advance(size_t n) { index += n; }

        //! Compute the distance in indices between \c other and \c this.
        size_t distance_to(const_iterator const& other) const { return other.index - index; }
    }; // }}}

    typedef const_iterator iterator;

    //! Returns an iterator at the first site in the matrix product state.
    const_iterator begin() const { return const_iterator(this,0); }

    //! Returns an iterator just past the last site in the matrix product state.
    const_iterator end() const { return const_iterator(this,numberOfSites()); }

    //! @}
    //! @name Site access

    //! @{

    public:

    //! Returns the left-most site.
    StateSite<Middle> const& getFirstSite() const { return first_site; }

    //! Returns the vector of sites after the left-most site.
    vector<StateSite<Right> > const& getRestSites() const { return rest_sites; }

    //! Returns the site at index \c i \a after the left-most site.
    StateSite<Right> const& getRestSite(unsigned int i) const { return rest_sites[i]; }

    //! Returns the site at index \c i.
    /*!
    \note The return type is StateSiteAny since the left-most site has a different normalization from the rest of the sites.
    */
    StateSiteAny const& operator[](unsigned int i) const {
        if(i == 0) return first_site;
        else return rest_sites[i-1];
    }

    //! @}
}; // }}}

// Functions {{{

StateSite<None> constructStateSite( // {{{
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
    , vector<StateSiteLink> const& links
); // }}}

IncreaseDimensionBetweenResult<Right,Right> increaseDimensionBetweenRightRight( // {{{
      unsigned int new_dimension
    , StateSite<Right> const& old_site_1
    , StateSite<Right> const& old_site_2
); // }}}

IncreaseDimensionBetweenResult<Middle,Right> increaseDimensionBetweenMiddleRight( // {{{
      unsigned int new_dimension
    , StateSite<Middle> const& old_site_1
    , StateSite<Right> const& old_site_2
); // }}}

MoveSiteCursorResult<Left> moveSiteCursorLeft( // {{{
      StateSite<Middle> const& old_state_site_2
    , StateSite<Left> const& old_state_site_1
); // }}}

MoveSiteCursorResult<Right> moveSiteCursorRight( // {{{
      StateSite<Middle> const& old_state_site_1
    , StateSite<Right> const& old_state_site_2
); // }}}

StateSite<Middle> randomStateSiteMiddle( // {{{
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
); // }}}

StateSite<Right> randomStateSiteRight( // {{{
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
); // }}}

template<typename StateSiteRange> complex<double> computeExpectationValue( // {{{
      StateSiteRange const& state_sites
    , Operator const& operator_sites
) {
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRange const>));
    ExpectationBoundary<Left> left_boundary(make_trivial);
    unsigned int i = 0;
    BOOST_FOREACH(StateSiteAny const& state_site, state_sites) {
        left_boundary =
            Unsafe::contractSOSLeft(
                 left_boundary
                ,state_site
                ,*operator_sites[i++]
            );
    }
    assert(i == operator_sites.size() && "the number of state sites is greater than the number of operator sites");
    assert(left_boundary.size() == 1);
    return left_boundary[0];
} // }}}

// computeStateOverlap {{{
OverlapSite<None> computeOverlapSiteFromStateSite(StateSiteAny const& state_site);
template<
     typename StateSiteRange1
    ,typename StateSiteRange2
> complex<double> computeStateOverlap(
      StateSiteRange1 const& state_sites_1
    , StateSiteRange2 const& state_sites_2
) {
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRange1 const>));
    BOOST_CONCEPT_ASSERT((SinglePassRangeConcept<StateSiteRange2 const>));
    OverlapBoundary<Left> left_boundary(make_trivial);
    typename boost::range_iterator<StateSiteRange1 const>::type
        state_site_1 = boost::begin(state_sites_1),
        end_of_state_sites_1 = boost::end(state_sites_1);
    typename boost::range_iterator<StateSiteRange2 const>::type
        state_site_2 = boost::begin(state_sites_2),
        end_of_state_sites_2 = boost::end(state_sites_2);
    for(;
        state_site_1 != end_of_state_sites_1 &&
        state_site_2 != end_of_state_sites_2;
        ++state_site_1, ++state_site_2
    ) {
        // Note:  Don't merge these two lines;  it makes some compilers unhappy.
        left_boundary =
            Unsafe::contractVSLeft(
                 left_boundary
                ,computeOverlapSiteFromStateSite(*state_site_1)
                ,*state_site_2
            );
    }
    assert(left_boundary.size() == 1);
    return left_boundary[0];
} // }}}

namespace Unsafe { // {{{

// increaseDimensionBetween {{{
template<typename side1,typename side2>
IncreaseDimensionBetweenResult<side1,side2> increaseDimensionBetween(
      unsigned int const new_dimension
    , StateSiteAny const& old_site_1
    , StateSiteAny const& old_site_2
) {
    unsigned int const old_dimension = connectDimensions(
         "state site 1 right"
        ,old_site_1.rightDimension()
        ,"state site 2 left"
        ,old_site_2.leftDimension()
    );
    assert(new_dimension >= old_dimension);

    StateSite<side1> new_site_1(
         old_site_1.physicalDimension(as_dimension)
        ,old_site_1.leftDimension(as_dimension)
        ,RightDimension(new_dimension)
    );
    StateSite<side2> new_site_2(
         old_site_2.physicalDimension(as_dimension)
        ,LeftDimension(new_dimension)
        ,old_site_2.rightDimension(as_dimension)
    );

    int const info =
        new_dimension > old_dimension
            ? Core::increase_bandwidth_between(
                 old_site_1.leftDimension()
                ,old_dimension
                ,old_site_2.rightDimension()
                ,old_site_1.physicalDimension()
                ,old_site_2.physicalDimension()
                ,new_dimension
                ,old_site_1
                ,old_site_2
                ,new_site_1
                ,new_site_2
              )
            : Core::norm_denorm_going_left(
                 old_site_1.leftDimension()
                ,old_dimension
                ,old_site_2.rightDimension()
                ,old_site_1.physicalDimension()
                ,old_site_2.physicalDimension()
                ,old_site_1
                ,old_site_2
                ,new_site_1
                ,new_site_2
              )
    ;
    if(info != 0) throw NormalizationError(info);
    return IncreaseDimensionBetweenResult<side1,side2>
        (boost::move(new_site_1)
        ,boost::move(new_site_2)
        );
} // }}}

MoveSiteCursorResult<Left> moveSiteCursorLeft( // {{{
      StateSiteAny const& left_state_site
    , StateSiteAny const& right_state_site
); // }}}

MoveSiteCursorResult<Right> moveSiteCursorRight( // {{{
      StateSiteAny const& left_state_site
    , StateSiteAny const& right_state_site
); // }}}

} // }}}

// moveSiteCursor {{{
template<typename side> struct moveSiteCursor { };
template<> struct moveSiteCursor<Left> { // {{{
    static MoveSiteCursorResult<Left> from(
          StateSite<Middle> const& old_middle_state_site
	   , StateSite<Left> const& old_left_state_site
    ) { return moveSiteCursorLeft(old_middle_state_site,old_left_state_site); }
}; // }}}
template<> struct moveSiteCursor<Right> { // {{{
    static MoveSiteCursorResult<Right> from(
          StateSite<Middle> const& old_middle_state_site
	   , StateSite<Right> const& old_right_state_site
    ) { return moveSiteCursorRight(old_middle_state_site,old_right_state_site); }
}; // }}}
// }}}

template<typename StateSiteRange> State::State(CopyFrom<StateSiteRange> sites) { // {{{
    BOOST_CONCEPT_ASSERT((boost::BidirectionalRangeConcept<StateSiteRange>));
    typedef typename boost::range_reverse_iterator<StateSiteRange const>::type iterator;
    iterator next_site_iter = boost::rbegin(*sites);
    iterator const end_of_sites = boost::rend(*sites);
    StateSite<Middle> current_site = next_site_iter->normalize();
    for(++next_site_iter; next_site_iter != end_of_sites; ++next_site_iter) {
        MoveSiteCursorResult<Left> result = Unsafe::moveSiteCursorLeft(*next_site_iter,current_site);
        rest_sites.emplace_back(boost::move(result.other_side_state_site));
        current_site = boost::move(result.middle_state_site);
    }
    BOOST_FOREACH(unsigned int i, irange((long unsigned)0u,rest_sites.size()/2)) {
        rest_sites[i].swap(rest_sites[rest_sites.size()-i-1]);
    }
    first_site = boost::move(current_site);
} // }}}

// }}}

}

namespace boost {
    template<> struct range_iterator<Nutcracker::State const> { typedef Nutcracker::State::const_iterator type; };
}

#endif
