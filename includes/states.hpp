//@+leo-ver=5-thin
//@+node:gcross.20110213161858.1810: * @file states.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2018: ** << License >>
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

#ifndef NUTCRACKER_STATES_HPP
#define NUTCRACKER_STATES_HPP

//@+<< Includes >>
//@+node:gcross.20110213161858.1811: ** << Includes >>
#include <boost/concept_check.hpp>
#include <boost/container/vector.hpp>
#include <boost/iterator/zip_iterator.hpp>
#include <boost/range/concepts.hpp>
#include <boost/tuple/tuple.hpp>

#include "boundaries.hpp"
#include "core.hpp"
#include "flat.hpp"
#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110213161858.1812: ** << Usings >>
using boost::container::vector;
using boost::make_tuple;
using boost::make_zip_iterator;
using boost::SinglePassRangeConcept;
using boost::tuple;
//@-<< Usings >>

//@+others
//@+node:gcross.20110214155808.1966: ** Exceptions
//@+node:gcross.20110214155808.1967: *3* NormalizationError
struct NormalizationError : public Exception {
    int const info;
    NormalizationError(int info);
};
//@+node:gcross.20110214155808.1968: *3* NotEnoughDegreesOfFreedomToNormalizeError
struct NotEnoughDegreesOfFreedomToNormalizeError : public Exception {
    string n1, n2, n3;
    unsigned int d1, d2, d3;
    NotEnoughDegreesOfFreedomToNormalizeError(
         string const& n1
        ,unsigned int const d1
        ,string const& n2
        ,unsigned int const d2
        ,string const& n3
        ,unsigned int const d3
    ) : Exception((
            format("Not enough degrees of freedom to normalize (%1% (%2%) > %3% (%4%) * %5% (%6%))")
                % n1
                % d1
                % n2
                % d2
                % n3
                % d3
        ).str())
      , n1(n1)
      , n2(n2)
      , n3(n3)
      , d1(d1)
      , d2(d2)
      , d3(d3)
    { }
    virtual ~NotEnoughDegreesOfFreedomToNormalizeError() throw() {}
};
//@+node:gcross.20110213233103.2762: ** Classes
//@+node:gcross.20110213233103.2763: *3* IncreaseDimensionBetweenResult
template<typename side1,typename side2> class IncreaseDimensionBetweenResult {
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
};
//@+node:gcross.20110213233103.2764: *3* MoveSiteCursorResult
template<typename side> class MoveSiteCursorResult {
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
};
//@+node:gcross.20110430223656.2179: *3* State
//@+<< Description >>
//@+node:gcross.20110430223656.2180: *4* << Description >>
//! Matrix product state in canonical form.
/*!
Specifically, this class contains an immutable matrix product state such that the first site is unnormalized and all sites after it are right-normalized.

\note This class is moveable but not copyable, and uses Boost.Move to implement these semantics.
*/
//@-<< Description >>
struct State {
    //@+others
    //@+node:gcross.20110430223656.2181: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(State)
    //@+node:gcross.20110430223656.2182: *4* Assignment
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
    //@+node:gcross.20110430223656.2183: *4* Constructors
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
    //@+node:gcross.20110430223656.2184: *4* Fields
    protected:

    //! The left-most site in the matrix product state.
    StateSite<Middle> first_site;

    //! Vector of sites excluding the left-most site in the matrix product state.
    vector<StateSite<Right> > rest_sites;
    //@+node:gcross.20110511112955.2212: *4* Flattening
    /*! @name Flattening
    Convenience functions for working with the flat vector representation of this state.
    */

    //! @{

    public:

    //! Computes the flat vector representation of this state.
    /*!
    \note Since this function necesarily has exponential running time, if you only need a few components of this vector then you should consider calling computeComponent().
    */
    StateVector computeVector() {
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
    //@+node:gcross.20110430223656.2185: *4* Informational
    //! @name Informational

    //! @{

    public:

    //! Returns the number of sites in this matrix product state;
    unsigned int numberOfSites() const { return 1+rest_sites.size(); }

    //! @}
    //@+node:gcross.20110430223656.2186: *4* Iteration support
    /*! @name Iteration support
    The State class supports iterating over the sites.  Since the sites have different normalizations, the iteration value type is the base class StateSiteAny.
    */

    //! @{

    public:

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
    };

    //! Returns an iterator at the first site in the matrix product state.
    const_iterator begin() const { return const_iterator(this,0); }

    //! Returns an iterator just past the last site in the matrix product state.
    const_iterator end() const { return const_iterator(this,numberOfSites()); }

    //! @}
    //@+node:gcross.20110430223656.2187: *4* Site access
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
    //@-others
};
//@+node:gcross.20110213161858.1813: ** Functions
IncreaseDimensionBetweenResult<Right,Right> increaseDimensionBetweenRightRight(
      unsigned int new_dimension
    , StateSite<Right> const& old_site_1
    , StateSite<Right> const& old_site_2
);

IncreaseDimensionBetweenResult<Middle,Right> increaseDimensionBetweenMiddleRight(
      unsigned int new_dimension
    , StateSite<Middle> const& old_site_1
    , StateSite<Right> const& old_site_2
);

MoveSiteCursorResult<Left> moveSiteCursorLeft(
      StateSite<Middle> const& old_state_site_2
    , StateSite<Left> const& old_state_site_1
);

MoveSiteCursorResult<Right> moveSiteCursorRight(
      StateSite<Middle> const& old_state_site_1
    , StateSite<Right> const& old_state_site_2
);

StateSite<Middle> randomStateSiteMiddle(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
);

StateSite<Right> randomStateSiteRight(
      const PhysicalDimension physical_dimension
    , const LeftDimension left_dimension
    , const RightDimension right_dimension
);
//@+node:gcross.20110215235924.1909: *3* computeExpectationValue
template<typename StateSiteRange> complex<double> computeExpectationValue(
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
}
//@+node:gcross.20110215235924.2015: *3* Unsafe
namespace Unsafe {

//@+others
//@+node:gcross.20110215235924.2016: *4* increaseDimensionBetween
template<typename side1,typename side2>
IncreaseDimensionBetweenResult<side1,side2> increaseDimensionBetween(
      unsigned int const new_dimension
    , StateSiteAny const& old_site_1
    , StateSiteAny const& old_site_2
) {
    unsigned int const old_dimension = connectDimensions(
         "state site 1 right"
        ,old_site_1.rightDimension(as_unsigned_integer)
        ,"state site 2 left"
        ,old_site_2.leftDimension(as_unsigned_integer)
    );
    assert(new_dimension >= old_dimension);

    StateSite<side1> new_site_1(
         old_site_1.physicalDimension()
        ,old_site_1.leftDimension()
        ,RightDimension(new_dimension)
    );
    StateSite<side2> new_site_2(
         old_site_2.physicalDimension()
        ,LeftDimension(new_dimension)
        ,old_site_2.rightDimension()
    );

    int const info =
        new_dimension > old_dimension
            ? Core::increase_bandwidth_between(
                 old_site_1.leftDimension(as_unsigned_integer)
                ,old_dimension
                ,old_site_2.rightDimension(as_unsigned_integer)
                ,old_site_1.physicalDimension(as_unsigned_integer)
                ,old_site_2.physicalDimension(as_unsigned_integer)
                ,new_dimension
                ,old_site_1
                ,old_site_2
                ,new_site_1
                ,new_site_2
              )
            : Core::norm_denorm_going_left(
                 old_site_1.leftDimension(as_unsigned_integer)
                ,old_dimension
                ,old_site_2.rightDimension(as_unsigned_integer)
                ,old_site_1.physicalDimension(as_unsigned_integer)
                ,old_site_2.physicalDimension(as_unsigned_integer)
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
}
//@-others

}
//@+node:gcross.20110213233103.2755: ** struct moveSiteCursor
template<typename side> struct moveSiteCursor { };

template<> struct moveSiteCursor<Left> {
    static MoveSiteCursorResult<Left> from(
          StateSite<Middle> const& old_middle_state_site
	   , StateSite<Left> const& old_left_state_site
    ) { return moveSiteCursorLeft(old_middle_state_site,old_left_state_site); }
};

template<> struct moveSiteCursor<Right> {
    static MoveSiteCursorResult<Right> from(
          StateSite<Middle> const& old_middle_state_site
	   , StateSite<Right> const& old_right_state_site
    ) { return moveSiteCursorRight(old_middle_state_site,old_right_state_site); }
};
//@-others

}

//@+<< Outside namespace >>
//@+node:gcross.20110430223656.2189: ** << Outside namespace >>
namespace boost {
    template<> struct range_iterator<Nutcracker::State const> { typedef Nutcracker::State::const_iterator type; };
}
//@-<< Outside namespace >>

#endif
//@-leo
