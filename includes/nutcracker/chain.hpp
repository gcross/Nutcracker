#ifndef NUTCRACKER_CHAIN_HPP
#define NUTCRACKER_CHAIN_HPP

// Includes {{{
#include <boost/container/vector.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/move/move.hpp>
#include <boost/optional.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/utility/result_of.hpp>

#include "nutcracker/base_chain.hpp"
#include "nutcracker/boundaries.hpp"
#include "nutcracker/chain_options.hpp"
#include "nutcracker/core.hpp"
#include "nutcracker/operators.hpp"
#include "nutcracker/optimizer.hpp"
#include "nutcracker/projectors.hpp"
#include "nutcracker/states.hpp"
#include "nutcracker/tensors.hpp"
// }}}

namespace Nutcracker {

// Usings {{{
using boost::adaptors::transformed;
using boost::function;
using boost::irange;
using boost::iterator_facade;
using boost::none;
using boost::random_access_traversal_tag;

namespace lambda = boost::lambda;
// }}}

// Exceptions {{{
struct ChainNotAtFirstSiteError : public std::logic_error { // {{{
    unsigned int current_site_number;
    ChainNotAtFirstSiteError(unsigned int current_site_number)
      : std::logic_error((
            format("The requested operation requires the chain to be at the first site (that is, site #0), but it is instead at (zero-based) site #%1%")
                % current_site_number
        ).str())
      , current_site_number(current_site_number)
    {}
}; // }}}
struct InitialChainEnergyNotRealError : public std::runtime_error { // {{{
    complex<double> const energy;
    InitialChainEnergyNotRealError(complex<double> const energy)
      : std::runtime_error((
            format("The initial chain energy is not real (energy = %|.15|).")
                % energy
        ).str())
      , energy(energy)
    {}
}; // }}}
// }}}

// Classes {{{
class Solution { // {{{
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(Solution)
public:
    double eigenvalue;
    State eigenvector;

    Solution(BOOST_RV_REF(Solution) other)
      : eigenvalue(other.eigenvalue)
      , eigenvector(boost::move(other.eigenvector))
    {}

    Solution(
          double eigenvalue
        , BOOST_RV_REF(State) eigenvector
    ) : eigenvalue(eigenvalue)
      , eigenvector(eigenvector)
    {}

    Solution& operator=(BOOST_RV_REF(Solution) other) {
        eigenvalue = other.eigenvalue;
        eigenvector = boost::move(other.eigenvector);
        return *this;
    }
}; // }}}
template<typename side> struct Neighbor { // {{{
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(Neighbor)
public:
    ExpectationBoundary<side> expectation_boundary;
    StateSite<side> state_site;
    vector<OverlapBoundary<side> > overlap_boundaries;

    Neighbor(BOOST_RV_REF(Neighbor) other) // {{{
      : expectation_boundary(boost::move(other.expectation_boundary))
      , state_site(boost::move(other.state_site))
      , overlap_boundaries(boost::move(other.overlap_boundaries))
    { } // }}}

    Neighbor( // BOOST_RV_REF(all fields) {{{
          BOOST_RV_REF(ExpectationBoundary<side>) expectation_boundary
        , BOOST_RV_REF(StateSite<side>) state_site
        , BOOST_RV_REF(vector<OverlapBoundary<side> >) overlap_boundaries
    ) : expectation_boundary(expectation_boundary)
      , state_site(state_site)
      , overlap_boundaries(overlap_boundaries)
    { } // }}}

    void operator=(BOOST_RV_REF(Neighbor) other) { // {{{
        expectation_boundary = boost::move(other.expectation_boundary);
        state_site = boost::move(other.state_site);
        overlap_boundaries = boost::move(other.overlap_boundaries);
    } // }}}
}; // }}}
class Chain: public BaseChain { // {{{
public:
    unsigned int const number_of_sites;
protected:
    Operator const operator_sites;
    vector<Projector> projectors;
    unsigned int current_site_number;
    vector<OverlapBoundary<Left> > left_overlap_boundaries;
    vector<OverlapBoundary<Right> > right_overlap_boundaries;
    vector<Neighbor<Left> > left_neighbors;
    vector<Neighbor<Right> > right_neighbors;
    ProjectorMatrix projector_matrix;
    vector<unsigned int> physical_dimensions;
public:
    unsigned int const maximum_number_of_levels;
    unsigned int const maximum_bandwidth_dimension;
protected:
    unsigned int bandwidth_dimension;

    template<typename side> vector<OverlapBoundary<side> >& overlapBoundaries() {
        throw BadLabelException("Chain::overlapBoundaries()",typeid(side));
    }
    template<typename side> vector<Neighbor<side> >& neighbors() {
        throw BadLabelException("Chain::neighbors()",typeid(side));
    }
    template<typename side> void moveSiteNumber() {
        throw BadLabelException("Chain::moveSiteNumber()",typeid(side));
    }

    void resetBoundaries();
    void resetProjectorMatrix();
    void checkAtFirstSite() const;

public:
    Chain(Operator const& operator_sites, boost::optional<ChainOptions const&> maybe_options = boost::none);

    boost::signal<void ()> signalChainReset;
    function<void (BOOST_RV_REF(State) state)> storeState;

    void clear();
    void reset();

    unsigned int bandwidthDimension() const { return bandwidth_dimension; }
    virtual OperatorSite const& getCurrentOperatorSite() const { return *operator_sites[current_site_number]; }
    virtual ProjectorMatrix const& getCurrentProjectorMatrix() const { return projector_matrix; }
    virtual unsigned int getCurrentBandwidthDimension() const { return bandwidth_dimension; }
    virtual unsigned int getMaximumBandwidthDimension() const { return maximum_bandwidth_dimension; }

    using BaseChain::computeExpectationValue;
    double computeProjectorOverlapAtCurrentSite() const;
    using BaseChain::computeStateNorm;

    template<typename side> void absorb(BOOST_RV_REF(StateSite<side>) state_site, unsigned int operator_number);
    template<typename side> void move();
    void moveTo(unsigned int new_site_number);
    virtual void increaseBandwidthDimension(unsigned int const new_bandwidth_dimension);
    void constructAndAddProjectorFromState();

    using BaseChain::optimizeSite;
    virtual void performOptimizationSweep();
    void solveForMultipleLevels(unsigned int number_of_levels);
    vector<Solution> solveForMultipleLevelsAndThenClearChain(unsigned int number_of_levels);
    vector<double> solveForEigenvalues(unsigned int number_of_levels);

    virtual double getConvergenceEnergy() const { return getEnergy(); }

    State makeCopyOfState() const;
    State removeState();

    // const_iterator {{{
    friend class const_iterator;
    class const_iterator :
        public iterator_facade<
             const_iterator
            ,StateSiteAny const
            ,random_access_traversal_tag
        >
    {
        Chain const* chain;
        unsigned int index;
    public:
        const_iterator(
              Chain const* chain
            , unsigned int const index
        ) : chain(chain)
          , index(index)
        {}

        StateSiteAny const& dereference() const {
            unsigned int i = index;
            if(i < chain->left_neighbors.size()) {
                return chain->left_neighbors[i].state_site;
            }
            i -= chain->left_neighbors.size();
            if(i == 0) {
                return chain->state_site;
            }
            i = chain->right_neighbors.size()-i;
            assert(i < chain->right_neighbors.size());
            return chain->right_neighbors[i].state_site;
        }

        bool equal(const_iterator const& other) const {
            return (chain == other.chain) && (index == other.index);
        }

        void increment() { ++index; }
        void decrement() { --index; }

        void advance(size_t n) { index += n; }

        size_t distance_to(const_iterator const& other) const { return other.index - index; }
    };
    // }}}

    const_iterator begin() const { return const_iterator(this,0); }
    const_iterator end() const { return const_iterator(this,number_of_sites); }

    template<typename Outputter> void writeStateTo(Outputter& out) const;
    template<typename Callback> void callWithStateSites(Callback& callback) const;
};

// External methods {{{

template<> inline vector<OverlapBoundary<Left> >& Chain::overlapBoundaries<Left>() { return left_overlap_boundaries; }
template<> inline vector<OverlapBoundary<Right> >& Chain::overlapBoundaries<Right>() { return right_overlap_boundaries; }

template<> inline vector<Neighbor<Left> >& Chain::neighbors<Left>() { return left_neighbors; }
template<> inline vector<Neighbor<Right> >& Chain::neighbors<Right>() { return right_neighbors; }

template<> inline void Chain::moveSiteNumber<Left>() { assert(current_site_number > 0); --current_site_number; }
template<> inline void Chain::moveSiteNumber<Right>() { assert(current_site_number < number_of_sites-1); ++current_site_number; }

template<typename side> void Chain::absorb(
      BOOST_RV_REF(StateSite<side>) state_site
    , unsigned int const site_number
) {
    ExpectationBoundary<side>& expectation_boundary = expectationBoundary<side>();
    ExpectationBoundary<side> new_expectation_boundary(
        contract<side>::SOS(
             expectation_boundary
            ,state_site
            ,*operator_sites[site_number]
        )
    );

    vector<OverlapBoundary<side> >& overlap_boundaries = overlapBoundaries<side>();
    vector<OverlapBoundary<side> > new_overlap_boundaries;
    new_overlap_boundaries.reserve(projectors.size());
    BOOST_FOREACH(unsigned int const i, irange(0u,(unsigned int)projectors.size())) {
        new_overlap_boundaries.push_back(
            contract<side>::VS(
                 overlap_boundaries[i]
                ,projectors[i][site_number].get<side>()
                ,state_site
            )
        );
    }

    neighbors<side>().emplace_back(
         boost::move(expectation_boundary)
        ,boost::move(state_site)
        ,boost::move(overlap_boundaries)
    );

    expectationBoundary<side>() = boost::move(new_expectation_boundary);
    overlapBoundaries<side>() = boost::move(new_overlap_boundaries);
}
template<typename Callback> void Chain::callWithStateSites(Callback& callback) const {
    assert(current_site_number == 0);
    callback(state_site,right_neighbors | reversed | transformed(boost::bind(&Neighbor<Right>::state_site,_1)));
}
template<typename side> void Chain::move() {
    optimized = false;

    unsigned int const operator_number = current_site_number;

    moveSiteNumber<side>();

    typedef typename Other<side>::value other_side;

    vector<Neighbor<side> >& side_neighbors = neighbors<side>();

    Neighbor<side>& neighbor = side_neighbors.back();

    MoveSiteCursorResult<side> cursor(moveSiteCursor<side>::from(
         state_site
        ,neighbor.state_site
    ));

    absorb<other_side>(boost::move(cursor.other_side_state_site),operator_number);

    expectationBoundary<side>() = boost::move(neighbor.expectation_boundary);
    overlapBoundaries<side>() = boost::move(neighbor.overlap_boundaries);

    state_site = boost::move(cursor.middle_state_site);

    side_neighbors.pop_back();

    resetProjectorMatrix();
}
template<typename Outputter> void Chain::writeStateTo(Outputter& out) const {
    assert(current_site_number == 0);
    out << state_site;
    BOOST_FOREACH(Neighbor<Right> const& neighbor, right_neighbors | reversed) {
        out << neighbor.state_site;
    }
}
// }}}

// }}}
// }}}

}

#endif
