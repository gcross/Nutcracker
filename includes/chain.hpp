//@+leo-ver=5-thin
//@+node:gcross.20110130170743.1665: * @thin chain.hpp
//@@language cplusplus

#ifndef NUTCRACKER_CHAIN_HPP
#define NUTCRACKER_CHAIN_HPP

//@+<< Includes >>
//@+node:gcross.20110130170743.1666: ** << Includes >>
#include <boost/container/vector.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/move/move.hpp>
#include <boost/signals2/signal.hpp>

#include "boundaries.hpp"
#include "core.hpp"
#include "operators.hpp"
#include "optimizer.hpp"
#include "projectors.hpp"
#include "states.hpp"
#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110130170743.1667: ** << Usings >>
using boost::function;
using boost::irange;
using boost::iterator_facade;
using boost::none;
using boost::random_access_traversal_tag;
using boost::signals2::signal;

namespace lambda = boost::lambda;
//@-<< Usings >>

//@+others
//@+node:gcross.20110130193548.1684: ** Exceptions
//@+node:gcross.20110202200838.1712: *3* InitialChainEnergyNotRealError
struct InitialChainEnergyNotRealError : public Exception {
    complex<double> const energy;
    InitialChainEnergyNotRealError(complex<double> const energy)
      : Exception((
            format("The initial chain energy is not real (energy = %|.15|).")
                % energy
        ).str())
      , energy(energy)
    {}
};
//@+node:gcross.20110202175920.1701: ** Classes
//@+node:gcross.20110202175920.1703: *3* Neighbor
template<typename side> struct Neighbor {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(Neighbor)
public:
    ExpectationBoundary<side> expectation_boundary;
    StateSite<side> state_site;
    vector<OverlapBoundary<side> > overlap_boundaries;

    Neighbor(BOOST_RV_REF(Neighbor) other)
      : expectation_boundary(boost::move(other.expectation_boundary))
      , state_site(boost::move(other.state_site))
      , overlap_boundaries(boost::move(other.overlap_boundaries))
    { }

    Neighbor(
          BOOST_RV_REF(ExpectationBoundary<side>) expectation_boundary
        , BOOST_RV_REF(StateSite<side>) state_site
        , BOOST_RV_REF(vector<OverlapBoundary<side> >) overlap_boundaries
    ) : expectation_boundary(expectation_boundary)
      , state_site(state_site)
      , overlap_boundaries(overlap_boundaries)
    { }

    void operator=(BOOST_RV_REF(Neighbor) other) {
        expectation_boundary = boost::move(other.expectation_boundary);
        state_site = boost::move(other.state_site);
        overlap_boundaries = boost::move(other.overlap_boundaries);
    }
};
//@+node:gcross.20110202175920.1704: *3* Chain
class Chain {
public:
    unsigned int const number_of_sites;
protected:
    Operator const operator_sites;
    vector<Projector> projectors;
    unsigned int current_site_number;
    ExpectationBoundary<Left> left_expectation_boundary;
    vector<OverlapBoundary<Left> > left_overlap_boundaries;
    ExpectationBoundary<Right> right_expectation_boundary;
    vector<OverlapBoundary<Right> > right_overlap_boundaries;
    StateSite<Middle> state_site;
    vector<Neighbor<Left> > left_neighbors;
    vector<Neighbor<Right> > right_neighbors;
    ProjectorMatrix projector_matrix;
    double energy;
    vector<unsigned int> physical_dimensions;
public:
    unsigned int const maximum_number_of_levels;
    unsigned int const maximum_bandwidth_dimension;
protected:
    unsigned int bandwidth_dimension;

    template<typename side> ExpectationBoundary<side>& expectationBoundary() {
        throw BadLabelException("Chain::expectationBoundary()",typeid(side));
    }
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

public:
    struct Options {
        unsigned int maximum_number_of_iterations;
        double site_convergence_threshold;
        double sweep_convergence_threshold;
        double chain_convergence_threshold;
        function<unsigned int (unsigned int)> computeNewBandwidthDimension;
    } options;
    static Options const defaults;

    signal<void (unsigned int)> signalOptimizeSiteSuccess;
    signal<void (OptimizerFailure&)> signalOptimizeSiteFailure;
    signal<void ()> signalSweepPerformed;
    signal<void ()> signalSweepsConverged;
    signal<void ()> signalChainOptimized;
    signal<void ()> signalChainReset;

    Chain(
      Operator const& operator_sites
    , unsigned int const initial_bandwidth = 1
    , Options const& options = defaults
    );
    void reset(unsigned int initial_bandwidth = 1);

    double getEnergy() const { return energy; }
    unsigned int bandwidthDimension() const { return bandwidth_dimension; }

    complex<double> computeExpectationValueAtCurrentSite() const;
    double computeProjectorOverlapAtCurrentSite() const;
    double computeStateNorm() const;

    template<typename side> void absorb(BOOST_RV_REF(StateSite<side>) state_site, unsigned int operator_number);
    template<typename side> void move();
    void moveTo(unsigned int new_site_number);
    void increaseBandwidthDimension(unsigned int const new_bandwidth_dimension);
    void convertStateToProjectorAndReset(unsigned int const new_bandwidth_dimension=1);

    void optimizeSite();
    void performOptimizationSweep();
    void sweepUntilConverged();
    void optimizeChain();
    void solveForMultipleLevels(unsigned int number_of_levels);

    State makeCopyOfState() const;

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

    const_iterator begin() const { return const_iterator(this,0); }
    const_iterator end() const { return const_iterator(this,number_of_sites); }
};

template<> inline ExpectationBoundary<Left>& Chain::expectationBoundary<Left>() { return left_expectation_boundary; }
template<> inline ExpectationBoundary<Right>& Chain::expectationBoundary<Right>() { return right_expectation_boundary; }

template<> inline vector<OverlapBoundary<Left> >& Chain::overlapBoundaries<Left>() { return left_overlap_boundaries; }
template<> inline vector<OverlapBoundary<Right> >& Chain::overlapBoundaries<Right>() { return right_overlap_boundaries; }

template<> inline vector<Neighbor<Left> >& Chain::neighbors<Left>() { return left_neighbors; }
template<> inline vector<Neighbor<Right> >& Chain::neighbors<Right>() { return right_neighbors; }

template<> inline void Chain::moveSiteNumber<Left>() { assert(current_site_number > 0); --current_site_number; }
template<> inline void Chain::moveSiteNumber<Right>() { assert(current_site_number < number_of_sites-1); ++current_site_number; }

//@+others
//@+node:gcross.20110207120702.1786: *4* absorb
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
            contract<side>::SS(
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
//@+node:gcross.20110202175920.1705: *4* move
template<typename side> void Chain::move() {
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
//@-others
//@-others

}

#endif
//@-leo
