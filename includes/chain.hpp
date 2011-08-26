//@+leo-ver=5-thin
//@+node:gcross.20110130170743.1665: * @file chain.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110220182654.2008: ** << License >>
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

#ifndef NUTCRACKER_CHAIN_HPP
#define NUTCRACKER_CHAIN_HPP

//@+<< Includes >>
//@+node:gcross.20110130170743.1666: ** << Includes >>
#include <boost/container/vector.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/move/move.hpp>
#include <boost/signal.hpp>

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
using boost::signal;

namespace lambda = boost::lambda;
//@-<< Usings >>

//@+others
//@+node:gcross.20110130193548.1684: ** Exceptions
//@+node:gcross.20110822214054.2527: *3* ChainNotAtFirstSiteError
struct ChainNotAtFirstSiteError : public std::logic_error {
    unsigned int current_site_number;
    ChainNotAtFirstSiteError(unsigned int current_site_number)
      : std::logic_error((
            format("The requested operation requires the chain to be at the first site (that is, site #0), but it is instead at (zero-based) site #%1%")
                % current_site_number
        ).str())
      , current_site_number(current_site_number)
    {}
};
//@+node:gcross.20110202200838.1712: *3* InitialChainEnergyNotRealError
struct InitialChainEnergyNotRealError : public std::runtime_error {
    complex<double> const energy;
    InitialChainEnergyNotRealError(complex<double> const energy)
      : std::runtime_error((
            format("The initial chain energy is not real (energy = %|.15|).")
                % energy
        ).str())
      , energy(energy)
    {}
};
//@+node:gcross.20110202175920.1701: ** Classes
//@+node:gcross.20110824002720.2604: *3* Solution
class Solution {
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
};
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
//@+node:gcross.20110823190118.2578: *3* ChainOptions
struct ChainOptions {
    unsigned int maximum_number_of_iterations;
    double sanity_check_threshold;
    double site_convergence_threshold;
    double sweep_convergence_threshold;
    double chain_convergence_threshold;

    unsigned int initial_bandwidth_dimension;
    function<unsigned int (unsigned int)> computeNewBandwidthDimension;

    OptimizerMode optimizer_mode;

    ChainOptions()
      : maximum_number_of_iterations(10000)
      , sanity_check_threshold(1e-12)
      , site_convergence_threshold(1e-12)
      , sweep_convergence_threshold(1e-12)
      , chain_convergence_threshold(1e-12)
      , initial_bandwidth_dimension(1)
      , computeNewBandwidthDimension(lambda::_1+1)
      , optimizer_mode(OptimizerMode::least_value)
    {}

    static ChainOptions const defaults;

#define GENERATE_ChainOptions_SETTER(type,underscore_name,CapsName) \
    ChainOptions& set##CapsName(type underscore_name) { \
        this->underscore_name = underscore_name; \
        return *this; \
    }

    GENERATE_ChainOptions_SETTER(unsigned int,maximum_number_of_iterations,MaximumNumberOfIterations)
    GENERATE_ChainOptions_SETTER(double,sanity_check_threshold,SanityCheckThreshold)
    GENERATE_ChainOptions_SETTER(double,site_convergence_threshold,SiteConvergenceThreshold)
    GENERATE_ChainOptions_SETTER(double,sweep_convergence_threshold,SweepConvergenceThreshold)
    GENERATE_ChainOptions_SETTER(double,chain_convergence_threshold,ChainConvergenceThreshold)
    GENERATE_ChainOptions_SETTER(unsigned int,initial_bandwidth_dimension,InitialBandwidthDimension)
    GENERATE_ChainOptions_SETTER(function<unsigned int (unsigned int)> const&,computeNewBandwidthDimension,ComputeNewBandwidthDimension)
    GENERATE_ChainOptions_SETTER(OptimizerMode const&,optimizer_mode,OptimizerMode)

#undef GENERATE_ChainOptions_SETTER

};
//@+node:gcross.20110202175920.1704: *3* Chain
class Chain: public ChainOptions {
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
    void checkAtFirstSite() const;

public:
    Chain(Operator const& operator_sites);
    Chain(Operator const& operator_sites, ChainOptions const& options);

    signal<void (unsigned int)> signalOptimizeSiteSuccess;
    signal<void (OptimizerFailure&)> signalOptimizeSiteFailure;
    signal<void ()> signalSweepPerformed;
    signal<void ()> signalSweepsConverged;
    signal<void ()> signalChainOptimized;
    signal<void ()> signalChainReset;

    void clear();
    void reset();

    double getEnergy() const { return energy; }
    unsigned int bandwidthDimension() const { return bandwidth_dimension; }

    complex<double> computeExpectationValueAtCurrentSite() const;
    double computeProjectorOverlapAtCurrentSite() const;
    double computeStateNorm() const;

    template<typename side> void absorb(BOOST_RV_REF(StateSite<side>) state_site, unsigned int operator_number);
    template<typename side> void move();
    void moveTo(unsigned int new_site_number);
    void increaseBandwidthDimension(unsigned int const new_bandwidth_dimension);
    void constructAndAddProjectorFromState();

    void optimizeSite();
    void performOptimizationSweep();
    void sweepUntilConverged();
    void optimizeChain();
    void solveForMultipleLevels(unsigned int number_of_levels);

    vector<double> solveForEigenvaluesAndThenClearChain(unsigned int number_of_levels);
    vector<Solution> solveForEigenvaluesAndEigenvectorsAndThenClearChain(unsigned int number_of_levels);

    State makeCopyOfState() const;
    State removeState();

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

    template<typename Outputter> void writeStateTo(Outputter& out) const;
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
//@+node:gcross.20110511190907.3683: *4* writeStateTo
template<typename Outputter> void Chain::writeStateTo(Outputter& out) const {
    assert(current_site_number == 0);
    out << state_site;
    BOOST_FOREACH(Neighbor<Right> const& neighbor, right_neighbors | reversed) {
        out << neighbor.state_site;
    }
}
//@-others
//@-others

}

#endif
//@-leo
