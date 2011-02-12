//@+leo-ver=5-thin
//@+node:gcross.20110130170743.1665: * @thin chain.hpp
//@@language cplusplus

#ifndef NUTCRACKER_CHAIN_HPP
#define NUTCRACKER_CHAIN_HPP

//@+<< Includes >>
//@+node:gcross.20110130170743.1666: ** << Includes >>
#include <boost/container/vector.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/move/move.hpp>
#include <boost/signals2/signal.hpp>
#include <functional>
#include <numeric>

#include "core.hpp"
#include "operators.hpp"
#include "tensors.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110130170743.1667: ** << Usings >>
using boost::function;
using boost::irange;
using boost::none;
using boost::reverse_copy;
using boost::signals2::signal;

using std::accumulate;
using std::min;
using std::multiplies;

namespace lambda = boost::lambda;
//@-<< Usings >>

//@+others
//@+node:gcross.20110130193548.1684: ** Exceptions
//@+node:gcross.20110202200838.1712: *3* InitialChainEnergyNotRealError
struct InitialChainEnergyNotRealError : public Exception {
    complex<double> const energy;
    InitialChainEnergyNotRealError(complex<double> const energy)
      : Exception((
            format("The initial chain energy is not real (energy = %1%).")
                % energy
        ).str())
      , energy(energy)
    {}
};
//@+node:gcross.20110130193548.1685: *3* RequestedBandwidthDimensionTooLargeError
struct RequestedBandwidthDimensionTooLargeError : public Exception {
    unsigned int const
          requested_bandwidth_dimension
        , greatest_possible_bandwidth_dimension
        ;
    RequestedBandwidthDimensionTooLargeError(
        unsigned int const requested_bandwidth_dimension
      , unsigned int const greatest_possible_bandwidth_dimension
    ) : Exception((
            format("Requested bandwidth dimension %1% is too large;  the highest possible with the given physical dimensions is %2%.")
                % requested_bandwidth_dimension
                % greatest_possible_bandwidth_dimension
        ).str())
      , requested_bandwidth_dimension(requested_bandwidth_dimension)
      , greatest_possible_bandwidth_dimension(greatest_possible_bandwidth_dimension)    
    {}
};
//@+node:gcross.20110130170743.1683: ** Functions
vector<unsigned int> computeBandwidthDimensionSequence(
    unsigned int const requested_bandwidth_dimension
   ,vector<unsigned int> const& physical_dimensions
);

vector<unsigned int> extractPhysicalDimensions(
    Operators const& operators
);
//@+node:gcross.20110208151104.1789: *3* maximumBandwidthDimension
template<typename T> unsigned int maximumBandwidthDimension(
    T const& physical_dimensions
) {
    size_t middle_index = (physical_dimensions.size()+1)/2;

    return min(
        accumulate(
             physical_dimensions.begin()
            ,physical_dimensions.begin()+middle_index
            ,1
            ,multiplies<unsigned int>()
        )
       ,accumulate(
             physical_dimensions.rbegin()
            ,physical_dimensions.rbegin()+middle_index
            ,1
            ,multiplies<unsigned int>()
        )
    );
}
//@+node:gcross.20110202175920.1701: ** Classes
//@+node:gcross.20110202175920.1702: *3* OverlapSiteTrio
struct OverlapSiteTrio {
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(OverlapSiteTrio)

    OverlapSite<Left> left;
    OverlapSite<Middle> middle;
    OverlapSite<Right> right;
public:
    OverlapSiteTrio(BOOST_RV_REF(OverlapSiteTrio) other)
      : left(boost::move(left))
      , middle(boost::move(middle))
      , right(boost::move(right))
    { }

    OverlapSiteTrio(
          BOOST_RV_REF(OverlapSite<Left>) left
        , BOOST_RV_REF(OverlapSite<Middle>) middle
        , BOOST_RV_REF(OverlapSite<Right>) right
    ) : left(left)
      , middle(middle)
      , right(right)
    { }

    void operator=(BOOST_RV_REF(OverlapSiteTrio) other) {
        left = boost::move(other.left);
        middle = boost::move(other.middle);
        right = boost::move(other.right);
    }

    void swap(OverlapSiteTrio& other) {
        left.swap(other.left);
        middle.swap(other.middle);
        right.swap(other.right);
    }

    template<typename side> OverlapSite<side> const& get() const {
        throw BadLabelException("OverlapSite::get",typeid(side));
    }
};
template<> inline OverlapSite<Left> const& OverlapSiteTrio::get<Left>() const { return left; }
template<> inline OverlapSite<Middle> const& OverlapSiteTrio::get<Middle>() const { return middle; }
template<> inline OverlapSite<Right> const& OverlapSiteTrio::get<Right>() const { return right; }
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
    Operators const operators;
    vector<vector<OverlapSiteTrio> > overlap_trios;
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
    signal<void ()> signalPerformedSweep;
    signal<void ()> signalSweepsConverged;
    signal<void ()> signalChainOptimized;

    Chain(
      Operators const& operators
    , unsigned int const initial_bandwidth = 1
    , Options const& options = defaults
    );
    void reset(unsigned int initial_bandwidth = 1);

    double getEnergy() const { return energy; }
    unsigned int bandwidthDimension() const { return bandwidth_dimension; }

    complex<double> computeExpectationValue() const;
    double computeStateNorm() const;

    template<typename side> void absorb(BOOST_RV_REF(StateSite<side>) state_site, unsigned int operator_number);
    template<typename side> void move();
    void increaseBandwidthDimension(unsigned int const new_bandwidth_dimension);

    void optimizeSite();
    void performOptimizationSweep();
    void sweepUntilConverged();
    void optimizeChain();
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
            ,*operators[site_number]
        )
    );

    vector<OverlapBoundary<side> >& overlap_boundaries = overlapBoundaries<side>();
    vector<OverlapBoundary<side> > new_overlap_boundaries;
    new_overlap_boundaries.reserve(overlap_boundaries.size());
    typename vector<OverlapBoundary<side> >::const_iterator overlap_boundary = overlap_boundaries.begin();
    BOOST_FOREACH(
         OverlapSiteTrio const& overlap_site_trio
        ,overlap_trios[site_number]
    ) {
        new_overlap_boundaries.push_back(
            contract<side>::SS(
                 *overlap_boundary
                ,overlap_site_trio.get<side>()
                ,state_site
            )
        );
        ++overlap_boundary;
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
}
//@-others
//@-others

}

#endif
//@-leo
