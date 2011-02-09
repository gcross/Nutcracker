//@+leo-ver=5-thin
//@+node:gcross.20110130170743.1674: * @thin chain.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110130170743.1675: ** << Includes >>
#include <algorithm>
#include <boost/assign.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <boost/range/irange.hpp>
#include <iterator>
#include <limits>
#include <utility>

#include "chain.hpp"
#include "core.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110130170743.1676: ** << Usings >>
using namespace boost;
using namespace boost::assign;
using namespace std;

namespace moveable = boost::container;

using boost::adaptors::reversed;
using std::numeric_limits;
//@-<< Usings >>

//@+others
//@+node:gcross.20110208151104.1790: ** Values
Chain::Options const Chain::defaults =
    { 10000
    , 1e-15
    , 1e-14
    , 1e-13
    , lambda::_1+1
    };
//@+node:gcross.20110130193548.1686: ** Functions
//@+node:gcross.20110130193548.1688: *3* computeBandwidthDimensionSequence
vector<unsigned int> computeBandwidthDimensionSequence(
    unsigned int const requested_bandwidth_dimension
   ,moveable::vector<unsigned int> const& physical_dimensions
) {
    size_t middle_index = (physical_dimensions.size()+1)/2;

    vector<unsigned int> forward_bandwidth_dimensions;
    forward_bandwidth_dimensions.reserve(physical_dimensions.size());
    unsigned int forward_bandwidth_dimension = 1;
    BOOST_FOREACH(
         unsigned int const physical_dimension
        ,make_pair(
             physical_dimensions.begin()
            ,physical_dimensions.begin()+middle_index
        )
    ) {
        forward_bandwidth_dimensions.push_back(forward_bandwidth_dimension);
        forward_bandwidth_dimension *= physical_dimension;
        if(forward_bandwidth_dimension >= requested_bandwidth_dimension) {
            forward_bandwidth_dimension = requested_bandwidth_dimension;
            break;
        }
    }

    vector<unsigned int> reverse_bandwidth_dimensions;
    unsigned int reverse_bandwidth_dimension = 1;
    BOOST_FOREACH(
         unsigned int const physical_dimension
        ,make_pair(
             physical_dimensions.rbegin()
            ,physical_dimensions.rbegin()+middle_index
        )
    ) {
        reverse_bandwidth_dimensions.push_back(reverse_bandwidth_dimension);
        reverse_bandwidth_dimension *= physical_dimension;
        if(reverse_bandwidth_dimension >= requested_bandwidth_dimension) {
            reverse_bandwidth_dimension = requested_bandwidth_dimension;
            break;
        }
    }

    if(forward_bandwidth_dimension < requested_bandwidth_dimension
     ||reverse_bandwidth_dimension < requested_bandwidth_dimension
      ) {
        throw RequestedBandwidthDimensionTooLargeError(
                 requested_bandwidth_dimension
                ,min(forward_bandwidth_dimension,reverse_bandwidth_dimension)
        );
    }

    fill_n(
         back_inserter(forward_bandwidth_dimensions)
        ,physical_dimensions.size()
            - forward_bandwidth_dimensions.size()
            - reverse_bandwidth_dimensions.size()
            + 1
        ,requested_bandwidth_dimension
    );

    reverse_copy(
         reverse_bandwidth_dimensions
        ,back_inserter(forward_bandwidth_dimensions)
    );

    return forward_bandwidth_dimensions;
}
//@+node:gcross.20110207215504.1786: *3* extractPhysicalDimensions
moveable::vector<unsigned int> extractPhysicalDimensions(Operators const& operators) {
    moveable::vector<unsigned int> physical_dimensions;
    physical_dimensions.reserve(operators.size()+1);
    BOOST_FOREACH(shared_ptr<OperatorSite const> const& operator_site, operators) {
        physical_dimensions.push_back(operator_site->physicalDimension(as_unsigned_integer));
    }
    return boost::move(physical_dimensions);
}
//@+node:gcross.20110202175920.1714: ** class Chain
//@+node:gcross.20110202175920.1715: *3* (constructors)
Chain::Chain(
      Operators const& operators
    , unsigned int const bandwidth_dimension
    , Options const& options
) : number_of_sites(operators.size())
  , operators(operators)
  , overlap_trios(number_of_sites)
  , current_site_number(0)
  , left_expectation_boundary(make_trivial)
  , right_expectation_boundary(make_trivial)
  , energy(0)
  , physical_dimensions(extractPhysicalDimensions(operators))
  , maximum_bandwidth_dimension(maximumBandwidthDimension(physical_dimensions))
  , bandwidth_dimension(bandwidth_dimension)
  , options(options)
{
    assert(number_of_sites > 0);

    reset(bandwidth_dimension);
}
//@+node:gcross.20110208233325.1798: *3* reset
void Chain::reset(unsigned int bandwidth_dimension) {
    current_site_number = 0;

    right_neighbors.clear();
    right_neighbors.reserve(number_of_sites-1);

    vector<unsigned int> initial_bandwidth_dimensions = computeBandwidthDimensionSequence(bandwidth_dimension,physical_dimensions);
    vector<unsigned int>::const_reverse_iterator
          right_dimension = initial_bandwidth_dimensions.rbegin()
        , left_dimension = right_dimension+1;
    BOOST_FOREACH(
         unsigned int const operator_number
        ,irange(1u,number_of_sites) | reversed
    ) {
        absorb<Right>(
             randomStateSiteRight(
                 operators[operator_number]->physicalDimension()
                ,LeftDimension(*(left_dimension++))
                ,RightDimension(*(right_dimension++))
             )
            ,operator_number
        );
    }

    state_site =
        randomStateSiteMiddle(
             operators[0]->physicalDimension()
            ,LeftDimension(initial_bandwidth_dimensions[0])
            ,RightDimension(initial_bandwidth_dimensions[1])
        );

    complex<double> const expectation_value = computeExpectationValue();
    if(abs(expectation_value.imag()) > 1e-10) throw InitialChainEnergyNotRealError(expectation_value);
    energy = expectation_value.real();
}
//@+node:gcross.20110202175920.1720: *3* computeExpectationValue
complex<double> Chain::computeExpectationValue() const {
    return 
        Nutcracker::computeExpectationValue(
             left_expectation_boundary
            ,state_site
            ,*operators[current_site_number]
            ,right_expectation_boundary
        );
}
//@+node:gcross.20110202175920.1721: *3* computeStateNorm
double Chain::computeStateNorm() const {
    return state_site.norm();
}
//@+node:gcross.20110207120702.1784: *3* increaseBandwidthDimension
void Chain::increaseBandwidthDimension(unsigned int const new_bandwidth_dimension) {
    if(bandwidth_dimension == new_bandwidth_dimension) return;
    assert(bandwidth_dimension < new_bandwidth_dimension);
    assert(new_bandwidth_dimension <= maximum_bandwidth_dimension);
    assert(current_site_number == 0);
    vector<unsigned int> initial_bandwidth_dimensions = computeBandwidthDimensionSequence(new_bandwidth_dimension,physical_dimensions);
    vector<unsigned int>::const_reverse_iterator dimension_iterator = initial_bandwidth_dimensions.rbegin()+1;

    moveable::vector<Neighbor<Right> > old_right_neighbors(boost::move(right_neighbors));

    StateSite<Middle> first_state_site(boost::move(state_site));

    {
        ExpectationBoundary<Right> trivial(make_trivial);
        right_expectation_boundary = boost::move(trivial);
    }

    unsigned int operator_number = number_of_sites-1;
    for(moveable::vector<Neighbor<Right> >::iterator neighbor_iterator = old_right_neighbors.begin()
       ;neighbor_iterator != old_right_neighbors.end()
       ;++neighbor_iterator,--operator_number
    ) {
        if(operator_number > 1) {
            StateSite<Right>& next_right_state_site = (neighbor_iterator+1)->state_site;
            IncreaseDimensionBetweenResult<Right,Right> result(
                increaseDimensionBetweenRightRight(
                     *(dimension_iterator++)
                    ,next_right_state_site
                    ,neighbor_iterator->state_site
                )
            );
            next_right_state_site = boost::move(result.state_site_1);
            absorb<Right>(boost::move(result.state_site_2),operator_number);
        } else {
            IncreaseDimensionBetweenResult<Middle,Right> result(
                increaseDimensionBetweenMiddleRight(
                     *(dimension_iterator++)
                    ,first_state_site
                    ,neighbor_iterator->state_site
                )
            );
            state_site = boost::move(result.state_site_1);
            absorb<Right>(boost::move(result.state_site_2),operator_number);
        }
    }
    bandwidth_dimension = new_bandwidth_dimension;
}
//@+node:gcross.20110206130502.1754: *3* optimizeSite
void Chain::optimizeSite() {
    try {
        OptimizerResult result(
            optimizeStateSite(
                 left_expectation_boundary
                ,state_site
                ,*operators[current_site_number]
                ,right_expectation_boundary
                ,none
                ,options.site_convergence_threshold
                ,options.maximum_number_of_iterations
            )
        );
        if((result.eigenvalue - energy)/abs(energy) > options.site_convergence_threshold) {
            throw OptimizerObtainedGreaterEigenvalue(energy,result.eigenvalue);
        }
        energy = result.eigenvalue;
        state_site = boost::move(result.state_site);
        signalOptimizeSiteSuccess(result.number_of_iterations);
    } catch(OptimizerFailure& failure) {
        signalOptimizeSiteFailure(failure);
    }
}
//@+node:gcross.20110206130502.1759: *3* performOptimizationSweep
void Chain::performOptimizationSweep() {
    unsigned int const starting_site = current_site_number;
    while(current_site_number+1 < number_of_sites) {
        optimizeSite();
        move<Right>();
    }
    while(current_site_number > 0) {
        optimizeSite();
        move<Left>();
    }
    while(current_site_number < starting_site) {
        optimizeSite();
        move<Right>();
    }
    signalPerformedSweep();
}
//@+node:gcross.20110208151104.1791: *3* sweepUntilConverged
void Chain::sweepUntilConverged() {
    double previous_energy = numeric_limits<double>::max();
    while(abs(previous_energy - energy)/(abs(previous_energy)+abs(energy)+options.sweep_convergence_threshold) > options.sweep_convergence_threshold) {
        previous_energy = energy;
        performOptimizationSweep();
    }
    signalSweepsConverged();
}
//@+node:gcross.20110208230325.1790: *3* optimizeChain
void Chain::optimizeChain() {
    double previous_energy = energy;
    sweepUntilConverged();
    while(abs(previous_energy - energy)/(abs(previous_energy)+abs(energy)+options.chain_convergence_threshold) > options.chain_convergence_threshold) {
        previous_energy = energy;
        increaseBandwidthDimension(options.computeNewBandwidthDimension(bandwidth_dimension));
        sweepUntilConverged();
    }
    signalChainOptimized();
}
//@-others

}
//@-leo
