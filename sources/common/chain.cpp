// Includes {{{
#include <boost/assign.hpp>
#include <boost/bind.hpp>
#include <boost/foreach.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/range/adaptor/reversed.hpp>
#include <boost/range/adaptor/transformed.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <boost/range/numeric.hpp>
#include <boost/range/irange.hpp>
#include <iterator>
#include <limits>
#include <numeric>
#include <utility>

#include "nutcracker/boundaries.hpp"
#include "nutcracker/chain.hpp"
#include "nutcracker/core.hpp"
#include "nutcracker/optimizer.hpp"
#include "nutcracker/utilities.hpp"
// }}}

namespace Nutcracker {

// Usings {{{
using boost::adaptors::transformed;
using boost::adaptors::reversed;
using boost::function;
using boost::optional;

using std::abs;
using std::max;
using std::min;
using std::numeric_limits;
// }}}

Chain::Chain(Operator const& operator_sites, optional<ChainOptions const&> maybe_options) // {{{
  : BaseChain(maybe_options)
  , number_of_sites(operator_sites.size())
  , operator_sites(operator_sites)
  , physical_dimensions(extractPhysicalDimensions(operator_sites))
  , maximum_number_of_levels(computeOverflowingProduct(physical_dimensions.begin(),physical_dimensions.end()))
  , maximum_bandwidth_dimension(maximumBandwidthDimension(physical_dimensions))
{
    assert(number_of_sites > 0);

    reset();
} // }}}

void Chain::checkAtFirstSite() const {{{
    if(current_site_number != 0u) throw ChainNotAtFirstSiteError(current_site_number);
}}}

void Chain::clear() {{{
    projectors.clear();
    reset();
}}}

double Chain::computeProjectorOverlapAtCurrentSite() const {{{
    return computeOverlapWithProjectors(projector_matrix,state_site);
}}}

void Chain::constructAndAddProjectorFromState() {{{
    optimized = false;
    using namespace boost;
    checkAtFirstSite();
    projectors.push_back(
        computeProjectorFromStateSites(
             state_site
            ,right_neighbors | reversed | transformed(bind(&Neighbor<Right>::state_site,_1))
        )
    );
}}}

void Chain::increaseBandwidthDimension(unsigned int const new_bandwidth_dimension) {{{
    optimized = false;

    if(bandwidth_dimension == new_bandwidth_dimension) return;
    assert(bandwidth_dimension < new_bandwidth_dimension);
    assert(new_bandwidth_dimension <= maximum_bandwidth_dimension);
    checkAtFirstSite();
    vector<unsigned int> initial_bandwidth_dimensions = computeBandwidthDimensionSequence(new_bandwidth_dimension,physical_dimensions);
    vector<unsigned int>::const_reverse_iterator dimension_iterator = initial_bandwidth_dimensions.rbegin()+1;

    vector<Neighbor<Right> > old_right_neighbors(boost::move(right_neighbors));

    StateSite<Middle> first_state_site(boost::move(state_site));

    resetBoundaries();

    unsigned int operator_number = number_of_sites-1;
    for(vector<Neighbor<Right> >::iterator neighbor_iterator = old_right_neighbors.begin()
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

    resetProjectorMatrix();
}}}

State Chain::makeCopyOfState() const {{{
    checkAtFirstSite();
    StateSite<Middle> first_state_site(copyFrom<StateSite<Middle> const>(state_site));
    vector<StateSite<Right> > rest_state_sites; rest_state_sites.reserve(number_of_sites-1);
    BOOST_FOREACH(Neighbor<Right> const& neighbor, right_neighbors | reversed) {
        rest_state_sites.emplace_back(copyFrom<StateSite<Right> const>(neighbor.state_site));
    }
    return State(
         boost::move(first_state_site)
        ,boost::move(rest_state_sites)
    );
}}}

void Chain::moveTo(unsigned int new_site_number) {{{
    assert(new_site_number < number_of_sites);
    while(current_site_number > new_site_number) {
        move<Left>();
    }
    while(current_site_number < new_site_number) {
        move<Right>();
    }
}}}

void Chain::performOptimizationSweep() {{{
    unsigned int const starting_site = current_site_number;
    if(!optimized) optimizeSite();
    while(current_site_number+1 < number_of_sites) {
        move<Right>();
        optimizeSite();
    }
    while(current_site_number > 0) {
        move<Left>();
        optimizeSite();
    }
    while(current_site_number < starting_site) {
        move<Right>();
        optimizeSite();
    }
    signalSweepPerformed();
}}}

State Chain::removeState() {{{
    checkAtFirstSite();
    vector<StateSite<Right> > rest_state_sites;
    BOOST_FOREACH(Neighbor<Right>& neighbor, right_neighbors | reversed) {
        rest_state_sites.emplace_back(boost::move(neighbor.state_site));
    }
    right_neighbors.clear();
    return State(boost::move(state_site),boost::move(rest_state_sites));
}}}

void Chain::reset() {{{
    optimized = false;

    bandwidth_dimension =
        min(maximum_bandwidth_dimension
           ,max(initial_bandwidth_dimension
               ,minimumBandwidthDimensionForProjectorCount(
                     physical_dimensions
                    ,projectors.size()
                )
            )
        )
    ;

    current_site_number = 0;

    resetBoundaries();

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
                 operator_sites[operator_number]->physicalDimension(as_dimension)
                ,LeftDimension(*(left_dimension++))
                ,RightDimension(*(right_dimension++))
             )
            ,operator_number
        );
    }

    state_site =
        randomStateSiteMiddle(
             operator_sites[0]->physicalDimension(as_dimension)
            ,LeftDimension(initial_bandwidth_dimensions[0])
            ,RightDimension(initial_bandwidth_dimensions[1])
        );

    resetProjectorMatrix();

    if(projectors.size() > 0) {
        while(projector_matrix.orthogonalSubspaceDimension() == 0) {
            move<Right>();
        }
        state_site = applyProjectorMatrix(projector_matrix,state_site);
        assert(abs(state_site.norm()-1) < 1e-7);
        moveTo(0);
    }

    complex<double> const expectation_value = computeExpectationValue();
    if(abs(expectation_value.imag())/abs(expectation_value) > 1e-10) throw InitialChainEnergyNotRealError(expectation_value);
    energy = expectation_value.real();

    signalChainReset();
}}}

void Chain::resetBoundaries() {{{
    left_expectation_boundary = ExpectationBoundary<Left>(make_trivial);

    left_overlap_boundaries.clear();
    REPEAT(projectors.size()) {
        left_overlap_boundaries.emplace_back(make_trivial);
    }

    right_expectation_boundary = ExpectationBoundary<Right>(make_trivial);

    right_overlap_boundaries.clear();
    REPEAT(projectors.size()) {
        right_overlap_boundaries.emplace_back(make_trivial);
    }
}}}

// resetProjectorMatrix {{{
namespace resetProjectorMatrix_IMPLEMENTATION {
    struct FetchOverlapSite {
        typedef OverlapSite<Middle> const& result_type;
        unsigned int current_site_number;
        FetchOverlapSite() {}
        FetchOverlapSite(unsigned int const current_site_number) : current_site_number(current_site_number) {}
        OverlapSite<Middle> const& operator()(Projector const& projector) const { return projector[current_site_number].get<Middle>(); }
    };
}

void Chain::resetProjectorMatrix() {
    using namespace resetProjectorMatrix_IMPLEMENTATION;
    projector_matrix =
        formProjectorMatrix(
             left_overlap_boundaries
            ,right_overlap_boundaries
            ,projectors | transformed(FetchOverlapSite(current_site_number))
        );
}
// }}}

// solveForEigenvalues {{{
struct solveForEigenvalues_postSolution {
    Chain& chain;
    vector<double>& solutions;
    solveForEigenvalues_postSolution(Chain& chain, vector<double>& solutions)
      : chain(chain)
      , solutions(solutions)
    {}
    void operator()() {
        solutions.push_back(chain.getEnergy());
    }
    static int const group_id;
};
int const solveForEigenvalues_postSolution::group_id = 123456789;

vector<double> Chain::solveForEigenvalues(unsigned int number_of_levels) {
    vector<double> eigenvalues;
    signalChainOptimized.connect(solveForEigenvalues_postSolution::group_id,solveForEigenvalues_postSolution(*this,eigenvalues));
    solveForMultipleLevels(number_of_levels);
    signalChainOptimized.disconnect(solveForEigenvalues_postSolution::group_id);
    return boost::move(eigenvalues);
}
// }}}

void Chain::solveForMultipleLevels(unsigned int number_of_levels) {{{
    assert(number_of_levels+projectors.size() <= maximum_number_of_levels);
    REPEAT(number_of_levels-1) {
        optimizeChain();
        constructAndAddProjectorFromState();
        if(storeState) storeState(removeState());
        reset();
    }
    optimizeChain();
}}}

// solveForMultipleLevelsAndThenClearChain {{{
struct solveForMultipleLevelsAndThenClearChain_postSolution {
    Chain& chain;
    vector<Solution>& solutions;
    solveForMultipleLevelsAndThenClearChain_postSolution(Chain& chain, vector<Solution>& solutions)
      : chain(chain)
      , solutions(solutions)
    {}
    void operator()(BOOST_RV_REF(State) state) {
        solutions.push_back(Solution(chain.getEnergy(),state));
    }
};
vector<Solution> Chain::solveForMultipleLevelsAndThenClearChain(unsigned int number_of_levels) {
    assert(!storeState);
    vector<Solution> solutions;
    storeState = solveForMultipleLevelsAndThenClearChain_postSolution(*this,solutions);
    solveForMultipleLevels(number_of_levels);
    solutions.push_back(Solution(getEnergy(),removeState()));
    clear();
    storeState.clear();
    return boost::move(solutions);
}
// }}}

}
