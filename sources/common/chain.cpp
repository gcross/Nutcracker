//@+leo-ver=5-thin
//@+node:gcross.20110130170743.1674: * @thin chain.cpp
//@@language cplusplus

//@+<< Includes >>
//@+node:gcross.20110130170743.1675: ** << Includes >>
#include <algorithm>
#include <boost/assign.hpp>
#include <boost/range/algorithm/reverse_copy.hpp>
#include <iterator>
#include <utility>

#include "chain.hpp"
#include "core.hpp"


#include <iostream>
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110130170743.1676: ** << Usings >>
using namespace boost;
using namespace boost::assign;
using namespace std;

namespace moveable = boost::container;
//@-<< Usings >>

//@+others
//@+node:gcross.20110130193548.1686: ** Functions
//@+node:gcross.20110130193548.1688: *3* computeBandwidthDimensionSequence
vector<unsigned int> computeBandwidthDimensionSequence(
    unsigned int const requested_bandwidth_dimension
   ,vector<unsigned int> const& physical_dimensions
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
//@+node:gcross.20110202175920.1714: ** class Chain
//@+node:gcross.20110202175920.1715: *3* (constructors)
Chain::Chain(
      BOOST_RV_REF(moveable::vector<OperatorSite>) operators
    , unsigned int const requested_bandwidth_dimension
    , double const tolerance
    , unsigned int const maximum_number_of_iterations
) : number_of_sites(operators.size())
  , current_site_number(0)
  , left_expectation_boundary(make_trivial)
  , right_expectation_boundary(make_trivial)
  , energy(0)
  , initial_bandwidth_dimensions(0,0)
  , tolerance(tolerance)
  , maximum_number_of_iterations(maximum_number_of_iterations)
{
    assert(number_of_sites > 0);

    //@+<< Compute initial bandwidth dimension sequence >>
    //@+node:gcross.20110202175920.1716: *4* << Compute initial bandwidth dimension sequence >>
    {
        vector<unsigned int> physical_dimensions;
        BOOST_FOREACH(OperatorSite const& operator_site, operators) {
            physical_dimensions.push_back(operator_site.physicalDimension(as_unsigned_integer));
        }

        initial_bandwidth_dimensions = computeBandwidthDimensionSequence(requested_bandwidth_dimension,physical_dimensions);
    }
    //@-<< Compute initial bandwidth dimension sequence >>

    //@+<< Construct all but the first site >>
    //@+node:gcross.20110202175920.1717: *4* << Construct all but the first site >>
    {
        vector<unsigned int>::const_reverse_iterator right_dimension = initial_bandwidth_dimensions.rbegin()+1;
        while(operators.size() > 1) {
            OperatorSite& operator_site = operators.back();
            StateSite<Right> right_state_site(
                randomStateSiteRight(
                     operator_site.physicalDimension()
                    ,LeftDimension(*right_dimension)
                    ,RightDimension(*(right_dimension-1))
                )
            );
            ExpectationBoundary<Right> old_right_expectation_boundary(boost::move(right_expectation_boundary));
            right_expectation_boundary =
                contractSOSRight(
                     old_right_expectation_boundary
                    ,right_state_site
                    ,operator_site
                );
            right_neighbors.push_back(
                Neighbor<Right>(
                     boost::move(old_right_expectation_boundary)
                    ,boost::move(right_state_site)
                    ,boost::move(operator_site)
                    ,moveable::vector<OverlapBoundary<Right> >()
                    ,moveable::vector<OverlapSiteTrio>()
                )
            );
            operators.pop_back();
        }
    }
    //@-<< Construct all but the first site >>

    //@+<< Construct the first site >>
    //@+node:gcross.20110202175920.1718: *4* << Construct the first site >>
    operator_site = boost::move(operators.back());
    state_site =
        randomStateSiteMiddle(
             operator_site.physicalDimension()
            ,LeftDimension(initial_bandwidth_dimensions[0])
            ,RightDimension(initial_bandwidth_dimensions[1])
        );
    //@-<< Construct the first site >>

    //@+<< Compute the initial energy >>
    //@+node:gcross.20110202175920.1719: *4* << Compute the initial energy >>
    {
        complex<double> const expectation_value = computeExpectationValue();
        if(abs(expectation_value.imag()) > 1e-10) throw InitialChainEnergyNotRealError(expectation_value);
        energy = expectation_value.real();
    }
    //@-<< Compute the initial energy >>
}
//@+node:gcross.20110202175920.1720: *3* computeExpectationValue
complex<double> Chain::computeExpectationValue() const {
    return 
        Nutcracker::computeExpectationValue(
             left_expectation_boundary
            ,state_site
            ,operator_site
            ,right_expectation_boundary
        );
}
//@+node:gcross.20110202175920.1721: *3* computeStateNorm
double Chain::computeStateNorm() const {
    return state_site.norm();
}
//@+node:gcross.20110206130502.1754: *3* optimizeSite
unsigned int Chain::optimizeSite() {
    OptimizerResult result(
        optimizeStateSite(
             left_expectation_boundary
            ,state_site
            ,operator_site
            ,right_expectation_boundary
            ,none
            ,tolerance
            ,maximum_number_of_iterations
        )
    );
    if((result.eigenvalue - energy)/abs(energy) > tolerance) {
        throw OptimizerObtainedGreaterEigenvalue(energy,result.eigenvalue);
    }
    energy = result.eigenvalue;
    state_site = boost::move(result.state_site);
    return result.number_of_iterations;
}
//@-others

}
//@-leo
