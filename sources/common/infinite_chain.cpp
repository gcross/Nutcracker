#include "nutcracker/core.hpp"
#include "nutcracker/infinite_chain.hpp"

complex<double> InfiniteChain::computeExpectationValue() const {{{
    return 
        Nutcracker::computeExpectationValueAtSite(
             left_expectation_boundary
            ,state_site
            ,operator_site
            ,right_expectation_boundary
        );
}}}
