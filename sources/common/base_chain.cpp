#include "nutcracker/core.hpp"
#include "nutcracker/base_chain.hpp"

namespace Nutcracker {

complex<double> BaseChain::computeExpectationValue() const {{{
    return 
        Nutcracker::computeExpectationValueAtSite(
             left_expectation_boundary
            ,state_site
            ,getCurrentOperatorSite()
            ,right_expectation_boundary
        );
}}}

double BaseChain::computeStateNorm() const {{{
    return state_site.norm();
}}}

}
