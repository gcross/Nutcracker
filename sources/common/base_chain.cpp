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

void BaseChain::optimizeChain() {{{
    double previous_energy = energy;
    sweepUntilConverged();
    while(outsideTolerance(previous_energy,energy,chain_convergence_threshold)
       && getCurrentBandwidthDimension() < getMaximumBandwidthDimension()
    ) {
        previous_energy = energy;
        increaseBandwidthDimension(min(computeNewBandwidthDimension(getCurrentBandwidthDimension()),getMaximumBandwidthDimension()));
        sweepUntilConverged();
    }
    signalChainOptimized();
}}}

void BaseChain::optimizeSite() {{{
    try {
        OptimizerResult result(
            optimizeStateSite(
                 left_expectation_boundary
                ,state_site
                ,getCurrentOperatorSite()
                ,right_expectation_boundary
                ,getCurrentProjectorMatrix()
                ,site_convergence_threshold
                ,sanity_check_threshold
                ,maximum_number_of_iterations
                ,optimizer_mode
            )
        );
        if(optimizer_mode.checkForRegressionFromTo(energy,result.eigenvalue,sanity_check_threshold)) {
            throw OptimizerObtainedRegressiveEigenvalue(energy,result.eigenvalue);
        }
        if((energy >= 0 && result.eigenvalue >= 0) || (energy <= 0 && result.eigenvalue <= 0) || outsideTolerance(abs(energy),abs(result.eigenvalue),sanity_check_threshold)) {
            energy = result.eigenvalue;
            state_site = boost::move(result.state_site);
        }
        signalOptimizeSiteSuccess(result.number_of_iterations);
    } catch(OptimizerFailure& failure) {
        signalOptimizeSiteFailure(failure);
    }
}}}

void BaseChain::sweepUntilConverged() {{{
    double previous_energy = energy;
    performOptimizationSweep();
    while(outsideTolerance(previous_energy,energy,sweep_convergence_threshold)) {
        previous_energy = energy;
        performOptimizationSweep();
    }
    signalSweepsConverged();
}}}

}
