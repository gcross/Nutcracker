#include "nutcracker/core.hpp"
#include "nutcracker/base_chain.hpp"

namespace Nutcracker {

// Constructors {{{
BaseChain::BaseChain(boost::optional<ChainOptions const&> maybe_options)
  : ChainOptions(maybe_options)
  , optimized(false)
  , energy_computed(false)
  , energy(0)
{}

BaseChain::BaseChain(
    BOOST_RV_REF(ExpectationBoundary<Left>) left_expectation_boundary,
    BOOST_RV_REF(ExpectationBoundary<Right>) right_expectation_boundary,
    BOOST_RV_REF(StateSite<Middle>) state_site,
    boost::optional<ChainOptions const&> maybe_options
) : ChainOptions(maybe_options)
  , left_expectation_boundary(left_expectation_boundary)
  , right_expectation_boundary(right_expectation_boundary)
  , state_site(state_site)
  , optimized(false)
  , energy_computed(false)
  , energy(0)
{}
// }}}

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

void BaseChain::ensureEnergyComputed() {{{
    if(!energy_computed) {
        energy = computeExpectationValue().real();
        energy_computed = true;
    }
}}}

double BaseChain::getEnergy() const {{{
    const_cast<BaseChain*>(this)->ensureEnergyComputed();
    return energy;
}}}

void BaseChain::optimizeChain() {{{
    if(!getConvergenceEnergy()) sweepUntilConverged();
    double previous_convergence_energy = *getConvergenceEnergy();
    sweepUntilConverged();
    double current_convergence_energy = *getConvergenceEnergy();
    while(outsideTolerance(previous_convergence_energy,current_convergence_energy,chain_convergence_threshold)
       && getCurrentBandwidthDimension() < getMaximumBandwidthDimension()
    ) {
        increaseBandwidthDimension(min(computeNewBandwidthDimension(getCurrentBandwidthDimension()),getMaximumBandwidthDimension()));
        sweepUntilConverged();
        previous_convergence_energy = current_convergence_energy;
        current_convergence_energy = *getConvergenceEnergy();
    }
    signalChainOptimized();
}}}

void BaseChain::optimizeSite() {{{
    ensureEnergyComputed();
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
        optimized = true;
        energy_computed = true;
        signalOptimizeSiteSuccess(result.number_of_iterations);
    } catch(OptimizerFailure& failure) {
        signalOptimizeSiteFailure(failure);
    }
}}}

void BaseChain::sweepUntilConverged() {{{
    if(!getConvergenceEnergy()) performOptimizationSweep();
    double previous_convergence_energy = *getConvergenceEnergy();
    performOptimizationSweep();
    double current_convergence_energy = *getConvergenceEnergy();
    while(outsideTolerance(previous_convergence_energy,current_convergence_energy,sweep_convergence_threshold)) {
        performOptimizationSweep();
        previous_convergence_energy = current_convergence_energy;
        current_convergence_energy = *getConvergenceEnergy();
    }
    signalSweepsConverged();
}}}

void BaseChain::dump() const {{{
    std::cerr << std::endl << std::endl;
    std::cerr << "LE:";  BOOST_FOREACH(complex<double> const& x, left_expectation_boundary) { std::cerr << ' ' << x; } std::cerr << std::endl;
    std::cerr << "RE:";  BOOST_FOREACH(complex<double> const& x, right_expectation_boundary) { std::cerr << ' ' << x; } std::cerr << std::endl;
    std::cerr << "MS:";  BOOST_FOREACH(complex<double> const& x, state_site) { std::cerr << ' ' << x; } std::cerr << std::endl;
}}}

}
