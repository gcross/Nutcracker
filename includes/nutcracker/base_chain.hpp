#ifndef NUTCRACKER_BASE_CHAIN_HPP
#define NUTCRACKER_BASE_CHAIN_HPP

#include <boost/signal.hpp>
#include <boost/utility.hpp>

#include "nutcracker/chain_options.hpp"
#include "nutcracker/tensors.hpp"

namespace Nutcracker {

class BaseChain: public ChainOptions { // {{{
private:
    BOOST_MOVABLE_BUT_NOT_COPYABLE(BaseChain)
protected:
    ExpectationBoundary<Left> left_expectation_boundary;
    ExpectationBoundary<Right> right_expectation_boundary;
    StateSite<Middle> state_site;
    bool optimized, energy_computed;
    double energy;

    explicit BaseChain(BOOST_RV_REF(BaseChain) other);

    explicit BaseChain(boost::optional<ChainOptions const&> maybe_options);

    explicit BaseChain(
        BOOST_RV_REF(ExpectationBoundary<Left>) left_expectation_boundary,
        BOOST_RV_REF(ExpectationBoundary<Right>) right_expectation_boundary,
        BOOST_RV_REF(StateSite<Middle>) state_site,
        boost::optional<ChainOptions const&> maybe_options
    );

    void ensureEnergyComputed();

public:
    boost::signal<void (unsigned int)> signalOptimizeSiteSuccess;
    boost::signal<void (OptimizerFailure&)> signalOptimizeSiteFailure;
    boost::signal<void ()> signalSweepPerformed;
    boost::signal<void ()> signalSweepsConverged;
    boost::signal<void ()> signalChainOptimized;

    void setOptimizerSiteFailureToThrow() { signalOptimizeSiteFailure.connect(rethrow<OptimizerFailure&>); }

    template<typename side> ExpectationBoundary<side>& expectationBoundary() {
        throw BadLabelException("Chain::expectationBoundary()",typeid(side));
    }

    std::complex<double> computeExpectationValue() const;
    double computeStateNorm() const;
    double getEnergy() const;
    virtual boost::optional<double> getConvergenceEnergy() const = 0;

    virtual OperatorSite const& getCurrentOperatorSite() const = 0;
    virtual ProjectorMatrix const& getCurrentProjectorMatrix() const = 0;
    virtual unsigned int getCurrentBandwidthDimension() const = 0;
    virtual unsigned int getMaximumBandwidthDimension() const = 0;

    void optimizeSite();
    virtual void performOptimizationSweep() = 0;
    virtual void increaseBandwidthDimension(unsigned int const new_bandwidth_dimension) = 0;
    void sweepUntilConverged();
    void optimizeChain();

    virtual void dump() const;
};

template<> inline ExpectationBoundary<Left>& BaseChain::expectationBoundary<Left>() { return left_expectation_boundary; }
template<> inline ExpectationBoundary<Right>& BaseChain::expectationBoundary<Right>() { return right_expectation_boundary; }
// }}}

}

#endif
