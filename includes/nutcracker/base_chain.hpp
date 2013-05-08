#ifndef NUTCRACKER_BASE_CHAIN_HPP
#define NUTCRACKER_BASE_CHAIN_HPP

#include <boost/signal.hpp>
#include <boost/utility.hpp>

#include "nutcracker/chain_options.hpp"
#include "nutcracker/tensors.hpp"

namespace Nutcracker {

class BaseChain: boost::noncopyable, public ChainOptions { // {{{
protected:
    ExpectationBoundary<Left> left_expectation_boundary;
    ExpectationBoundary<Right> right_expectation_boundary;
    StateSite<Middle> state_site;
    double energy;

    explicit BaseChain(boost::optional<ChainOptions const&> maybe_options)
      : ChainOptions(maybe_options)
    {}

    explicit BaseChain(
        BOOST_RV_REF(ExpectationBoundary<Left>) left_expectation_boundary,
        BOOST_RV_REF(ExpectationBoundary<Right>) right_expectation_boundary,
        BOOST_RV_REF(StateSite<Middle>) state_site,
        boost::optional<ChainOptions const&> maybe_options
    ) : ChainOptions(maybe_options)
      , left_expectation_boundary(left_expectation_boundary)
      , right_expectation_boundary(right_expectation_boundary)
      , state_site(state_site)
    {}

public:
    boost::signal<void (unsigned int)> signalOptimizeSiteSuccess;
    boost::signal<void (OptimizerFailure&)> signalOptimizeSiteFailure;
    boost::signal<void ()> signalSweepPerformed;
    boost::signal<void ()> signalSweepsConverged;
    boost::signal<void ()> signalChainOptimized;

    double getEnergy() const { return energy; }

    template<typename side> ExpectationBoundary<side>& expectationBoundary() {
        throw BadLabelException("Chain::expectationBoundary()",typeid(side));
    }

    std::complex<double> computeExpectationValue() const;
    double computeStateNorm() const;

    virtual OperatorSite const& getCurrentOperatorSite() const = 0;
};

template<> inline ExpectationBoundary<Left>& BaseChain::expectationBoundary<Left>() { return left_expectation_boundary; }
template<> inline ExpectationBoundary<Right>& BaseChain::expectationBoundary<Right>() { return right_expectation_boundary; }
// }}}

}

#endif
