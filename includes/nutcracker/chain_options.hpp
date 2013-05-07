#ifndef NUTCRACKER_CHAIN_OPTIONS_HPP
#define NUTCRACKER_CHAIN_OPTIONS_HPP

#include <boost/function.hpp>
#include <boost/optional.hpp>

#include "nutcracker/optimizer.hpp"

namespace Nutcracker {

struct ChainOptions { // {{{
protected:
    void initializeDefaults();
public:
    unsigned int maximum_number_of_iterations;
    double sanity_check_threshold;
    double site_convergence_threshold;
    double sweep_convergence_threshold;
    double chain_convergence_threshold;

    unsigned int initial_bandwidth_dimension;
    boost::function<unsigned int (unsigned int)> computeNewBandwidthDimension;

    OptimizerMode optimizer_mode;

    ChainOptions();
    explicit ChainOptions(boost::optional<ChainOptions const&> maybe_options);

    static ChainOptions const defaults;

#define GENERATE_ChainOptions_SETTER(type,underscore_name,CapsName) \
    ChainOptions& set##CapsName(type underscore_name) { \
        this->underscore_name = underscore_name; \
        return *this; \
    }

    GENERATE_ChainOptions_SETTER(unsigned int,maximum_number_of_iterations,MaximumNumberOfIterations)
    GENERATE_ChainOptions_SETTER(double,sanity_check_threshold,SanityCheckThreshold)
    GENERATE_ChainOptions_SETTER(double,site_convergence_threshold,SiteConvergenceThreshold)
    GENERATE_ChainOptions_SETTER(double,sweep_convergence_threshold,SweepConvergenceThreshold)
    GENERATE_ChainOptions_SETTER(double,chain_convergence_threshold,ChainConvergenceThreshold)
    GENERATE_ChainOptions_SETTER(unsigned int,initial_bandwidth_dimension,InitialBandwidthDimension)
    GENERATE_ChainOptions_SETTER(function<unsigned int (unsigned int)> const&,computeNewBandwidthDimension,ComputeNewBandwidthDimension)
    GENERATE_ChainOptions_SETTER(OptimizerMode const&,optimizer_mode,OptimizerMode)

#undef GENERATE_ChainOptions_SETTER

}; // }}}

}

#endif
