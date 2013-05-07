#include <boost/lambda/lambda.hpp>

#include "nutcracker/chain_options.hpp"

namespace Nutcracker {

using boost::optional;
namespace lambda = boost::lambda;

ChainOptions::ChainOptions() {
    initializeDefaults();
}

ChainOptions::ChainOptions(optional<ChainOptions const&> maybe_options) {
    if(maybe_options)
        *this = *maybe_options;
    else
        initializeDefaults();
}

void ChainOptions::initializeDefaults() {
    maximum_number_of_iterations = 10000;
    sanity_check_threshold = 1e-12;
    site_convergence_threshold = 1e-12;
    sweep_convergence_threshold = 1e-12;
    chain_convergence_threshold = 1e-12;
    initial_bandwidth_dimension = 1;
    computeNewBandwidthDimension = lambda::_1+1;
    optimizer_mode = OptimizerMode::least_value;
}

ChainOptions const ChainOptions::defaults;

}
