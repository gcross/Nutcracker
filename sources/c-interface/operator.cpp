//@+leo-ver=5-thin
//@+node:gcross.20110823131135.2582: * @file operator.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110823131135.2584: ** << Includes >>
#include "common.hpp"

#include "nutcracker/chain.hpp"
#include "nutcracker/operators.hpp"

//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110823131135.2585: ** << Usings >>
//@-<< Usings >>

extern "C" {

//@+others
//@+node:gcross.20110823131135.2589: ** Functions
//@+node:gcross.20110908221100.3020: *3* deserialize
NutcrackerOperator* Nutcracker_Operator_deserialize(uint32_t size, void const* data) { BEGIN_ERROR_REGION {
    using namespace Nutcracker;
    using namespace Nutcracker::Protobuf;
    OperatorBuffer buffer;
    buffer.ParseFromArray(data,size);
    Operator op;
    buffer >> op;
    return new NutcrackerOperator(boost::move(op));
} END_ERROR_REGION(NULL) }
//@+node:gcross.20110903000806.2752: *3* free
void Nutcracker_Operator_free(NutcrackerOperator* op) { delete op; }
//@+node:gcross.20110908221100.3017: *3* serialize
NutcrackerSerialization* Nutcracker_Operator_serialize(NutcrackerOperator const* op) { BEGIN_ERROR_REGION {
    return NutcrackerSerialization::create<Nutcracker::Protobuf::OperatorBuffer>(*op);
} END_ERROR_REGION(NULL) }
//@+node:gcross.20110908152849.2994: *3* simpleSolveForLeastEigenvalues
void Nutcracker_Operator_simpleSolveForLeastEigenvalues(NutcrackerOperator const* op, uint32_t number_of_levels, float* eigenvalues) {
    boost::copy(
        Nutcracker::Chain(*op,
            Nutcracker::ChainOptions()
                .setOptimizerMode(Nutcracker::OptimizerMode::least_value)
                .setInitialBandwidthDimension(2u)
                .setSanityCheckThreshold(1e-12)
                .setSiteConvergenceThreshold(1e-9)
                .setSweepConvergenceThreshold(1e-8)
                .setChainConvergenceThreshold(1e-8)
        ).solveForEigenvalues(number_of_levels)
       ,eigenvalues
    );
}
//@+node:gcross.20110908152849.2996: *3* simpleSolveForLeastEigenvaluesWithEigenvectors
void Nutcracker_Operator_simpleSolveForLeastEigenvaluesWithEigenvectors(NutcrackerOperator const* op, uint32_t number_of_levels, float* eigenvalues, NutcrackerState** eigenvectors) {
    boost::container::vector<Nutcracker::Solution> solutions; solutions =
        Nutcracker::Chain(*op,
            Nutcracker::ChainOptions()
                .setOptimizerMode(Nutcracker::OptimizerMode::least_value)
                .setInitialBandwidthDimension(2u)
                .setSanityCheckThreshold(1e-12)
                .setSiteConvergenceThreshold(1e-9)
                .setSweepConvergenceThreshold(1e-8)
                .setChainConvergenceThreshold(1e-8)
        ).solveForMultipleLevelsAndThenClearChain(number_of_levels);
    BOOST_FOREACH(Nutcracker::Solution& solution, solutions) {
        *eigenvalues++ = static_cast<float>(solution.eigenvalue);
        *eigenvectors++ = new NutcrackerState(boost::move(solution.eigenvector));
    }
}
//@-others

}
//@-leo
