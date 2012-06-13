//@+leo-ver=5-thin
//@+node:gcross.20110823131135.2582: * @file operator.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110823131135.2583: ** << License >>
//@+at
// Copyright (c) 2011, Gregory Crosswhite
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//@@c
//@-<< License >>

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
