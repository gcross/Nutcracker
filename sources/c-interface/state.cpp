//@+leo-ver=5-thin
//@+node:gcross.20110904235122.2804: * @file state.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110904235122.2805: ** << License >>
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
//@+node:gcross.20110904235122.2806: ** << Includes >>
#include "common.hpp"

#include "nutcracker/operators.hpp"

//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110904235122.2807: ** << Usings >>
//@-<< Usings >>

extern "C" {

//@+others
//@+node:gcross.20110904235122.2808: ** Functions
//@+node:gcross.20110904235122.2868: *3* computeExpectation
void Nutcracker_State_computeExpectation(NutcrackerState const* state, NutcrackerOperator const* op, std::complex<double>* result) { BEGIN_ERROR_REGION {
    *result = Nutcracker::computeExpectationValue(*state,*op);
} END_ERROR_REGION() }
//@+node:gcross.20110904235122.2867: *3* computeOverlap
void Nutcracker_State_computeOverlap(NutcrackerState const* state1, NutcrackerState const* state2, std::complex<double>* result) { BEGIN_ERROR_REGION {
    *result = Nutcracker::computeStateOverlap(*state1,*state2);
} END_ERROR_REGION() }
//@+node:gcross.20110908221100.3024: *3* deserialize
NutcrackerState* Nutcracker_State_deserialize(uint32_t size, void const* data) { BEGIN_ERROR_REGION {
    using namespace Nutcracker;
    using namespace Nutcracker::Protobuf;
    StateBuffer buffer;
    buffer.ParseFromArray(data,size);
    State state;
    buffer >> state;
    return new NutcrackerState(boost::move(state));
} END_ERROR_REGION(NULL) }
//@+node:gcross.20110904235122.2809: *3* free
void Nutcracker_State_free(NutcrackerState* state) { delete state; }
//@+node:gcross.20110908221100.3022: *3* serialize
NutcrackerSerialization* Nutcracker_State_serialize(NutcrackerState const* state) { BEGIN_ERROR_REGION {
    return NutcrackerSerialization::create<Nutcracker::Protobuf::StateBuffer>(*state);
} END_ERROR_REGION(NULL) }
//@-others

}
//@-leo
