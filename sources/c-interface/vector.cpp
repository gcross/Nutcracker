//@+leo-ver=5-thin
//@+node:gcross.20110904235122.2823: * @file vector.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110904235122.2824: ** << License >>
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
//@+node:gcross.20110904235122.2825: ** << Includes >>
#include <algorithm>

#include "common.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110904235122.2826: ** << Usings >>
using boost::make_shared;
//@-<< Usings >>

extern "C" {

//@+others
//@+node:gcross.20110905174854.2834: ** Values
NutcrackerVector const
      *Nutcracker_Vector_Qubit_Up = new NutcrackerVector(Nutcracker::Qubit::Up)
    , *Nutcracker_Vector_Qubit_Down = new NutcrackerVector(Nutcracker::Qubit::Down)
    ;
//@+node:gcross.20110904235122.2828: ** Functions
//@+node:gcross.20110904235122.2829: *3* add
NutcrackerVector* Nutcracker_Vector_add(NutcrackerVector const* x, NutcrackerVector const* y) {
    using namespace Nutcracker;
    return new NutcrackerVector((*x) + (*y));
}
//@+node:gcross.20110904235122.2830: *3* free
void Nutcracker_Vector_free(NutcrackerVector* vector) { delete vector; }
//@+node:gcross.20110905174854.2836: *3* getQubit
NutcrackerVector const* Nutcracker_Vector_getQubitUp() { return Nutcracker_Vector_Qubit_Up; }
NutcrackerVector const* Nutcracker_Vector_getQubitDown() { return Nutcracker_Vector_Qubit_Down; }
//@+node:gcross.20110904235122.2832: *3* multiply
NutcrackerVector* Nutcracker_Vector_multiply(std::complex<double> const* c, NutcrackerVector const* x) {
    using namespace Nutcracker;
    return new NutcrackerVector((*c) * (*x));
}
//@+node:gcross.20110904235122.2869: *3* new
NutcrackerVector* Nutcracker_Vector_new(unsigned int physical_dimension, std::complex<double> const* data) {
    using namespace Nutcracker;
    VectorPtr vector = make_shared<Vector>(physical_dimension);
    std::copy(data,data+physical_dimension,vector->data().begin());
    return new NutcrackerVector(vector);
}
//@+node:gcross.20110904235122.2871: *3* newBasis
NutcrackerVector* Nutcracker_Vector_newBasis(unsigned int physical_dimension, unsigned int observation) { BEGIN_ERROR_REGION {
    return new NutcrackerVector(Nutcracker::basisVector(physical_dimension,observation));
} END_ERROR_REGION(NULL) }
//@-others

}
//@-leo
