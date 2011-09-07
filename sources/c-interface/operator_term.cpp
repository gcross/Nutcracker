//@+leo-ver=5-thin
//@+node:gcross.20110906155043.4833: * @file operator_term.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110906155043.4834: ** << License >>
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
//@+node:gcross.20110906155043.4835: ** << Includes >>
#include "common.hpp"

#include "nutcracker/compiler.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110906155043.4836: ** << Usings >>
//@-<< Usings >>

extern "C" {

//@+others
//@+node:gcross.20110906155043.4837: ** Functions
//@+node:gcross.20110906155043.4840: *3* create_GlobalExternalField
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_GlobalExternalField(NutcrackerMatrix const* field_matrix) {
    return new NutcrackerOperatorTermWrapper<Nutcracker::GlobalExternalField>(*field_matrix);
}
//@+node:gcross.20110906155043.4844: *3* create_GlobalNeighborCouplingField
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_GlobalNeighborCouplingField(NutcrackerMatrix const* left_field_matrix, NutcrackerMatrix const* right_field_matrix) {
    return new NutcrackerOperatorTermWrapper<Nutcracker::GlobalNeighborCouplingField>(*left_field_matrix,*right_field_matrix);
}
//@+node:gcross.20110906155043.4838: *3* create_LocalExternalField
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_LocalExternalField(uint32_t site_number, NutcrackerMatrix const* field_matrix) {
    return new NutcrackerOperatorTermWrapper<Nutcracker::LocalExternalField>(site_number,*field_matrix);
}
//@+node:gcross.20110906155043.4842: *3* create_LocalNeighborCouplingField
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_LocalNeighborCouplingField(uint32_t site_number, NutcrackerMatrix const* left_field_matrix, NutcrackerMatrix const* right_field_matrix) {
    return new NutcrackerOperatorTermWrapper<Nutcracker::LocalNeighborCouplingField>(site_number,*left_field_matrix,*right_field_matrix);
}
//@+node:gcross.20110906155043.4846: *3* create_TransverseIsingField
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_TransverseIsingField(NutcrackerMatrix const* external_field_matrix, NutcrackerMatrix const* left_coupling_field_matrix, NutcrackerMatrix const* right_coupling_field_matrix) {
    return new NutcrackerOperatorTermWrapper<Nutcracker::TransverseIsingField>(*external_field_matrix,*left_coupling_field_matrix,*right_coupling_field_matrix);
}
//@+node:gcross.20110906155043.4847: *3* free
void Nutcracker_OperatorTerm_free(NutcrackerOperatorTerm* op) {
    delete op;
}
//@+node:gcross.20110906155043.4879: *3* multiply
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_multiply(std::complex<double> const* c, NutcrackerOperatorTerm const* x) {
    return x->copyAndMultiplyBy(*c);
}
//@-others

}
//@-leo
