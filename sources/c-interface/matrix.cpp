//@+leo-ver=5-thin
//@+node:gcross.20110904235122.2777: * @file matrix.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110904235122.2778: ** << License >>
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
//@+node:gcross.20110904235122.2779: ** << Includes >>
#include "common.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110904235122.2780: ** << Usings >>
using boost::make_shared;
//@-<< Usings >>

extern "C" {

//@+others
//@+node:gcross.20110904235122.2783: ** Values
NutcrackerMatrix const
      *Nutcracker_Matrix_Pauli_I = new NutcrackerMatrix(Nutcracker::Pauli::I)
    , *Nutcracker_Matrix_Pauli_X = new NutcrackerMatrix(Nutcracker::Pauli::X)
    , *Nutcracker_Matrix_Pauli_Y = new NutcrackerMatrix(Nutcracker::Pauli::Y)
    , *Nutcracker_Matrix_Pauli_Z = new NutcrackerMatrix(Nutcracker::Pauli::Z)
    ;
//@+node:gcross.20110904235122.2781: ** Functions
//@+node:gcross.20110904235122.2786: *3* add
NutcrackerMatrix* Nutcracker_Matrix_add(NutcrackerMatrix const* x, NutcrackerMatrix const* y) {
    using namespace Nutcracker;
    return new NutcrackerMatrix((*x) + (*y));
}
//@+node:gcross.20110904235122.2784: *3* free
void Nutcracker_Matrix_free(NutcrackerMatrix* matrix) { delete matrix; }
//@+node:gcross.20110904235122.2785: *3* getPauli
NutcrackerMatrix const* Nutcracker_Matrix_getPauliI() { return Nutcracker_Matrix_Pauli_I; }
NutcrackerMatrix const* Nutcracker_Matrix_getPauliX() { return Nutcracker_Matrix_Pauli_X; }
NutcrackerMatrix const* Nutcracker_Matrix_getPauliY() { return Nutcracker_Matrix_Pauli_Y; }
NutcrackerMatrix const* Nutcracker_Matrix_getPauliZ() { return Nutcracker_Matrix_Pauli_Z; }
//@+node:gcross.20110904235122.2787: *3* multiply
NutcrackerMatrix* Nutcracker_Matrix_multiply(std::complex<double> const* c, NutcrackerMatrix const* x) {
    using namespace Nutcracker;
    return new NutcrackerMatrix((*c) * (*x));
}
//@+node:gcross.20110905174854.2851: *3* new
NutcrackerMatrix* Nutcracker_Matrix_new(unsigned int dimension, std::complex<double> const* data) {
    using namespace Nutcracker;
    MatrixPtr matrix = make_shared<Matrix>(dimension,dimension);
    std::copy(data,data+dimension*dimension,matrix->data().begin());
    return new NutcrackerMatrix(matrix);
}
//@+node:gcross.20110905174854.2853: *3* newDiagonal
NutcrackerMatrix* Nutcracker_Matrix_newDiagonal(unsigned int dimension, std::complex<double> const* diagonal) {
    return new NutcrackerMatrix(Nutcracker::diagonalMatrix(boost::make_iterator_range(diagonal,diagonal+dimension)));
}
//@-others

}
//@-leo
