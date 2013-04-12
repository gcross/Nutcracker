#include "common.hpp"

using boost::make_shared;

extern "C" {

NutcrackerMatrix const
      *Nutcracker_Matrix_Pauli_I = new NutcrackerMatrix(Nutcracker::Pauli::I)
    , *Nutcracker_Matrix_Pauli_X = new NutcrackerMatrix(Nutcracker::Pauli::X)
    , *Nutcracker_Matrix_Pauli_Y = new NutcrackerMatrix(Nutcracker::Pauli::Y)
    , *Nutcracker_Matrix_Pauli_Z = new NutcrackerMatrix(Nutcracker::Pauli::Z)
    ;
NutcrackerMatrix* Nutcracker_Matrix_add(NutcrackerMatrix const* x, NutcrackerMatrix const* y) {
    using namespace Nutcracker;
    return new NutcrackerMatrix((*x) + (*y));
}
void Nutcracker_Matrix_free(NutcrackerMatrix* matrix) { delete matrix; }
void Nutcracker_Matrix_getElementAtCoordinate(NutcrackerMatrix const* x, uint32_t i, uint32_t j, std::complex<double>* element) { BEGIN_ERROR_REGION {
    *element = (**x)(i,j);
} END_ERROR_REGION() }
NutcrackerMatrix const* Nutcracker_Matrix_getPauliI() { return Nutcracker_Matrix_Pauli_I; }
NutcrackerMatrix const* Nutcracker_Matrix_getPauliX() { return Nutcracker_Matrix_Pauli_X; }
NutcrackerMatrix const* Nutcracker_Matrix_getPauliY() { return Nutcracker_Matrix_Pauli_Y; }
NutcrackerMatrix const* Nutcracker_Matrix_getPauliZ() { return Nutcracker_Matrix_Pauli_Z; }
uint32_t Nutcracker_Matrix_getSize(NutcrackerMatrix const* x) {
    return (*x)->size1();
}
NutcrackerMatrix* Nutcracker_Matrix_multiply(std::complex<double> const* c, NutcrackerMatrix const* x) {
    using namespace Nutcracker;
    return new NutcrackerMatrix((*c) * (*x));
}
NutcrackerMatrix* Nutcracker_Matrix_new(uint32_t dimension, std::complex<double> const* data) {
    using namespace Nutcracker;
    MatrixPtr matrix = make_shared<Matrix>(dimension,dimension);
    std::copy(data,data+dimension*dimension,matrix->data().begin());
    return new NutcrackerMatrix(matrix);
}
NutcrackerMatrix* Nutcracker_Matrix_newDiagonal(uint32_t dimension, std::complex<double> const* diagonal) {
    return new NutcrackerMatrix(Nutcracker::diagonalMatrix(boost::make_iterator_range(diagonal,diagonal+dimension)));
}

}
