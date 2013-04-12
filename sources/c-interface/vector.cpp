#include <algorithm>

#include "common.hpp"

using boost::make_shared;

extern "C" {

NutcrackerVector const
      *Nutcracker_Vector_Qubit_Up = new NutcrackerVector(Nutcracker::Qubit::Up)
    , *Nutcracker_Vector_Qubit_Down = new NutcrackerVector(Nutcracker::Qubit::Down)
    ;
NutcrackerVector* Nutcracker_Vector_add(NutcrackerVector const* x, NutcrackerVector const* y) {
    using namespace Nutcracker;
    return new NutcrackerVector((*x) + (*y));
}
void Nutcracker_Vector_free(NutcrackerVector* vector) { delete vector; }
void Nutcracker_Vector_getElementAtIndex(NutcrackerVector const* x, uint32_t index, std::complex<double>* element) { BEGIN_ERROR_REGION {
    *element = (**x)[index];
} END_ERROR_REGION() }
NutcrackerVector const* Nutcracker_Vector_getQubitUp() { return Nutcracker_Vector_Qubit_Up; }
NutcrackerVector const* Nutcracker_Vector_getQubitDown() { return Nutcracker_Vector_Qubit_Down; }
uint32_t Nutcracker_Vector_getSize(NutcrackerVector const* x) {
    return (*x)->size();
}
NutcrackerVector* Nutcracker_Vector_multiply(std::complex<double> const* c, NutcrackerVector const* x) {
    using namespace Nutcracker;
    return new NutcrackerVector((*c) * (*x));
}
NutcrackerVector* Nutcracker_Vector_new(uint32_t physical_dimension, std::complex<double> const* data) {
    using namespace Nutcracker;
    VectorPtr vector = make_shared<Vector>(physical_dimension);
    std::copy(data,data+physical_dimension,vector->data().begin());
    return new NutcrackerVector(vector);
}
NutcrackerVector* Nutcracker_Vector_newBasis(uint32_t physical_dimension, uint32_t observation) { BEGIN_ERROR_REGION {
    return new NutcrackerVector(Nutcracker::basisVector(physical_dimension,observation));
} END_ERROR_REGION(NULL) }

}
