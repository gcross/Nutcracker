//@+leo-ver=5-thin
//@+node:gcross.20110904235122.2823: * @file vector.cpp
//@@language cplusplus
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
//@+node:gcross.20110906104131.3071: *3* getElementAtIndex
void Nutcracker_Vector_getElementAtIndex(NutcrackerVector const* x, uint32_t index, std::complex<double>* element) { BEGIN_ERROR_REGION {
    *element = (**x)[index];
} END_ERROR_REGION() }
//@+node:gcross.20110905174854.2836: *3* getQubit
NutcrackerVector const* Nutcracker_Vector_getQubitUp() { return Nutcracker_Vector_Qubit_Up; }
NutcrackerVector const* Nutcracker_Vector_getQubitDown() { return Nutcracker_Vector_Qubit_Down; }
//@+node:gcross.20110906104131.3070: *3* getSize
uint32_t Nutcracker_Vector_getSize(NutcrackerVector const* x) {
    return (*x)->size();
}
//@+node:gcross.20110904235122.2832: *3* multiply
NutcrackerVector* Nutcracker_Vector_multiply(std::complex<double> const* c, NutcrackerVector const* x) {
    using namespace Nutcracker;
    return new NutcrackerVector((*c) * (*x));
}
//@+node:gcross.20110904235122.2869: *3* new
NutcrackerVector* Nutcracker_Vector_new(uint32_t physical_dimension, std::complex<double> const* data) {
    using namespace Nutcracker;
    VectorPtr vector = make_shared<Vector>(physical_dimension);
    std::copy(data,data+physical_dimension,vector->data().begin());
    return new NutcrackerVector(vector);
}
//@+node:gcross.20110904235122.2871: *3* newBasis
NutcrackerVector* Nutcracker_Vector_newBasis(uint32_t physical_dimension, uint32_t observation) { BEGIN_ERROR_REGION {
    return new NutcrackerVector(Nutcracker::basisVector(physical_dimension,observation));
} END_ERROR_REGION(NULL) }
//@-others

}
//@-leo
