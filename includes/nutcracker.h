//@+leo-ver=5-thin
//@+node:gcross.20110902235008.2736: * @file nutcracker.h
//@@language cplusplus
#ifndef NUTCRACKER_H
#define NUTCRACKER_H

//@+<< Includes >>
//@+node:gcross.20110902235008.2738: ** << Includes >>
#ifdef __cplusplus
#include <complex>
#else
#include <comlex.h>
#include <stdbool.h>
#endif
//@-<< Includes >>

#ifdef __cplusplus
extern "C" {
#endif

//@+others
//@+node:gcross.20110904235122.2788: ** Types
typedef struct NutcrackerMatrix NutcrackerMatrix;
typedef struct NutcrackerOperator NutcrackerOperator;
typedef struct NutcrackerOperatorBuilder NutcrackerOperatorBuilder;
typedef struct NutcrackerOperatorTerm NutcrackerOperatorTerm;
typedef struct NutcrackerSerialization NutcrackerSerialization;
typedef struct NutcrackerState NutcrackerState;
typedef struct NutcrackerStateBuilder NutcrackerStateBuilder;
typedef struct NutcrackerStateTerm NutcrackerStateTerm;
typedef struct NutcrackerVector NutcrackerVector;
//@+node:gcross.20110904235122.2789: ** Functions
//@+node:gcross.20110902235008.2749: *3* Errors
void Nutcracker_clearError();
char const* Nutcracker_getError();
void Nutcracker_setError(char const* message);
//@+node:gcross.20110904235122.2772: *3* Matrix
void Nutcracker_Matrix_free(NutcrackerMatrix* matrix);

extern NutcrackerMatrix const
     *Nutcracker_Matrix_Pauli_I
   , *Nutcracker_Matrix_Pauli_X
   , *Nutcracker_Matrix_Pauli_Y
   , *Nutcracker_Matrix_Pauli_Z
   ;

NutcrackerMatrix const* Nutcracker_Matrix_getPauliI();
NutcrackerMatrix const* Nutcracker_Matrix_getPauliX();
NutcrackerMatrix const* Nutcracker_Matrix_getPauliY();
NutcrackerMatrix const* Nutcracker_Matrix_getPauliZ();

#ifdef __cplusplus
NutcrackerMatrix* Nutcracker_Matrix_new(uint32_t dimension, std::complex<double> const* data);
#else
NutcrackerMatrix* Nutcracker_Matrix_new(uint32_t dimension, compex double const* data);
#endif

#ifdef __cplusplus
NutcrackerMatrix* Nutcracker_Matrix_newDiagonal(uint32_t dimension, std::complex<double> const* data);
#else
NutcrackerMatrix* Nutcracker_Matrix_newDiagonal(uint32_t dimension, compex double const* data);
#endif

NutcrackerMatrix* Nutcracker_Matrix_add(NutcrackerMatrix const* x, NutcrackerMatrix const* y);

#ifdef __cplusplus
NutcrackerMatrix* Nutcracker_Matrix_multiply(std::complex<double> const* c, NutcrackerMatrix const* x);
#else
NutcrackerMatrix* Nutcracker_Matrix_multiply(complex double const* x, NutcrackerMatrix const* y);
#endif

uint32_t Nutcracker_Matrix_getSize(NutcrackerMatrix const* x);

#ifdef __cplusplus
void Nutcracker_Matrix_getElementAtCoordinate(NutcrackerMatrix const* x, uint32_t i, uint32_t j, std::complex<double>* element);
#else
void Nutcracker_Matrix_getElementAtCoordinate(NutcrackerMatrix const* x, uint32_t i, uint32_t j, complex double* element);
#endif
//@+node:gcross.20110823131135.2552: *3* Operator
void Nutcracker_Operator_free(NutcrackerOperator* op);

void Nutcracker_Operator_simpleSolveForLeastEigenvalues(NutcrackerOperator const* op, uint32_t number_of_levels, float* eigenvalues);
void Nutcracker_Operator_simpleSolveForLeastEigenvaluesWithEigenvectors(NutcrackerOperator const* op, uint32_t number_of_levels, float* eigenvalues, NutcrackerState** eigenvectors);

NutcrackerSerialization* Nutcracker_Operator_serialize(NutcrackerOperator const* op);
NutcrackerOperator* Nutcracker_Operator_deserialize(uint32_t size, void const* buffer);
//@+node:gcross.20110823131135.2559: *3* OperatorBuilder
NutcrackerOperatorBuilder* Nutcracker_OperatorBuilder_new(uint32_t number_of_sites, uint32_t* dimensions);
NutcrackerOperatorBuilder* Nutcracker_OperatorBuilder_newSimple(uint32_t number_of_sites, uint32_t physical_dimension);

void Nutcracker_OperatorBuilder_free(NutcrackerOperatorBuilder* builder);

uint32_t Nutcracker_OperatorBuilder_numberOfSites(NutcrackerOperatorBuilder const* builder);
uint32_t Nutcracker_OperatorBuilder_dimensionOfSite(NutcrackerOperatorBuilder const* builder, uint32_t site_number);

void Nutcracker_OperatorBuilder_addProductTerm(NutcrackerOperatorBuilder* builder, NutcrackerMatrix const* const* components);
void Nutcracker_OperatorBuilder_addTerm(NutcrackerOperatorBuilder* builder, NutcrackerOperatorTerm* term);

NutcrackerOperator* Nutcracker_OperatorBuilder_compile(NutcrackerOperatorBuilder* builder);
NutcrackerOperator* Nutcracker_OperatorBuilder_compileCustomized(NutcrackerOperatorBuilder* builder, bool optimize, bool add_start_and_end_loops);
//@+node:gcross.20110906155043.4827: *3* OperatorTerm
void Nutcracker_OperatorTerm_free(NutcrackerOperatorTerm* op);

NutcrackerOperatorTerm* Nutcracker_OperatorTerm_add(NutcrackerOperatorTerm const* x, NutcrackerOperatorTerm const* y);

#ifdef __cplusplus
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_multiply(std::complex<double> const* c, NutcrackerOperatorTerm const* x);
#else
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_multiply(complex double const* x, NutcrackerOperatorTerm const* y);
#endif

NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_LocalExternalField(uint32_t site_number, NutcrackerMatrix const* field_matrix);
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_GlobalExternalField(NutcrackerMatrix const* field_matrix);

NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_LocalNeighborCouplingField(uint32_t site_number, NutcrackerMatrix const* left_field_matrix, NutcrackerMatrix const* right_field_matrix);
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_GlobalNeighborCouplingField(NutcrackerMatrix const* left_field_matrix, NutcrackerMatrix const* right_field_matrix);

NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_TransverseIsingField(NutcrackerMatrix const* external_field_matrix, NutcrackerMatrix const* left_coupling_field_matrix, NutcrackerMatrix const* right_coupling_field_matrix);
//@+node:gcross.20110908221100.3003: *3* Serialization
void Nutcracker_Serialization_free(NutcrackerSerialization* s);

uint32_t Nutcracker_Serialization_getSize(NutcrackerSerialization* s);
void Nutcracker_Serialization_write(NutcrackerSerialization* s, void* buffer);
//@+node:gcross.20110904235122.2810: *3* State
void Nutcracker_State_free(NutcrackerState* state);

#ifdef __cplusplus
void Nutcracker_State_computeOverlap(NutcrackerState const* state1, NutcrackerState const* state2, std::complex<double>* result);
void Nutcracker_State_computeExpectation(NutcrackerState const* state, NutcrackerOperator const* op, std::complex<double>* result);
#else
void Nutcracker_State_computeOverlap(NutcrackerState const* state1, NutcrackerState const* state2, complex double* result);
void Nutcracker_State_computeExpectation(NutcrackerState const* state, NutcrackerOperator const* op, complex double* result);
#endif

NutcrackerSerialization* Nutcracker_State_serialize(NutcrackerState const* op);
NutcrackerState* Nutcracker_State_deserialize(uint32_t size, void const* buffer);
//@+node:gcross.20110904235122.2838: *3* StateBuilder
NutcrackerStateBuilder* Nutcracker_StateBuilder_new(uint32_t number_of_sites, uint32_t* dimensions);
NutcrackerStateBuilder* Nutcracker_StateBuilder_newSimple(uint32_t number_of_sites, uint32_t physical_dimension);

void Nutcracker_StateBuilder_free(NutcrackerStateBuilder* builder);

uint32_t Nutcracker_StateBuilder_numberOfSites(NutcrackerStateBuilder const* builder);
uint32_t Nutcracker_StateBuilder_dimensionOfSite(NutcrackerStateBuilder const* builder, uint32_t site_number);

void Nutcracker_StateBuilder_addProductTerm(NutcrackerStateBuilder* builder, NutcrackerVector const* const* components);
void Nutcracker_StateBuilder_addTerm(NutcrackerStateBuilder* builder, NutcrackerStateTerm* term);

NutcrackerState* Nutcracker_StateBuilder_compile(NutcrackerStateBuilder* builder);
NutcrackerState* Nutcracker_StateBuilder_compileCustomized(NutcrackerStateBuilder* builder, bool optimize);
//@+node:gcross.20110908221100.3047: *3* StateTerm
void Nutcracker_StateTerm_free(NutcrackerStateTerm* op);

NutcrackerStateTerm* Nutcracker_StateTerm_add(NutcrackerStateTerm const* x, NutcrackerStateTerm const* y);

#ifdef __cplusplus
NutcrackerStateTerm* Nutcracker_StateTerm_multiply(std::complex<double> const* c, NutcrackerStateTerm const* x);
#else
NutcrackerStateTerm* Nutcracker_StateTerm_multiply(complex double const* x, NutcrackerStateTerm const* y);
#endif

NutcrackerStateTerm* Nutcracker_StateTerm_create_ProductWithOneSiteDifferent(uint32_t site_number, NutcrackerVector const* common_observation, NutcrackerVector const* special_observation);
NutcrackerStateTerm* Nutcracker_StateTerm_create_W(NutcrackerVector const* common_observation, NutcrackerVector const* special_observation, bool normalized);
//@+node:gcross.20110904235122.2836: *3* Vector
void Nutcracker_Vector_free(NutcrackerVector* Vector);

extern NutcrackerVector const
     *Nutcracker_Vector_Qubit_Up
   , *Nutcracker_Vector_Qubit_Down
   ;

#ifdef __cplusplus
NutcrackerVector* Nutcracker_Vector_new(uint32_t physical_dimension, std::complex<double> const* data);
#else
NutcrackerVector* Nutcracker_Vector_new(uint32_t physical_dimension, complex double const* data);
#endif

NutcrackerVector* Nutcracker_Vector_newBasis(uint32_t physical_dimension, uint32_t observed_value);

NutcrackerVector* Nutcracker_Vector_add(NutcrackerVector const* x, NutcrackerVector const* y);

#ifdef __cplusplus
NutcrackerVector* Nutcracker_Vector_multiply(std::complex<double> const* c, NutcrackerVector const* x);
#else
NutcrackerVector* Nutcracker_Vector_multiply(complex double const* x, NutcrackerVector const* y);
#endif

uint32_t Nutcracker_Vector_getSize(NutcrackerVector const* x);

#ifdef __cplusplus
void Nutcracker_Vector_getElementAtIndex(NutcrackerVector const* x, uint32_t index, std::complex<double>* element);
#else
void Nutcracker_Vector_getElementAtIndex(NutcrackerVector const* x, uint32_t index, complex double* element);
#endif
//@+node:gcross.20110910181738.4758: *3* Version
uint32_t Nutcracker_Version_getComponent(uint32_t index);
uint32_t Nutcracker_Version_getSize();
char const* Nutcracker_Version_getString();
void Nutcracker_Version_write(uint32_t* version);
//@-others

#ifdef __cplusplus
}
#endif

#endif
//@-leo
