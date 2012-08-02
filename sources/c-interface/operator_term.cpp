//@+leo-ver=5-thin
//@+node:gcross.20110906155043.4833: * @file operator_term.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110906155043.4835: ** << Includes >>
#include "terms.hpp"

#include "nutcracker/compiler.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110906155043.4836: ** << Usings >>
//@-<< Usings >>

//@+<< Classes >>
//@+node:gcross.20110906155043.4974: ** << Classes >>
//@+others
//@+node:gcross.20110906155043.4978: *3* NutcrackerOperatorSumTerm
typedef NutcrackerSumTerm<NutcrackerOperatorTerm> NutcrackerOperatorSumTerm;
//@+node:gcross.20110906155043.4977: *3* NutcrackerOperatorTermWrapper
template<typename Term> struct NutcrackerOperatorTermWrapper : public NutcrackerTermWrapper<NutcrackerOperatorTerm,Term> {
    typedef NutcrackerTermWrapper<NutcrackerOperatorTerm,Term>  Base;
    template<typename A> NutcrackerOperatorTermWrapper(A const& a) : Base(a) {}
    template<typename A, typename B> NutcrackerOperatorTermWrapper(A const& a, B const& b) : Base(a,b) {}
    template<typename A, typename B, typename C> NutcrackerOperatorTermWrapper(A const& a, B const& b, C const& c) : Base(a,b,c) {}
    template<typename A, typename B, typename C, typename D> NutcrackerOperatorTermWrapper(A const& a, B const& b, C const& c, D const& d) : Base(a,b,c,d) {}
};
//@-others
//@-<< Classes >>

extern "C" {

//@+others
//@+node:gcross.20110906155043.4837: ** Functions
//@+node:gcross.20110906155043.4979: *3* add
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_add(NutcrackerOperatorTerm const* x, NutcrackerOperatorTerm const* y) {
    return new NutcrackerOperatorSumTerm(x,y);
}
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
