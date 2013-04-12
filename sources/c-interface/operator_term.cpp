#include "terms.hpp"

#include "nutcracker/compiler.hpp"


typedef NutcrackerSumTerm<NutcrackerOperatorTerm> NutcrackerOperatorSumTerm;
template<typename Term> struct NutcrackerOperatorTermWrapper : public NutcrackerTermWrapper<NutcrackerOperatorTerm,Term> {
    typedef NutcrackerTermWrapper<NutcrackerOperatorTerm,Term>  Base;
    template<typename A> NutcrackerOperatorTermWrapper(A const& a) : Base(a) {}
    template<typename A, typename B> NutcrackerOperatorTermWrapper(A const& a, B const& b) : Base(a,b) {}
    template<typename A, typename B, typename C> NutcrackerOperatorTermWrapper(A const& a, B const& b, C const& c) : Base(a,b,c) {}
    template<typename A, typename B, typename C, typename D> NutcrackerOperatorTermWrapper(A const& a, B const& b, C const& c, D const& d) : Base(a,b,c,d) {}
};

extern "C" {

NutcrackerOperatorTerm* Nutcracker_OperatorTerm_add(NutcrackerOperatorTerm const* x, NutcrackerOperatorTerm const* y) {
    return new NutcrackerOperatorSumTerm(x,y);
}
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_GlobalExternalField(NutcrackerMatrix const* field_matrix) {
    return new NutcrackerOperatorTermWrapper<Nutcracker::GlobalExternalField>(*field_matrix);
}
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_GlobalNeighborCouplingField(NutcrackerMatrix const* left_field_matrix, NutcrackerMatrix const* right_field_matrix) {
    return new NutcrackerOperatorTermWrapper<Nutcracker::GlobalNeighborCouplingField>(*left_field_matrix,*right_field_matrix);
}
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_LocalExternalField(uint32_t site_number, NutcrackerMatrix const* field_matrix) {
    return new NutcrackerOperatorTermWrapper<Nutcracker::LocalExternalField>(site_number,*field_matrix);
}
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_LocalNeighborCouplingField(uint32_t site_number, NutcrackerMatrix const* left_field_matrix, NutcrackerMatrix const* right_field_matrix) {
    return new NutcrackerOperatorTermWrapper<Nutcracker::LocalNeighborCouplingField>(site_number,*left_field_matrix,*right_field_matrix);
}
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_create_TransverseIsingField(NutcrackerMatrix const* external_field_matrix, NutcrackerMatrix const* left_coupling_field_matrix, NutcrackerMatrix const* right_coupling_field_matrix) {
    return new NutcrackerOperatorTermWrapper<Nutcracker::TransverseIsingField>(*external_field_matrix,*left_coupling_field_matrix,*right_coupling_field_matrix);
}
void Nutcracker_OperatorTerm_free(NutcrackerOperatorTerm* op) {
    delete op;
}
NutcrackerOperatorTerm* Nutcracker_OperatorTerm_multiply(std::complex<double> const* c, NutcrackerOperatorTerm const* x) {
    return x->copyAndMultiplyBy(*c);
}

}
