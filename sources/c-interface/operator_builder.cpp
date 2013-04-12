#include <boost/range/adaptor/indirected.hpp>
#include <boost/make_shared.hpp>

#include "common.hpp"

#include "nutcracker/compiler.hpp"




#include <iostream>

using boost::adaptors::indirected;
using boost::make_shared;

extern "C" {

void Nutcracker_OperatorBuilder_addProductTerm(NutcrackerOperatorBuilder* builder, NutcrackerMatrix const* const* components) {
    builder->addProductTerm(boost::make_iterator_range(components,components+builder->numberOfSites()) | indirected);
}
void Nutcracker_OperatorBuilder_addTerm(NutcrackerOperatorBuilder* builder,NutcrackerOperatorTerm* term) { builder->addTerm(*term); }
NutcrackerOperator* Nutcracker_OperatorBuilder_compile(NutcrackerOperatorBuilder* builder) { BEGIN_ERROR_REGION {
    return new NutcrackerOperator(builder->compile());
} END_ERROR_REGION(NULL) }
NutcrackerOperator* Nutcracker_OperatorBuilder_compileCustomized(NutcrackerOperatorBuilder* builder,bool optimize,bool add_start_and_end_loops) { BEGIN_ERROR_REGION {
    return new NutcrackerOperator(builder->compile(optimize,add_start_and_end_loops));
} END_ERROR_REGION(NULL) }
uint32_t Nutcracker_OperatorBuilder_dimensionOfSite(NutcrackerOperatorBuilder const* builder, uint32_t site_number) { BEGIN_ERROR_REGION {
    return builder->dimensionOfSite(site_number);
} END_ERROR_REGION(0) }
void Nutcracker_OperatorBuilder_free(NutcrackerOperatorBuilder* builder) { delete builder; }
NutcrackerOperatorBuilder* Nutcracker_OperatorBuilder_new(uint32_t number_of_sites, uint32_t* dimensions) {
    return new NutcrackerOperatorBuilder(boost::make_iterator_range(dimensions,dimensions+number_of_sites));
}
NutcrackerOperatorBuilder* Nutcracker_OperatorBuilder_newSimple(uint32_t number_of_sites, uint32_t physical_dimension) {
    return new NutcrackerOperatorBuilder(number_of_sites,physical_dimension);
}
uint32_t Nutcracker_OperatorBuilder_numberOfSites(NutcrackerOperatorBuilder const* builder) { return builder->numberOfSites(); }

}
