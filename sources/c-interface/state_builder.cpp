#include <boost/range/adaptor/indirected.hpp>
#include <boost/make_shared.hpp>

#include "common.hpp"

#include "nutcracker/compiler.hpp"

using boost::adaptors::indirected;
using boost::make_shared;

extern "C" {

void Nutcracker_StateBuilder_addProductTerm(NutcrackerStateBuilder* builder, NutcrackerVector const* const* components) {
    builder->addProductTerm(boost::make_iterator_range(components,components+builder->numberOfSites()) | indirected);
}
void Nutcracker_StateBuilder_addTerm(NutcrackerStateBuilder* builder,NutcrackerStateTerm* term) { builder->addTerm(*term); }
NutcrackerState* Nutcracker_StateBuilder_compile(NutcrackerStateBuilder* builder) { BEGIN_ERROR_REGION {
    return new NutcrackerState(builder->compile());
} END_ERROR_REGION(NULL) }
NutcrackerState* Nutcracker_StateBuilder_compileCustomized(NutcrackerStateBuilder* builder,bool optimize) { BEGIN_ERROR_REGION {
    return new NutcrackerState(builder->compile(optimize));
} END_ERROR_REGION(NULL) }
uint32_t Nutcracker_StateBuilder_dimensionOfSite(NutcrackerStateBuilder const* builder, uint32_t site_number) { BEGIN_ERROR_REGION {
    return builder->dimensionOfSite(site_number);
} END_ERROR_REGION(0) }
void Nutcracker_StateBuilder_free(NutcrackerStateBuilder* builder) { delete builder; }
NutcrackerStateBuilder* Nutcracker_StateBuilder_new(uint32_t number_of_sites, uint32_t* dimensions) {
    return new NutcrackerStateBuilder(boost::make_iterator_range(dimensions,dimensions+number_of_sites));
}
NutcrackerStateBuilder* Nutcracker_StateBuilder_newSimple(uint32_t number_of_sites, uint32_t physical_dimension) {
    return new NutcrackerStateBuilder(number_of_sites,physical_dimension);
}
uint32_t Nutcracker_StateBuilder_numberOfSites(NutcrackerStateBuilder const* builder) { return builder->numberOfSites(); }

}
