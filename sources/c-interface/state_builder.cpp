//@+leo-ver=5-thin
//@+node:gcross.20110904235122.2855: * @file state_builder.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110904235122.2857: ** << Includes >>
#include <boost/range/adaptor/indirected.hpp>
#include <boost/make_shared.hpp>

#include "common.hpp"

#include "nutcracker/compiler.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110904235122.2858: ** << Usings >>
using boost::adaptors::indirected;
using boost::make_shared;
//@-<< Usings >>

extern "C" {

//@+others
//@+node:gcross.20110904235122.2859: ** Functions
//@+node:gcross.20110904235122.2874: *3* addProductTerm
void Nutcracker_StateBuilder_addProductTerm(NutcrackerStateBuilder* builder, NutcrackerVector const* const* components) {
    builder->addProductTerm(boost::make_iterator_range(components,components+builder->numberOfSites()) | indirected);
}
//@+node:gcross.20110904235122.2861: *3* addTerm
void Nutcracker_StateBuilder_addTerm(NutcrackerStateBuilder* builder,NutcrackerStateTerm* term) { builder->addTerm(*term); }
//@+node:gcross.20110904235122.2862: *3* compile
NutcrackerState* Nutcracker_StateBuilder_compile(NutcrackerStateBuilder* builder) { BEGIN_ERROR_REGION {
    return new NutcrackerState(builder->compile());
} END_ERROR_REGION(NULL) }
//@+node:gcross.20110904235122.2863: *3* compileCustomized
NutcrackerState* Nutcracker_StateBuilder_compileCustomized(NutcrackerStateBuilder* builder,bool optimize) { BEGIN_ERROR_REGION {
    return new NutcrackerState(builder->compile(optimize));
} END_ERROR_REGION(NULL) }
//@+node:gcross.20110906130654.2920: *3* dimensionOfSite
uint32_t Nutcracker_StateBuilder_dimensionOfSite(NutcrackerStateBuilder const* builder, uint32_t site_number) { BEGIN_ERROR_REGION {
    return builder->dimensionOfSite(site_number);
} END_ERROR_REGION(0) }
//@+node:gcross.20110904235122.2864: *3* free
void Nutcracker_StateBuilder_free(NutcrackerStateBuilder* builder) { delete builder; }
//@+node:gcross.20110905151655.2828: *3* new
NutcrackerStateBuilder* Nutcracker_StateBuilder_new(uint32_t number_of_sites, uint32_t* dimensions) {
    return new NutcrackerStateBuilder(boost::make_iterator_range(dimensions,dimensions+number_of_sites));
}
//@+node:gcross.20110905151655.2830: *3* newSimple
NutcrackerStateBuilder* Nutcracker_StateBuilder_newSimple(uint32_t number_of_sites, uint32_t physical_dimension) {
    return new NutcrackerStateBuilder(number_of_sites,physical_dimension);
}
//@+node:gcross.20110904235122.2866: *3* numberOfSites
uint32_t Nutcracker_StateBuilder_numberOfSites(NutcrackerStateBuilder const* builder) { return builder->numberOfSites(); }
//@-others

}
//@-leo
