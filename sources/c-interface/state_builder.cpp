//@+leo-ver=5-thin
//@+node:gcross.20110904235122.2855: * @file state_builder.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110904235122.2856: ** << License >>
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
//@+node:gcross.20110904235122.2864: *3* free
void Nutcracker_StateBuilder_free(NutcrackerStateBuilder* builder) { delete builder; }
//@+node:gcross.20110905151655.2828: *3* new
NutcrackerStateBuilder* Nutcracker_StateBuilder_new(unsigned int number_of_sites, uint32_t* dimensions) {
    return new NutcrackerStateBuilder(boost::make_iterator_range(dimensions,dimensions+number_of_sites));
}
//@+node:gcross.20110905151655.2830: *3* newSimple
NutcrackerStateBuilder* Nutcracker_StateBuilder_newSimple(unsigned int number_of_sites, unsigned int physical_dimension) {
    return new NutcrackerStateBuilder(number_of_sites,physical_dimension);
}
//@+node:gcross.20110904235122.2866: *3* numberOfSites
unsigned int Nutcracker_StateBuilder_numberOfSites(NutcrackerStateBuilder const* builder) { return builder->numberOfSites(); }
//@-others

}
//@-leo
