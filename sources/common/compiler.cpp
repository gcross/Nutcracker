//@+leo-ver=5-thin
//@+node:gcross.20110805222031.2351: * @file compiler.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110805222031.2352: ** << License >>
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
//@+node:gcross.20110805222031.2353: ** << Includes >>
#include <boost/container/set.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/make_shared.hpp>
#include <boost/range/algorithm/for_each.hpp>
#include <boost/range/algorithm/fill.hpp>
#include <boost/range/algorithm/sort.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/smart_ptr.hpp>
#include <boost/unordered_set.hpp>

#include "compiler.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110805222031.2354: ** << Usings >>
using boost::adaptors::map_keys;
using boost::container::map;
using boost::container::set;
using boost::fill;
namespace lambda = boost::lambda;
using boost::make_shared;
using boost::make_tuple;
using boost::irange;
using boost::scoped_array;
using boost::sort;
using boost::transform;
using boost::unordered_set;

using std::iterator_traits;
using std::make_pair;
//@-<< Usings >>

//@+others
//@+node:gcross.20110805222031.2355: ** Classes
//@+node:gcross.20110805222031.2356: *3* OperatorBuilder
//@+node:gcross.20110817110920.2479: *4* (constructors)
OperatorBuilder::OperatorBuilder() {}

OperatorBuilder::OperatorBuilder(BOOST_RV_REF(OperatorBuilder) other)
  : MatrixTable(static_cast<BOOST_RV_REF(MatrixTable)>(other))
  , SignalTable(other)
  , connections(boost::move(other.connections))
{}
//@+node:gcross.20110822214054.2515: *4* add(X)ExternalField
OperatorBuilder& OperatorBuilder::addLocalExternalField(unsigned int site_number, unsigned int field_matrix_id, complex<double> scale_factor) {
    return connect(site_number,getStartSignal(),getEndSignal(),field_matrix_id,scale_factor);
}

OperatorBuilder& OperatorBuilder::addGlobalExternalField(unsigned int field_matrix_id, complex<double> scale_factor) {
    BOOST_FOREACH(unsigned int const site_number, irange(0u,numberOfSites())) {
        addLocalExternalField(site_number,field_matrix_id,scale_factor);
    }
    return *this;
}
//@+node:gcross.20110822214054.2520: *4* add(X)NeighborCouplingField
OperatorBuilder& OperatorBuilder::addLocalNeighborCouplingField(unsigned int left_site_number, unsigned int left_field_matrix_id, unsigned int right_field_matrix_id, complex<double> scale_factor) {
    unsigned int const signal = allocateSignal();
    connect(left_site_number,getStartSignal(),signal,left_field_matrix_id,scale_factor);
    return connect(left_site_number+1,signal,getEndSignal(),right_field_matrix_id);
}

OperatorBuilder& OperatorBuilder::addGlobalNeighborCouplingField(unsigned int left_field_matrix_id, unsigned int right_field_matrix_id, complex<double> scale_factor) {
    unsigned int const signal = allocateSignal();
    BOOST_FOREACH(unsigned int const site_number, irange(0u,numberOfSites())) {
        connect(site_number,getStartSignal(),signal,left_field_matrix_id,scale_factor);
        connect(site_number,signal,getEndSignal(),right_field_matrix_id);
    }
    return *this;
}
//@+node:gcross.20110805222031.2380: *4* addSite(s)
OperatorBuilder& OperatorBuilder::addSite(unsigned int dimension) {
    sites.push_back(dimension);
    connections.emplace_back();
    return *this;
}

OperatorBuilder& OperatorBuilder::addSites(unsigned int number_of_sites, PhysicalDimension dimension) {
    REPEAT(number_of_sites) { addSite(*dimension); }
    return *this;
}
//@+node:gcross.20110826085250.2531: *4* addTerm
OperatorBuilder& OperatorBuilder::addTerm(vector<unsigned int> const& components) {
    if(components.size() != numberOfSites()) throw WrongNumberOfSites(components.size(),numberOfSites());
    unsigned int const signal = allocateSignal();
    connect(0u,getStartSignal(),signal,components[0u]);
    BOOST_FOREACH(unsigned int const site_number, irange(1u,numberOfSites()-1)) {
        connect(site_number,signal,signal,components[site_number]);
    }
    connect(numberOfSites()-1,signal,getEndSignal(),components[numberOfSites()-1]);
    return *this;
}
//@+node:gcross.20110814140556.2427: *4* compile
Operator OperatorBuilder::compile(bool optimize, bool add_start_and_end_loops) {
    OperatorSpecification source(generateSpecification(add_start_and_end_loops));
    if(optimize) source.optimize();
    return source.compile();
}
//@+node:gcross.20110805222031.2385: *4* connect
OperatorBuilder& OperatorBuilder::connect(
    unsigned int const site_number,
    unsigned int const left_signal,
    unsigned int const right_signal,
    unsigned int const matrix_id,
    complex<double> const scale_factor
) {
    if(site_number >= sites.size()) throw SiteNumberTooLarge(site_number,sites.size());
    if(getSizeOf(matrix_id) != sites[site_number])
        throw BadMatrixDimensionForSite(site_number,sites[site_number],matrix_id,getSizeOf(matrix_id));
    UnmergedSiteConnections& site_connections = connections[site_number];
    UnmergedSiteConnections::key_type key(left_signal,right_signal,matrix_id);
    UnmergedSiteConnections::iterator iter = site_connections.find(key);
    if(iter != site_connections.end()) {
        iter->second += scale_factor;
    } else {
        site_connections[key] = scale_factor;
    }
    return *this;
}
//@+node:gcross.20110805222031.4630: *4* generateSpecification
OperatorSpecification OperatorBuilder::generateSpecification(bool add_start_and_end_loops) {
    OperatorSpecification specification;
    specification.reserveSignalsBelow(next_free_signal);
    unsigned int site_number = 0u;
    BOOST_FOREACH(UnmergedSiteConnections const& site_connections,connections) {
        typedef unordered_map<pair<unsigned int,unsigned int>, vector<pair<unsigned int,complex<double> > > > MatricesAndFactors;
        MatricesAndFactors matrices_and_factors;
        BOOST_FOREACH(UnmergedSiteConnections::const_reference p, site_connections) {
            matrices_and_factors[make_pair(p.first.get<0>(),p.first.get<1>())].push_back(make_pair(p.first.get<2>(),p.second));
        }
        BOOST_FOREACH(MatricesAndFactors::const_reference p, matrices_and_factors) {
            specification.connect(site_number,p.first.first,p.first.second,specification.lookupIdFromTable(*this,lookupIdOfWeightedSum(p.second)));
        }
        ++site_number;
    }
    if(add_start_and_end_loops) {
        BOOST_FOREACH(unsigned int const site_number, irange((size_t)0u,connections.size())) {
            specification.connect(site_number,specification.getStartSignal(),specification.getStartSignal(),specification.lookupIdOfIdentityWithDimension(sites[site_number]));
            specification.connect(site_number,specification.getEndSignal(),  specification.getEndSignal(),  specification.lookupIdOfIdentityWithDimension(sites[site_number]));
        }
    }
    return boost::move(specification);
}
//@+node:gcross.20110817110920.2483: *4* operator=
OperatorBuilder& OperatorBuilder::operator=(BOOST_COPY_ASSIGN_REF(OperatorBuilder) other)
{
    if (this != &other){
        MatrixTable::operator=(other);
        SignalTable::operator=(other);
        connections = other.connections;
    }
    return *this;
}

OperatorBuilder& OperatorBuilder::operator=(BOOST_RV_REF(OperatorBuilder) other)
{
    if (this != &other){
        MatrixTable::operator=(static_cast<BOOST_RV_REF(MatrixTable)>(other));
        SignalTable::operator=(other);
        connections = boost::move(other.connections);
    }
    return *this;
}
//@+node:gcross.20110814140556.2438: *3* OperatorSpecification
//@+node:gcross.20110814140556.2445: *4* compile
Operator OperatorSpecification::compile() const {
    Operator op;
    typedef map<vector<OperatorLink>,shared_ptr<OperatorSite> > OperatorSites;
    OperatorSites operator_sites;
    map<unsigned int,unsigned int> left_signals_map;
    left_signals_map[1] = 1;
    unsigned int site_number = 0;
    BOOST_FOREACH(SiteConnections const& site_connections, connections) {
        set<unsigned int> left_signals, right_signals;
        BOOST_FOREACH(SiteConnections::key_type signal, site_connections | map_keys) {
            left_signals.insert(signal.first);
            right_signals.insert(signal.second);
        }
        if(!boost::equal(left_signals_map | map_keys,left_signals)) {
            throw NeighborSignalConflict(site_number,left_signals_map | map_keys,left_signals);
        }
        map<unsigned int,unsigned int> right_signals_map;
        unsigned int next_index = 1;
        BOOST_FOREACH(unsigned int right_signal, right_signals) {
            right_signals_map[right_signal] = next_index++;
        }
        vector<OperatorLink> links;
        links.reserve(site_connections.size());
        BOOST_FOREACH(SiteConnections::const_reference p, site_connections) {
            links.emplace_back(left_signals_map[p.first.first],right_signals_map[p.first.second],get(p.second));
        }
        OperatorSites::iterator iter = operator_sites.find(links);
        shared_ptr<OperatorSite> operator_site;
        if(iter != operator_sites.end()) {
            operator_site = iter->second;
        } else {
            operator_site.reset(new OperatorSite(
                constructOperatorSite(
                    PhysicalDimension(links[0].matrix->size1()),
                    LeftDimension(left_signals.size()),
                    RightDimension(right_signals.size()),
                    links
                )
            ));
            operator_sites[links] = operator_site;
        }
        op.emplace_back(operator_site);
        ++site_number;
        left_signals_map = boost::move(right_signals_map);
    }
    static vector<unsigned int> left_signals = list_of(2u);
    if(!boost::equal(left_signals_map | map_keys,left_signals)) {
        throw NeighborSignalConflict(site_number,left_signals_map | map_keys,left_signals);
    }
    return boost::move(op);
}
//@+node:gcross.20110805222031.4644: *3* SignalTable
//@+node:gcross.20110805222031.4645: *4* (constructors)
SignalTable::SignalTable()
  : next_free_signal(3)
{}
//@+node:gcross.20110805222031.2378: *4* allocateSignal
unsigned int SignalTable::allocateSignal() {
    return next_free_signal++;
}
//@+node:gcross.20110817110920.2484: *4* reserveSignalsBelow
void SignalTable::reserveSignalsBelow(unsigned int exclusive_upper_bound) {
    next_free_signal = std::max(next_free_signal,exclusive_upper_bound);
}
//@-others

}
//@-leo
