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
#include <boost/range/adaptor/map.hpp>
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
//@+node:gcross.20110805222031.4640: ** Helper Functions
//@+node:gcross.20110805222031.4652: *3* XSignalOf
static inline unsigned int& incomingSignalOf(bool forward,pair<unsigned int,unsigned int>& key) {
    return forward ? key.first : key.second;
}

static inline unsigned int& outgoingSignalOf(bool forward,pair<unsigned int,unsigned int>& key) {
    return forward ? key.second : key.first;
}

static inline unsigned int const& incomingSignalOf(bool forward,pair<unsigned int,unsigned int> const& key) {
    return forward ? key.first : key.second;
}

static inline unsigned int const& outgoingSignalOf(bool forward,pair<unsigned int,unsigned int> const& key) {
    return forward ? key.second : key.first;
}
//@+node:gcross.20110805222031.4653: *3* newSignalPair
static inline pair<unsigned int,unsigned int> newSignalPair(bool forward,unsigned int incoming_signal, unsigned int outgoing_signal) {
    return forward ? make_pair(incoming_signal,outgoing_signal) : make_pair(outgoing_signal,incoming_signal);
}
//@+node:gcross.20110805222031.2355: ** Classes
//@+node:gcross.20110805222031.4635: *3* MatrixTable
//@+node:gcross.20110805222031.2373: *4* Constructors
MatrixTable::MatrixTable() {
    matrices.emplace_back();
    lookupIdentityMatrixId(2);
    lookupMatrixId(Pauli::X);
    lookupMatrixId(Pauli::Y);
    lookupMatrixId(Pauli::Z);
}

MatrixTable::MatrixTable(BOOST_RV_REF(MatrixTable) other)
  : identity_lookup_table(boost::move(other.identity_lookup_table))
  , matrices(boost::move(other.matrices))
  , matrix_lookup_table(boost::move(other.matrix_lookup_table))
{}
//@+node:gcross.20110805222031.2372: *4* Methods
//@+node:gcross.20110805222031.2357: *5* lookupIdentityMatrixId
unsigned int MatrixTable::lookupIdentityMatrixId(unsigned int dimension) {
    map<unsigned int,unsigned int>::const_iterator const iter = identity_lookup_table.find(dimension);
    if(iter != identity_lookup_table.end()) {
        return iter->second;
    } else {
        MatrixConstPtr const matrix = identityMatrix(dimension);
        unsigned int matrix_id = lookupMatrixId(matrix);
        identity_lookup_table[dimension] = matrix_id;
        return matrix_id;
    }
}
//@+node:gcross.20110805222031.2358: *5* lookupMatrixId
unsigned int MatrixTable::lookupMatrixId(shared_ptr<Matrix const> const& matrix) {
    if(matrix->size1() != matrix->size2()) throw NonSquareMatrix(matrix->size1(),matrix->size2());
    if(matrix->size1() == 0) return 0;
    BOOST_FOREACH(complex<double> const& datum, matrix->data()) {
        if(abs(datum) >= 1e-14) goto notnull;
    }
    return 0;
notnull:
    MatrixLookupTable::const_iterator const iter = matrix_lookup_table.find(matrix);
    if(iter != matrix_lookup_table.end()) {
        return iter->second;
    } else {
        unsigned int matrix_id = matrices.size();
        matrix_lookup_table[matrix] = matrix_id;
        matrices.emplace_back(matrix);
        return matrix_id;
    }
}
//@+node:gcross.20110805222031.4639: *5* lookupMatrixIdFromTable
unsigned int MatrixTable::lookupMatrixIdFromTable(MatrixTable const& other, unsigned int matrix_id_in_other) {
    return lookupMatrixId(other.getMatrixPtr(matrix_id_in_other));
}
//@+node:gcross.20110805222031.4655: *5* lookupSumOfMatrices
unsigned int MatrixTable::lookupSumOfMatrices(vector<unsigned int> const& matrix_ids) {
    if(matrix_ids.size() == 0) return 0;
    if(matrix_ids.size() == 1) return matrix_ids[0];
    MatrixPtr matrix(new Matrix(getMatrix(matrix_ids[0])));
    boost::for_each(
        make_iterator_range(matrix_ids.begin()+1,matrix_ids.end()),
        lambda::var(*matrix) += lambda::bind(&MatrixTable::getMatrix,this,lambda::_1)
    );
    return lookupMatrixId(matrix);
}
//@+node:gcross.20110805222031.4719: *5* lookupWeightedSumOfMatrices
unsigned int MatrixTable::lookupWeightedSumOfMatrices(vector<pair<unsigned int,complex<double> > > const& matrix_ids_and_scale_factors) {
    if(matrix_ids_and_scale_factors.size() == 0) return 0;
    if(matrix_ids_and_scale_factors.size() == 1 && matrix_ids_and_scale_factors.front().second == c(1,0))
        return matrix_ids_and_scale_factors.front().first;
    unsigned int dimension = getMatrix(matrix_ids_and_scale_factors.front().first).size1();
    shared_ptr<Matrix> matrix(make_shared<Matrix>(dimension,dimension,c(0,0)));
    typedef pair<unsigned int,complex<double> > P;
    typedef vector<P> T;
    BOOST_FOREACH(P const& p,matrix_ids_and_scale_factors) {
        (*matrix) += p.second*getMatrix(p.first);
    }
    return lookupMatrixId(matrix);
}
//@+node:gcross.20110805222031.2370: *5* lookupMatrix
Matrix const& MatrixTable::getMatrix(unsigned int matrix_id) const {
    return *getMatrixPtr(matrix_id);
}
//@+node:gcross.20110815001337.2469: *5* lookupMatrixPtr
shared_ptr<Matrix const> const& MatrixTable::getMatrixPtr(unsigned int matrix_id) const {
    return matrices[matrix_id];
}
//@+node:gcross.20110814140556.2432: *4* Operators
//@+node:gcross.20110814140556.2433: *5* operator=
MatrixTable& MatrixTable::operator=(BOOST_COPY_ASSIGN_REF(MatrixTable) other)
{
    if (this != &other){
        identity_lookup_table = other.identity_lookup_table;
        matrices = other.matrices;
        matrix_lookup_table = other.matrix_lookup_table;
    }
    return *this;
}

MatrixTable& MatrixTable::operator=(BOOST_RV_REF(MatrixTable) other)
{
    if (this != &other){
        identity_lookup_table = boost::move(other.identity_lookup_table);
        matrices = boost::move(other.matrices);
        matrix_lookup_table = boost::move(other.matrix_lookup_table);
    }
    return *this;
}
//@+node:gcross.20110805222031.2356: *3* OperatorBuilder
//@+node:gcross.20110817110920.2479: *4* (constructors)
OperatorBuilder::OperatorBuilder() {}

OperatorBuilder::OperatorBuilder(BOOST_RV_REF(OperatorBuilder) other)
  : MatrixTable(static_cast<BOOST_RV_REF(MatrixTable)>(other))
  , SignalTable(other)
  , connections(boost::move(other.connections))
{}
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
//@+node:gcross.20110822214054.2515: *4* add(X)ExternalField
OperatorBuilder& OperatorBuilder::addLocalExternalField(unsigned int site_number, MatrixConstPtr const& field_operator, complex<double> scale_factor) {
    return connect(site_number,getStartSignal(),getEndSignal(),lookupMatrixId(field_operator),scale_factor);
}

OperatorBuilder& OperatorBuilder::addGlobalExternalField(MatrixConstPtr const& field_operator, complex<double> scale_factor) {
    unsigned int const matrix_id = lookupMatrixId(field_operator);
    BOOST_FOREACH(unsigned int const site_number, irange(0u,numberOfSites())) {
        connect(site_number,getStartSignal(),getEndSignal(),matrix_id,scale_factor);
    }
    return *this;
}
//@+node:gcross.20110822214054.2520: *4* add(X)NeighborCouplingField
OperatorBuilder& OperatorBuilder::addLocalNeighborCouplingField(unsigned int left_site_number, MatrixConstPtr const& left_field_operator, MatrixConstPtr const& right_field_operator, complex<double> scale_factor) {
    unsigned int const signal = allocateSignal();
    connect(left_site_number,getStartSignal(),signal,lookupMatrixId(left_field_operator),scale_factor);
    return connect(left_site_number+1,signal,getEndSignal(),lookupMatrixId(right_field_operator));
}

OperatorBuilder& OperatorBuilder::addGlobalNeighborCouplingField(MatrixConstPtr const& left_field_operator, MatrixConstPtr const& right_field_operator, complex<double> scale_factor) {
    unsigned int const
        left_matrix_id = lookupMatrixId(left_field_operator),
        right_matrix_id = lookupMatrixId(right_field_operator),
        signal = allocateSignal();
    ;
    BOOST_FOREACH(unsigned int const site_number, irange(0u,numberOfSites())) {
        connect(site_number,getStartSignal(),signal,left_matrix_id,scale_factor);
        connect(site_number,signal,getEndSignal(),right_matrix_id);
    }
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
    if(getMatrix(matrix_id).size1() != sites[site_number])
        throw BadMatrixDimensionForSite(site_number,sites[site_number],matrix_id,getMatrix(matrix_id).size1());
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
            specification.connect(site_number,p.first.first,p.first.second,specification.lookupMatrixIdFromTable(*this,lookupWeightedSumOfMatrices(p.second)));
        }
        ++site_number;
    }
    if(add_start_and_end_loops) {
        BOOST_FOREACH(unsigned int const site_number, irange((size_t)0u,connections.size())) {
            specification.connect(site_number,specification.getStartSignal(),specification.getStartSignal(),specification.lookupIdentityMatrixId(sites[site_number]));
            specification.connect(site_number,specification.getEndSignal(),  specification.getEndSignal(),  specification.lookupIdentityMatrixId(sites[site_number]));
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
//@+node:gcross.20110817110920.2477: *4* (constructors)
OperatorSpecification::OperatorSpecification() {}

OperatorSpecification::OperatorSpecification(BOOST_RV_REF(OperatorSpecification) other)
  : MatrixTable(static_cast<BOOST_RV_REF(MatrixTable)>(other))
  , SignalTable(other)
  , connections(boost::move(other.connections))
{}
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
            links.emplace_back(left_signals_map[p.first.first],right_signals_map[p.first.second],getMatrixPtr(p.second));
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
//@+node:gcross.20110817110920.2469: *4* connect
void OperatorSpecification::connect(unsigned int site_number, unsigned int from, unsigned int to, unsigned int matrix_id) {
    if(connections.size() <= site_number) connections.resize(site_number+1);
    connections[site_number][make_pair(from,to)] = matrix_id;
}
//@+node:gcross.20110805222031.4620: *4* eliminateDeadXSignals
static inline bool eliminateDeadSignalsGeneric(
    bool forward,
    unsigned int first_signal,
    vector<SiteConnections>& connections
) {
    bool changed = false;
    set<unsigned int> incoming_signals;
    incoming_signals.insert(first_signal);
    vector<SiteConnections>::iterator iter = getFirstLoopIterator(forward,connections);
    REPEAT(connections.size()) {
        SiteConnections& site_connections = updateLoopIterator(forward,iter);
        set<SiteConnections::key_type> keys_to_remove;
        set<unsigned int> outgoing_signals;
        BOOST_FOREACH(SiteConnections::key_type const& key, site_connections | map_keys) {
            if(incoming_signals.find(incomingSignalOf(forward,key)) == incoming_signals.end()) {
                changed = true;
                keys_to_remove.insert(key);
            } else {
                outgoing_signals.insert(outgoingSignalOf(forward,key));
            }
        }
        site_connections.eraseAll(keys_to_remove);
        incoming_signals = boost::move(outgoing_signals);
    }
    return changed;
}

bool OperatorSpecification::eliminateDeadLeftSignals() {
    return eliminateDeadSignalsGeneric(true,getStartSignal(),connections);
}

bool OperatorSpecification::eliminateDeadRightSignals() {
    return eliminateDeadSignalsGeneric(false,getEndSignal(),connections);
}

bool OperatorSpecification::eliminateDeadSignals() {
    return eliminateDeadLeftSignals()
        || eliminateDeadRightSignals()
    ;
}
//@+node:gcross.20110805222031.4619: *4* eliminateNullMatrices
bool OperatorSpecification::eliminateNullMatrices() {
    bool changed = false;
    BOOST_FOREACH(SiteConnections& site_connections, connections) {
        set<SiteConnections::key_type> keys_to_remove;
        BOOST_FOREACH(SiteConnections::const_reference p, site_connections) {
            if(p.second == 0) {
                changed = true;
                keys_to_remove.insert(p.first);
            }
        }
        site_connections.eraseAll(keys_to_remove);
    }
    return changed;
}
//@+node:gcross.20110805222031.4625: *4* mergeXSignals
inline static bool mergeSignalsGeneric(
    bool forward,
    MatrixTable& matrix_table,
    SignalTable& signal_table,
    vector<SiteConnections>& connections
) {
    bool changed = false;
    vector<SiteConnections>::iterator iter = getFirstLoopIterator(forward,connections);
    REPEAT(connections.size()-1) {
        SiteConnections& site_connections = updateLoopIterator(forward,iter);
        SiteConnections& next_site_connections = dereferenceLoopIterator(forward,iter);

//@+at
// First, we build a table that gives us the set of incoming signals connected to each outgoing signal.
//@@c
        typedef map<unsigned int,unsigned int> IncomingToMatricesMap;
        typedef map<unsigned int,IncomingToMatricesMap> OutgoingToIncomingToMatricesMap;
        OutgoingToIncomingToMatricesMap out_to_in_to_matrics;
        BOOST_FOREACH(SiteConnections::const_reference site_connection, site_connections) {
            out_to_in_to_matrics
                [outgoingSignalOf(forward,site_connection.first)]
                [incomingSignalOf(forward,site_connection.first)]
                    = site_connection.second;
        }
//@+at
// Now we invert the out_to_in_matrices_table so that for each set of incoming signal/matrix connections it gives us the list of outgoing signals which share that exact set of connections.
//@@c
        typedef map<IncomingToMatricesMap,vector<unsigned int> > MergableOutgoingSignals;
        MergableOutgoingSignals mergable_outgoing_signals;
        BOOST_FOREACH(OutgoingToIncomingToMatricesMap::const_reference p, out_to_in_to_matrics) {
            mergable_outgoing_signals[p.second].push_back(p.first);
        }
//@+at
// Iterate over the inverted table.
//@@c
        BOOST_FOREACH(MergableOutgoingSignals::const_reference incoming_connections_and_outgoing_signals, mergable_outgoing_signals) {
            IncomingToMatricesMap incoming_connections = incoming_connections_and_outgoing_signals.first;
            vector<unsigned int> const& outgoing_signals = incoming_connections_and_outgoing_signals.second;
            if(outgoing_signals.size() <= 1) continue;
            changed = true;
//@+at
// Remove all of the old outgoing signals to be merged and replace them with the same set of connections to a freshly allocated signal.
//@@c
            unsigned int new_outgoing_signal = signal_table.allocateSignal();
            BOOST_FOREACH(IncomingToMatricesMap::const_reference incoming_signal_and_matrix, incoming_connections) {
                unsigned int const
                    incoming_signal = incoming_signal_and_matrix.first,
                    matrix_id = incoming_signal_and_matrix.second;
                BOOST_FOREACH(unsigned int const outgoing_signal, outgoing_signals) {
                    site_connections.erase(newSignalPair(forward,incoming_signal,outgoing_signal));
                }
                site_connections[newSignalPair(forward,incoming_signal,new_outgoing_signal)] = matrix_id;
            }
//@+at
// Now in the neighboring site take all of the connections to the incoming signals that we just removed and replace them with the sum of all of these connections made to the freshly allocated incoming signal.
//@@c
            vector<SiteConnections::key_type> keys_to_remove;
            typedef map<unsigned int,vector<unsigned int> > OutgoingToMatricesMap;
            OutgoingToMatricesMap merged_connections;
            BOOST_FOREACH(unsigned int const outgoing_signal, outgoing_signals) {
                BOOST_FOREACH(SiteConnections::const_reference p, next_site_connections) {
                    if(incomingSignalOf(forward,p.first) == outgoing_signal) {
                        keys_to_remove.push_back(p.first);
                        merged_connections[outgoingSignalOf(forward,p.first)].push_back(p.second);
                    }
                }
            }
            next_site_connections.eraseAll(keys_to_remove);
            BOOST_FOREACH(OutgoingToMatricesMap::const_reference merged_connection, merged_connections) {
                unsigned int const outgoing_signal = merged_connection.first;
                vector<unsigned int> const& matrices = merged_connection.second;
                next_site_connections[newSignalPair(forward,new_outgoing_signal,outgoing_signal)]
                    = matrix_table.lookupSumOfMatrices(matrices);
            }
        }
    }
    return changed;
}

bool OperatorSpecification::mergeLeftSignals() {
    return mergeSignalsGeneric(true,*this,*this,connections);
}

bool OperatorSpecification::mergeRightSignals() {
    return mergeSignalsGeneric(false,*this,*this,connections);
}

bool OperatorSpecification::mergeSignals() {
    return mergeLeftSignals()
        || mergeRightSignals()
    ;
}
//@+node:gcross.20110814140556.2442: *4* operator=
OperatorSpecification& OperatorSpecification::operator=(BOOST_COPY_ASSIGN_REF(OperatorSpecification) other)
{
    if (this != &other){
        MatrixTable::operator=(other);
        SignalTable::operator=(other);
        connections = other.connections;
    }
    return *this;
}

OperatorSpecification& OperatorSpecification::operator=(BOOST_RV_REF(OperatorSpecification) other)
{
    if (this != &other){
        MatrixTable::operator=(static_cast<BOOST_RV_REF(MatrixTable)>(other));
        SignalTable::operator=(other);
        connections = boost::move(other.connections);
    }
    return *this;
}
//@+node:gcross.20110814140556.2444: *4* optimize
void OperatorSpecification::optimize() {
    for(bool keep_going = true;
        keep_going;
        keep_going =
            eliminateNullMatrices()
         || eliminateDeadSignals()
         || mergeSignals()
    ) ;
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
