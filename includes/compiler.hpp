//@+leo-ver=5-thin
//@+node:gcross.20110805222031.2339: * @file compiler.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110805222031.2340: ** << License >>
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

#ifndef NUTCRACKER_COMPILER_HPP
#define NUTCRACKER_COMPILER_HPP

//@+<< Includes >>
//@+node:gcross.20110805222031.2341: ** << Includes >>
#include <boost/container/map.hpp>
#include <boost/container/set.hpp>
#include <boost/foreach.hpp>
#include <boost/functional/hash.hpp>
#include <boost/iterator/zip_iterator.hpp>
#include <boost/make_shared.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>
#include <boost/unordered_map.hpp>
#include <utility>

#include "operators.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110805222031.2342: ** << Usings >>
using boost::hash;
using boost::hash_range;
using boost::tuple;
using boost::unordered_map;

using std::binary_function;
using std::pair;
//@-<< Usings >>

//@+others
//@+node:gcross.20110805222031.2386: ** Exceptions
//@+node:gcross.20110805222031.2389: *3* BadMatrixDimensionForSite
struct BadMatrixDimensionForSite : public std::logic_error {
    unsigned int site_number, site_dimension, matrix_id, matrix_dimension;
    BadMatrixDimensionForSite(unsigned int site_number, unsigned int site_dimension, unsigned int matrix_id, unsigned int matrix_dimension)
      : std::logic_error((format("Attempted to add matrix id %3% which has dimension %4% to site number %1% which has dimension %2%. (%4% != %2%)") % site_number % site_dimension % matrix_dimension).str())
      , site_number(site_number)
      , site_dimension(site_dimension)
      , matrix_id(matrix_id)
      , matrix_dimension(matrix_dimension)
    {}
};
//@+node:gcross.20110815001337.2442: *3* NeighborSignalConflict
struct NeighborSignalConflict : public std::logic_error {
    unsigned int right_site_number;
    boost::container::set<unsigned int> left_site_right_signals, right_site_left_signals;
    template<typename RangeType1, typename RangeType2> NeighborSignalConflict(
        unsigned int right_site_number,
        RangeType1 left_site_right_signals,
        RangeType2 right_site_left_signals
    )
      : std::logic_error((
            boost::format("The left signals at site %1%, %2%, do not match the right signals of its left neighbor, %3%.")
              % right_site_number
              % rangeToString(left_site_right_signals)
              % rangeToString(right_site_left_signals)
        ).str())
      , right_site_number(right_site_number)
      , left_site_right_signals(boost::begin(left_site_right_signals),boost::end(left_site_right_signals))
      , right_site_left_signals(boost::begin(right_site_left_signals),boost::end(right_site_left_signals))
    {}
};
//@+node:gcross.20110815001337.2471: *3* NonSquareMatrix
struct NonSquareMatrix : public std::logic_error {
    unsigned int number_of_rows, number_of_columns;
    NonSquareMatrix(unsigned int number_of_rows,unsigned int number_of_columns)
      : std::logic_error((
            boost::format("Matrix is not square (%1%x%2%).")
              % number_of_rows
              % number_of_columns
        ).str())
    {}
};
//@+node:gcross.20110805222031.2387: *3* SiteDimensionTooLarge
struct SiteNumberTooLarge : public std::logic_error {
    unsigned int site_number, maximum_site_number;
    SiteNumberTooLarge(unsigned int site_number, unsigned int maximum_site_number)
      : std::logic_error((format("Site number %1% is too large because there are only %2% sites.") % site_number % maximum_site_number).str())
      , site_number(site_number)
      , maximum_site_number(maximum_site_number)
    {}
};
//@+node:gcross.20110826085250.2533: *3* WrongNumberOfSites
struct WrongNumberOfSites : public std::logic_error {
    unsigned int argument_number_of_sites, system_number_of_sites;
    WrongNumberOfSites(unsigned int argument_number_of_sites, unsigned int system_number_of_sites)
      : std::logic_error((format("The number of sites in the argument (%1%) does not match the number of sites in the system (%2%).  [%1% != %2%]") % argument_number_of_sites % system_number_of_sites).str())
      , argument_number_of_sites(argument_number_of_sites)
      , system_number_of_sites(system_number_of_sites)
    {}
};
//@+node:gcross.20110826235932.2597: ** Traits
//@+node:gcross.20110805222031.2343: ** Classes
//@+node:gcross.20110826235932.2550: *3* DataTable
template<typename DataTraits> struct DataTable {
//@+<< DataConstPtr alias >>
//@+node:gcross.20110826235932.2615: *4* << DataConstPtr alias >>
typedef typename DataTraits::DataPtr DataPtr;
typedef typename DataTraits::DataConstPtr DataConstPtr;
//@-<< DataConstPtr alias >>
//@+others
//@+node:gcross.20110826235932.2551: *4* [Move support]
private:

BOOST_COPYABLE_AND_MOVABLE(DataTable)
//@+node:gcross.20110826235932.2610: *4* [Inner function objects]
//@+node:gcross.20110826235932.2614: *5* DataConstPtrComparisonPredicate
struct DataConstPtrComparisonPredicate : std::binary_function<DataConstPtr,DataConstPtr,bool> {
    bool operator()(DataConstPtr const& x, DataConstPtr const& y) const {
        if(x == y) return false;
        unsigned int const
            x_size = DataTraits::dimensionOf(x),
            y_size = DataTraits::dimensionOf(y);
        if(x_size < y_size) return true;
        if(x_size > y_size) return false;
        typedef boost::tuple<std::complex<double>,std::complex<double> > XY;
        BOOST_FOREACH(
            XY const& xy,
            std::make_pair(
                boost::make_zip_iterator(boost::make_tuple(
                    DataTraits::access(x).begin(),
                    DataTraits::access(y).begin()
                )),
                boost::make_zip_iterator(boost::make_tuple(
                    DataTraits::access(x).end(),
                    DataTraits::access(y).end()
                ))
            )
        ) {
            complex<double> const &a = xy.get<0>(), &b = xy.get<1>();
            if(abs(a-b) > 1e-14) {
                if(a.real() != b.real())
                    return a.real() < b.real();
                else
                    return a.imag() < b.imag();
            }
        }
        return false;
    }
};
//@+node:gcross.20110826235932.2552: *4* [Type alises]
protected:

typedef boost::container::map<unsigned int,unsigned int> IdentityIndex;
typedef boost::container::map<DataConstPtr,unsigned int,DataConstPtrComparisonPredicate> DataIndex;
typedef boost::container::vector<DataConstPtr> DataStore;
//@+node:gcross.20110826235932.2553: *4* Constructors
public:

DataTable() {
    store.emplace_back();
}

DataTable(BOOST_RV_REF(DataTable) other)
  : identity_index(boost::move(other.identity_index))
  , store(boost::move(other.store))
  , index(boost::move(other.index))
{}
//@+node:gcross.20110826235932.2554: *4* Fields
protected:

IdentityIndex identity_index;
DataStore store;
DataIndex index;
//@+node:gcross.20110826235932.2589: *4* Methods
public:

//@+others
//@+node:gcross.20110826235932.2595: *5* get
DataConstPtr const& get(unsigned int id) const {
    return store[id];
}
//@+node:gcross.20110826235932.2609: *5* getSizeOf
unsigned int getSizeOf(unsigned int const id) const {
    return DataTraits::dimensionOf(get(id));
}
//@+node:gcross.20110826235932.2592: *5* lookupIdFromTable
unsigned int lookupIdFromTable(DataTable const& other, unsigned int id_in_other) {
    return lookupIdOf(other.get(id_in_other));
}
//@+node:gcross.20110826235932.2591: *5* lookupIdOf
unsigned int lookupIdOf(DataConstPtr const& data) {
    DataTraits::assertCorrectlyFormed(data);
    if(DataTraits::dimensionOf(data) == 0) return 0;
    BOOST_FOREACH(complex<double> const& datum, DataTraits::access(data)) {
        if(abs(datum) >= 1e-14) goto notnull;
    }
    return 0;
notnull:
    typename DataIndex::const_iterator const iter = index.find(data);
    if(iter != index.end()) {
        return iter->second;
    } else {
        unsigned int id = store.size();
        index[data] = id;
        store.emplace_back(data);
        return id;
    }
}
//@+node:gcross.20110826235932.2593: *5* lookupIdOfSum
unsigned int lookupIdOfSum(vector<unsigned int> const& ids) {
    if(ids.size() == 0) return 0;
    if(ids.size() == 1) return ids[0];
    DataPtr data = DataTraits::copy(get(ids[0]));
    BOOST_FOREACH(unsigned int id, make_iterator_range(ids.begin()+1,ids.end())) {
        boost::transform(
            DataTraits::access(data),
            DataTraits::access(get(id)),
            DataTraits::access(data).begin(),
            boost::lambda::_1 + boost::lambda::_2
        );
    }
    return lookupIdOf(data);
}
//@+node:gcross.20110826235932.2594: *5* lookupIdOfWeightedSum
unsigned int lookupIdOfWeightedSum(vector<pair<unsigned int,complex<double> > > const& ids_and_scale_factors) {
    if(ids_and_scale_factors.size() == 0) return 0;
    if(ids_and_scale_factors.size() == 1 && ids_and_scale_factors.front().second == c(1,0))
        return ids_and_scale_factors.front().first;
    unsigned int dimension = DataTraits::dimensionOf(get(ids_and_scale_factors.front().first));
    DataPtr data = DataTraits::createBlank(dimension);
    typedef pair<unsigned int,complex<double> > P;
    BOOST_FOREACH(P const& p,ids_and_scale_factors) {
        boost::transform(
            DataTraits::access(data),
            DataTraits::access(get(p.first)),
            DataTraits::access(data).begin(),
            boost::lambda::_1 + p.second * boost::lambda::_2
        );
    }
    return lookupIdOf(data);
}
//@-others
//@+node:gcross.20110826235932.2556: *4* Operators
public:

DataTable& operator=(BOOST_COPY_ASSIGN_REF(DataTable) other) {
    if (this != &other){
        identity_index = other.identity_index;
        store = other.store;
        index = other.index;
    }
    return *this;
}

DataTable& operator=(BOOST_RV_REF(DataTable) other) {
    if (this != &other){
        identity_index = boost::move(other.identity_index);
        store = boost::move(other.store);
        index = boost::move(other.index);
    }
    return *this;
}
//@-others
};
//@+node:gcross.20110805222031.4631: *3* MatrixTable
//@+<< Traits >>
//@+node:gcross.20110805222031.4634: *4* << Traits >>
struct MatrixDataTraits {

//@+others
//@+node:gcross.20110826235932.2602: *5* [Type aliases]
typedef MatrixPtr DataPtr;
typedef MatrixConstPtr DataConstPtr;
//@+node:gcross.20110826235932.2605: *5* access
static inline Matrix::array_type const& access(DataConstPtr const& data) {
    return data->data();
}
static inline Matrix::array_type& access(DataPtr const& data) {
    return data->data();
}
//@+node:gcross.20110826235932.2606: *5* assertCorrectlyFormed
static inline void assertCorrectlyFormed(DataConstPtr const& data) {
    if(data->size1() != data->size2()) throw NonSquareMatrix(data->size1(),data->size2());
}
//@+node:gcross.20110826235932.2607: *5* copy
static inline DataPtr copy(DataConstPtr const& data) {
    return boost::make_shared<Matrix>(*data);
}
//@+node:gcross.20110826235932.2608: *5* createBlank
static inline DataPtr createBlank(unsigned int const dimension) {
    return boost::make_shared<Matrix>(dimension,dimension,c(0,0));
}
//@+node:gcross.20110826235932.2604: *5* createIdentity
static inline DataConstPtr createIdentity(unsigned int const dimension) {
    return identityMatrix(dimension);
}
//@+node:gcross.20110826235932.2601: *5* dimensionOf
static inline unsigned int dimensionOf(DataConstPtr const& data) {
    return data->size1();
}
//@-others

};
//@-<< Traits >>

class MatrixTable: public DataTable<MatrixDataTraits> {
//@+others
//@+node:gcross.20110814140556.2431: *4* [Move support]
private:

BOOST_COPYABLE_AND_MOVABLE(MatrixTable)
//@+node:gcross.20110826235932.2603: *4* [Type alises]
private:

typedef DataTable<MatrixDataTraits> Base;
//@+node:gcross.20110805222031.4641: *4* Constructors
public:

MatrixTable() {
    lookupIdOfIdentityWithDimension(2);
    lookupIdOf(Pauli::X);
    lookupIdOf(Pauli::Y);
    lookupIdOf(Pauli::Z);
}

MatrixTable(BOOST_RV_REF(MatrixTable) other)
  : Base(static_cast<BOOST_RV_REF(Base)>(other))
{}
//@+node:gcross.20110805222031.2346: *4* Methods
public:

static unsigned int getIMatrixId() { return 1u; };
static unsigned int getXMatrixId() { return 2u; };
static unsigned int getYMatrixId() { return 3u; };
static unsigned int getZMatrixId() { return 4u; };

//@+others
//@+node:gcross.20110826235932.2617: *5* lookupIdOfIdentityWithDimension
unsigned int lookupIdOfIdentityWithDimension(unsigned int dimension) {
    IdentityIndex::const_iterator const iter = identity_index.find(dimension);
    if(iter != identity_index.end()) {
        return iter->second;
    } else {
        DataConstPtr const data = identityMatrix(dimension);
        unsigned int id = lookupIdOf(data);
        identity_index[dimension] = id;
        return id;
    }
}
//@-others
//@+node:gcross.20110814140556.2434: *4* Operators
public:

MatrixTable& operator=(BOOST_COPY_ASSIGN_REF(MatrixTable) other) {
    Base::operator=(static_cast<BOOST_COPY_ASSIGN_REF(Base)>(other));
    return *this;
}
MatrixTable& operator=(BOOST_RV_REF(MatrixTable) other) {
    Base::operator=(static_cast<BOOST_RV_REF(Base)>(other));
    return *this;
}
//@-others
};
//@+node:gcross.20110805222031.4642: *3* SignalTable
class SignalTable {
//@+others
//@+node:gcross.20110805222031.2371: *4* Constructors
public:

SignalTable();
//@+node:gcross.20110805222031.4643: *4* Fields
protected:

unsigned int next_free_signal;
//@+node:gcross.20110805222031.2376: *4* Methods
public:

unsigned int allocateSignal();

static unsigned int getStartSignal() { return 1u; }
static unsigned int getEndSignal() { return 2u; }

void reserveSignalsBelow(unsigned int exclusive_upper_bound);
//@-others
};
//@+node:gcross.20110805222031.4637: *3* SiteConnections
class SiteConnections: public boost::container::map<std::pair<unsigned int,unsigned int>,unsigned int> {

public:

    template<typename KeysRange> void eraseAll(KeysRange keys) {
        BOOST_FOREACH(key_type const& key, keys) { erase(key); }
    };

};
//@+node:gcross.20110814140556.2428: *3* OperatorSpecification
class OperatorSpecification: public MatrixTable, public SignalTable {
//@+others
//@+node:gcross.20110815001337.2470: *4* [Inner types]
struct Connection {
    unsigned int from, to, matrix;
    Connection(unsigned int from, unsigned int to, unsigned int matrix)
      : from(from)
      , to(to)
      , matrix(matrix)
    {}
    bool operator<(Connection const& other) {
        return
            from < other.from 
         || from == other.from &&
            (   to < other.to
             || to == other.to &&
                (matrix < other.matrix
                )
            );
    }
};
//@+node:gcross.20110814140556.2436: *4* [Move support]
private:

BOOST_COPYABLE_AND_MOVABLE(OperatorSpecification)
//@+node:gcross.20110814140556.2435: *4* Constructors
public:

OperatorSpecification();
OperatorSpecification(BOOST_RV_REF(OperatorSpecification) other);
//@+node:gcross.20110814140556.2429: *4* Fields
public:

vector<SiteConnections> connections;
//@+node:gcross.20110814140556.2443: *4* Methods
void connect(unsigned int site_number, unsigned int from, unsigned int to, unsigned int matrix_id);

Operator compile() const;

bool eliminateNullMatrices();

bool eliminateDeadLeftSignals();
bool eliminateDeadRightSignals();
bool eliminateDeadSignals();

bool mergeLeftSignals();
bool mergeRightSignals();
bool mergeSignals();

void optimize();
//@+node:gcross.20110814140556.2437: *4* Operators
public:

OperatorSpecification& operator=(BOOST_COPY_ASSIGN_REF(OperatorSpecification) other);
OperatorSpecification& operator=(BOOST_RV_REF(OperatorSpecification) other);
//@-others
};
//@+node:gcross.20110805222031.2344: *3* OperatorBuilder
class OperatorBuilder: public MatrixTable, public SignalTable {
//@+others
//@+node:gcross.20110817110920.2471: *4* [Move support]
private:

BOOST_COPYABLE_AND_MOVABLE(OperatorBuilder)
//@+node:gcross.20110805222031.2381: *4* [Type alises]
protected:

typedef boost::container::map<tuple<unsigned int,unsigned int,unsigned int>,complex<double> > UnmergedSiteConnections;
//@+node:gcross.20110805222031.2382: *4* Compiling
public:

Operator compile(bool optimize=true,bool add_start_and_end_loops=true);
OperatorSpecification generateSpecification(bool add_start_and_end_loops=true);
//@+node:gcross.20110817110920.2473: *4* Constructors
public:

OperatorBuilder();
OperatorBuilder(BOOST_RV_REF(OperatorBuilder) other);
//@+node:gcross.20110805222031.2345: *4* Fields
protected:

vector<unsigned int> sites;

vector<UnmergedSiteConnections> connections;
//@+node:gcross.20110822214054.2516: *4* Informational
public:

unsigned int numberOfSites() const { return sites.size(); }
//@+node:gcross.20110817110920.2481: *4* Operators
public:

OperatorBuilder& operator=(BOOST_COPY_ASSIGN_REF(OperatorBuilder) other);
OperatorBuilder& operator=(BOOST_RV_REF(OperatorBuilder) other);
//@+node:gcross.20110822214054.2514: *4* Physics
OperatorBuilder& addLocalExternalField(unsigned int site_number, unsigned int field_matrix_id, complex<double> scale_factor=c(1,0));
OperatorBuilder& addGlobalExternalField(unsigned int field_matrix_id, complex<double> scale_factor=c(1,0));

OperatorBuilder& addLocalNeighborCouplingField(unsigned int left_site_number, unsigned int left_field_matrix_id, unsigned int right_field_matrix_id, complex<double> scale_factor=c(1,0));
OperatorBuilder& addGlobalNeighborCouplingField(unsigned int left_field_matrix_id, unsigned int right_field_matrix_id, complex<double> scale_factor=c(1,0));

OperatorBuilder& addTerm(vector<unsigned int> const& components);
//@+node:gcross.20110805222031.2375: *4* Sites
OperatorBuilder& addSite(unsigned int dimension);
OperatorBuilder& addSites(unsigned int number_of_sites, PhysicalDimension dimension);

OperatorBuilder& connect(unsigned int site_number, unsigned int left_signal, unsigned int right_signal, unsigned int matrix_id, complex<double> scale_factor=c(1,0));
//@-others
};
//@-others

}

#endif
//@-leo
