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
#include <boost/functional/hash.hpp>
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
//@+node:gcross.20110805222031.2343: ** Classes
//@+node:gcross.20110805222031.4631: *3* MatrixTable
class MatrixTable {
//@+others
//@+node:gcross.20110814140556.2431: *4* [Move support]
private:

BOOST_COPYABLE_AND_MOVABLE(MatrixTable)
//@+node:gcross.20110805222031.4634: *4* [Type alises]
protected:

typedef boost::container::map<MatrixConstPtr,unsigned int> MatrixLookupTable;
typedef boost::container::vector<MatrixConstPtr> Matrices;
//@+node:gcross.20110805222031.4641: *4* Constructors
public:

MatrixTable();
MatrixTable(BOOST_RV_REF(MatrixTable) other);
//@+node:gcross.20110805222031.4632: *4* Fields
protected:

boost::container::map<unsigned int,unsigned int> identity_lookup_table;
Matrices matrices;
MatrixLookupTable matrix_lookup_table;
//@+node:gcross.20110805222031.2346: *4* Methods
public:

unsigned int lookupMatrixId(MatrixConstPtr const& matrix);
unsigned int lookupSumOfMatrices(vector<unsigned int> const& matrix_ids);
unsigned int lookupWeightedSumOfMatrices(vector<pair<unsigned int,complex<double> > > const& matrix_ids_and_scale_factors);
unsigned int lookupIdentityMatrixId(unsigned int dimension);
unsigned int lookupMatrixIdFromTable(MatrixTable const& other, unsigned int matrix_id_in_other);

static unsigned int getIMatrixId() { return 1u; };
static unsigned int getXMatrixId() { return 2u; };
static unsigned int getYMatrixId() { return 3u; };
static unsigned int getZMatrixId() { return 4u; };

Matrix const& getMatrix(unsigned int index) const;
MatrixConstPtr const& getMatrixPtr(unsigned int index) const;

//@+others
//@-others
//@+node:gcross.20110814140556.2434: *4* Operators
public:

MatrixTable& operator=(BOOST_COPY_ASSIGN_REF(MatrixTable) other);
MatrixTable& operator=(BOOST_RV_REF(MatrixTable) other);
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
