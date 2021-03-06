#ifndef NUTCRACKER_COMPILER_HPP
#define NUTCRACKER_COMPILER_HPP

#include <boost/concept_check.hpp>
#include <boost/container/map.hpp>
#include <boost/container/set.hpp>
#include <boost/foreach.hpp>
#include <boost/function.hpp>
#include <boost/functional/hash.hpp>
#include <boost/iterator/zip_iterator.hpp>
#include <boost/make_shared.hpp>
#include <boost/range/adaptor/indirected.hpp>
#include <boost/range/adaptor/map.hpp>
#include <boost/range/algorithm/transform.hpp>
#include <boost/range/concepts.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>
#include <boost/unordered_map.hpp>
#include <utility>

#include "nutcracker/operators.hpp"
#include "nutcracker/states.hpp"

namespace Nutcracker {

using boost::hash;
using boost::hash_range;
namespace lambda = boost::lambda;
using boost::tuple;
using boost::unordered_map;

using std::binary_function;
using std::pair;

struct NeighborSignalConflict : public std::logic_error {
    unsigned int right_site_number;
    boost::container::set<unsigned int> left_site_right_signals, right_site_left_signals;
    template<typename RangeType1, typename RangeType2> NeighborSignalConflict(
        unsigned int right_site_number,
        RangeType1 left_site_right_signals,
        RangeType2 right_site_left_signals
    ) throw ()
      : std::logic_error((
            boost::format("The left signals at site %1%, %2%, do not match the right signals of its left neighbor, %3%.")
              % right_site_number
              % rangeToString(right_site_left_signals)
              % rangeToString(left_site_right_signals)
        ).str())
      , right_site_number(right_site_number)
      , left_site_right_signals(boost::begin(left_site_right_signals),boost::end(left_site_right_signals))
      , right_site_left_signals(boost::begin(right_site_left_signals),boost::end(right_site_left_signals))
    {}
    ~NeighborSignalConflict() throw () {}
};
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
struct SiteNumberTooLarge : public std::logic_error {
    unsigned int site_number, maximum_site_number;
    SiteNumberTooLarge(unsigned int site_number, unsigned int maximum_site_number)
      : std::logic_error((format("Site number %1% is too large because there are only %2% sites.") % site_number % maximum_site_number).str())
      , site_number(site_number)
      , maximum_site_number(maximum_site_number)
    {}
};
struct WrongDimensionForSite : public std::logic_error {
    unsigned int site_number, site_dimension, matrix_dimension;
    WrongDimensionForSite(unsigned int site_number, unsigned int site_dimension, unsigned int matrix_dimension)
      : std::logic_error((format("Attempted to add matrix which has dimension %3% to site number %1% which has dimension %2%. (%3% != %2%)") % site_number % site_dimension % matrix_dimension).str())
      , site_number(site_number)
      , site_dimension(site_dimension)
      , matrix_dimension(matrix_dimension)
    {}
};
struct WrongNumberOfSites : public std::logic_error {
    unsigned int argument_number_of_sites, system_number_of_sites;
    WrongNumberOfSites(unsigned int argument_number_of_sites, unsigned int system_number_of_sites)
      : std::logic_error((format("The number of sites in the argument (%1%) does not match the number of sites in the system (%2%).  [%1% != %2%]") % argument_number_of_sites % system_number_of_sites).str())
      , argument_number_of_sites(argument_number_of_sites)
      , system_number_of_sites(system_number_of_sites)
    {}
};
static inline unsigned int& incomingSignalOf(bool forward,std::pair<unsigned int,unsigned int>& key) {
    return forward ? key.first : key.second;
}

static inline unsigned int& outgoingSignalOf(bool forward,std::pair<unsigned int,unsigned int>& key) {
    return forward ? key.second : key.first;
}

static inline unsigned int const& incomingSignalOf(bool forward,std::pair<unsigned int,unsigned int> const& key) {
    return forward ? key.first : key.second;
}

static inline unsigned int const& outgoingSignalOf(bool forward,std::pair<unsigned int,unsigned int> const& key) {
    return forward ? key.second : key.first;
}
static inline std::pair<unsigned int,unsigned int> newSignalPair(bool forward,unsigned int incoming_signal, unsigned int outgoing_signal) {
    return forward ? std::make_pair(incoming_signal,outgoing_signal) : std::make_pair(outgoing_signal,incoming_signal);
}
template<typename DataTraits> struct DataTable {
typedef typename DataTraits::DataPtr DataPtr;
typedef typename DataTraits::DataConstPtr DataConstPtr;
private:

BOOST_COPYABLE_AND_MOVABLE(DataTable)
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
public:

typedef DataTraits Traits;

protected:

typedef boost::container::map<DataConstPtr,unsigned int,DataConstPtrComparisonPredicate> DataIndex;
typedef boost::container::vector<DataConstPtr> DataStore;
public:

DataTable() {
    store.emplace_back();
}

DataTable(BOOST_RV_REF(DataTable) other)
  : store(boost::move(other.store))
  , index(boost::move(other.index))
{}
protected:

DataStore store;
DataIndex index;
public:

DataConstPtr const& get(unsigned int id) const {
    return store[id];
}
unsigned int getSizeOf(unsigned int const id) const {
    return DataTraits::dimensionOf(get(id));
}
unsigned int lookupIdFromTable(DataTable const& other, unsigned int id_in_other) {
    return lookupIdOf(other.get(id_in_other));
}
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
public:

DataTable& operator=(BOOST_COPY_ASSIGN_REF(DataTable) other) {
    if (this != &other) {
        store = other.store;
        index = other.index;
    }
    return *this;
}

DataTable& operator=(BOOST_RV_REF(DataTable) other) {
    if (this != &other) {
        store = boost::move(other.store);
        index = boost::move(other.index);
    }
    return *this;
}
};
struct MatrixDataTraits {

typedef Matrix Data;
typedef MatrixPtr DataPtr;
typedef MatrixConstPtr DataConstPtr;
static inline Matrix::array_type const& access(DataConstPtr const& data) {
    return data->data();
}
static inline Matrix::array_type& access(DataPtr const& data) {
    return data->data();
}
static inline void assertCorrectlyFormed(DataConstPtr const& data) {
    if(data->size1() != data->size2()) throw NonSquareMatrix(data->size1(),data->size2());
}
static inline DataPtr copy(DataConstPtr const& data) {
    return boost::make_shared<Matrix>(*data);
}
static inline DataPtr createBlank(unsigned int const dimension) {
    return boost::make_shared<Matrix>(dimension,dimension,c(0,0));
}
static inline unsigned int dimensionOf(DataConstPtr const& data) {
    return data->size1();
}

};

class MatrixTable: public DataTable<MatrixDataTraits> {
private:

BOOST_COPYABLE_AND_MOVABLE(MatrixTable)
private:

typedef DataTable<MatrixDataTraits> Base;

public:

typedef boost::container::map<unsigned int,unsigned int> IdentityIndex;
public:

MatrixTable() {
    lookupIdOfIdentityWithDimension(2);
    lookupIdOf(Pauli::X);
    lookupIdOf(Pauli::Y);
    lookupIdOf(Pauli::Z);
}

MatrixTable(BOOST_RV_REF(MatrixTable) other)
  : Base(static_cast<BOOST_RV_REF(Base)>(other))
  , identity_index(boost::move(other.identity_index))
{}
protected:

IdentityIndex identity_index;
public:

static unsigned int getIMatrixId() { return 1u; };
static unsigned int getXMatrixId() { return 2u; };
static unsigned int getYMatrixId() { return 3u; };
static unsigned int getZMatrixId() { return 4u; };

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
public:

MatrixTable& operator=(BOOST_COPY_ASSIGN_REF(MatrixTable) other) {
    if (this != &other) {
        Base::operator=(static_cast<BOOST_COPY_ASSIGN_REF(Base)>(other));
        identity_index = other.identity_index;
    }
    return *this;
}
MatrixTable& operator=(BOOST_RV_REF(MatrixTable) other) {
    if (this != &other) {
        Base::operator=(static_cast<BOOST_RV_REF(Base)>(other));
        identity_index = boost::move(other.identity_index);
    }
    return *this;
}
};
struct VectorDataTraits {

typedef Vector Data;
typedef VectorPtr DataPtr;
typedef VectorConstPtr DataConstPtr;
static inline Data const& access(DataConstPtr const& data) {
    return *data;
}
static inline Data& access(DataPtr const& data) {
    return *data;
}
static inline void assertCorrectlyFormed(DataConstPtr const& data) { }
static inline DataPtr copy(DataConstPtr const& data) {
    return boost::make_shared<Data>(*data);
}
static inline DataPtr createBlank(unsigned int const dimension) {
    return boost::make_shared<Data>(dimension,c(0,0));
}
static inline unsigned int dimensionOf(DataConstPtr const& data) {
    return data->size();
}

};

class VectorTable: public DataTable<VectorDataTraits> {
private:

BOOST_COPYABLE_AND_MOVABLE(VectorTable)
private:

typedef DataTable<VectorDataTraits> Base;

public:

typedef boost::container::map<std::pair<unsigned int,unsigned int>,unsigned int> ObservationIndex;
public:

VectorTable() {}

VectorTable(BOOST_RV_REF(VectorTable) other)
  : Base(static_cast<BOOST_RV_REF(Base)>(other))
  , observation_index(boost::move(other.observation_index))
{}
protected:

ObservationIndex observation_index;
public:

template<typename Range> unsigned int lookupIdOfRange(Range const& components) {
    BOOST_CONCEPT_ASSERT((boost::RandomAccessRangeConcept<Range const>));
    VectorPtr vector = boost::make_shared<Vector>(components.size());
    boost::copy(components,vector->begin());
    return Base::lookupIdOf(vector);
}
unsigned int lookupIdOfObservation(unsigned int observation, unsigned int dimension) {
    assert(observation < dimension);
    ObservationIndex::const_iterator const iter = observation_index.find(std::make_pair(observation,dimension));
    if(iter != observation_index.end()) {
        return iter->second;
    } else {
        DataPtr data = boost::make_shared<Vector>(dimension,c(0,0));
        (*data)[observation] = c(1,0);
        unsigned int id = Base::lookupIdOf(data);
        observation_index[std::make_pair(observation,dimension)] = id;
        return id;
    }
}
public:

VectorTable& operator=(BOOST_COPY_ASSIGN_REF(VectorTable) other) {
    if (this != &other) {
        Base::operator=(static_cast<BOOST_COPY_ASSIGN_REF(Base)>(other));
        observation_index = other.observation_index;
    }
    return *this;
}
VectorTable& operator=(BOOST_RV_REF(VectorTable) other) {
    if (this != &other) {
        Base::operator=(static_cast<BOOST_RV_REF(Base)>(other));
        observation_index = boost::move(other.observation_index);
    }
    return *this;
}
};
class SignalTable {
public:

SignalTable();
protected:

unsigned int next_free_signal;
public:

unsigned int allocateSignal();

static unsigned int getStartSignal() { return 1u; }
static unsigned int getEndSignal() { return 2u; }

void reserveSignalsBelow(unsigned int exclusive_upper_bound);
};
class SiteConnections: public boost::container::map<std::pair<unsigned int,unsigned int>,unsigned int> {

public:

    template<typename KeysRange> void eraseAll(KeysRange keys) {
        BOOST_FOREACH(key_type const& key, keys) { erase(key); }
    };

};
template<typename SpecificationTraits, typename Table, typename Facade> class Specification: public Table, public SignalTable {
private:

BOOST_COPYABLE_AND_MOVABLE(Specification)
public:

typedef Table DataTable;
typedef typename SpecificationTraits::Result Result;
typedef typename SpecificationTraits::Site Site;
typedef shared_ptr<Site const> SiteConstPtr;
typedef Link<typename Table::DataConstPtr> SiteLink;
public:

Specification() {}

Specification(BOOST_RV_REF(Specification) other)
  : DataTable(static_cast<BOOST_RV_REF(DataTable)>(other))
  , SignalTable(other)
  , connections(boost::move(other.connections))
{}
public:

vector<SiteConnections> connections;
public:

Result compile() const {
    using boost::adaptors::map_keys;
    using boost::container::map;
    using boost::container::set;

    vector<SiteConstPtr> sites;
    typedef map<vector<SiteLink>,SiteConstPtr> SiteIndex;
    SiteIndex site_index;
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
        vector<SiteLink> links;
        links.reserve(site_connections.size());
        BOOST_FOREACH(SiteConnections::const_reference p, site_connections) {
            links.emplace_back(left_signals_map[p.first.first],right_signals_map[p.first.second],DataTable::get(p.second));
        }
        typename SiteIndex::iterator iter = site_index.find(links);
        SiteConstPtr site;
        if(iter != site_index.end()) {
            site = iter->second;
        } else {
            site.reset(new Site(
                SpecificationTraits::constructSite(
                    PhysicalDimension(DataTable::Traits::dimensionOf(links[0].label)),
                    LeftDimension(left_signals.size()),
                    RightDimension(right_signals.size()),
                    links
                )
            ));
            site_index[links] = site;
        }
        sites.emplace_back(site);
        ++site_number;
        left_signals_map = boost::move(right_signals_map);
    }
    static vector<unsigned int> left_signals = list_of(2u);
    if(!boost::equal(left_signals_map | map_keys,left_signals)) {
        throw NeighborSignalConflict(site_number,left_signals_map | map_keys,left_signals);
    }
    return SpecificationTraits::postProcess(sites);
}
void connect(unsigned int site_number, unsigned int from, unsigned int to, unsigned int id) {
    if(connections.size() <= site_number) connections.resize(site_number+1);
    connections[site_number][std::make_pair(from,to)] = id;
}
private:

bool inline eliminateDeadSignalsGeneric(
    bool forward,
    unsigned int first_signal
) {
    bool changed = false;
    boost::container::set<unsigned int> incoming_signals;
    incoming_signals.insert(first_signal);
    vector<SiteConnections>::iterator iter = getFirstLoopIterator(forward,connections);
    REPEAT(connections.size()) {
        SiteConnections& site_connections = updateLoopIterator(forward,iter);
        boost::container::set<SiteConnections::key_type> keys_to_remove;
        boost::container::set<unsigned int> outgoing_signals;
        BOOST_FOREACH(SiteConnections::key_type const& key, site_connections | boost::adaptors::map_keys) {
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

public:

bool eliminateDeadLeftSignals() {
    return eliminateDeadSignalsGeneric(true,getStartSignal());
}

bool eliminateDeadRightSignals() {
    return eliminateDeadSignalsGeneric(false,getEndSignal());
}

bool eliminateDeadSignals() {
    return eliminateDeadLeftSignals()
        || eliminateDeadRightSignals()
    ;
}
bool eliminateNullMatrices() {
    bool changed = false;
    BOOST_FOREACH(SiteConnections& site_connections, connections) {
        boost::container::set<SiteConnections::key_type> keys_to_remove;
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
private:

inline bool mergeSignalsGeneric(bool forward) {
    bool changed = false;
    boost::container::vector<SiteConnections>::iterator iter = getFirstLoopIterator(forward,connections);
    REPEAT(connections.size()-1) {
        SiteConnections& site_connections = updateLoopIterator(forward,iter);
        SiteConnections& next_site_connections = dereferenceLoopIterator(forward,iter);

// First, we build a table that gives us the set of incoming signals connected to each outgoing signal.
        typedef boost::container::map<unsigned int,unsigned int> IncomingToMatricesMap;
        typedef boost::container::map<unsigned int,IncomingToMatricesMap> OutgoingToIncomingToMatricesMap;
        OutgoingToIncomingToMatricesMap out_to_in_to_matrics;
        BOOST_FOREACH(SiteConnections::const_reference site_connection, site_connections) {
            out_to_in_to_matrics
                [outgoingSignalOf(forward,site_connection.first)]
                [incomingSignalOf(forward,site_connection.first)]
                    = site_connection.second;
        }
// Now we invert the out_to_in_matrices_table so that for each set of incoming signal/matrix connections it gives us the list of outgoing signals which share that exact set of connections.
        typedef boost::container::map<IncomingToMatricesMap,vector<unsigned int> > MergableOutgoingSignals;
        MergableOutgoingSignals mergable_outgoing_signals;
        BOOST_FOREACH(OutgoingToIncomingToMatricesMap::const_reference p, out_to_in_to_matrics) {
            mergable_outgoing_signals[p.second].push_back(p.first);
        }
// Iterate over the inverted table.
        BOOST_FOREACH(MergableOutgoingSignals::const_reference incoming_connections_and_outgoing_signals, mergable_outgoing_signals) {
            IncomingToMatricesMap incoming_connections = incoming_connections_and_outgoing_signals.first;
            vector<unsigned int> const& outgoing_signals = incoming_connections_and_outgoing_signals.second;
            if(outgoing_signals.size() <= 1) continue;
            changed = true;
// Remove all of the old outgoing signals to be merged and replace them with the same set of connections to a freshly allocated signal.
            unsigned int const new_outgoing_signal = allocateSignal()
            ;
            BOOST_FOREACH(IncomingToMatricesMap::const_reference incoming_signal_and_matrix, incoming_connections) {
                unsigned int const
                    incoming_signal = incoming_signal_and_matrix.first,
                    matrix_id = incoming_signal_and_matrix.second;
                BOOST_FOREACH(unsigned int const outgoing_signal, outgoing_signals) {
                    site_connections.erase(newSignalPair(forward,incoming_signal,outgoing_signal));
                }
                site_connections[newSignalPair(forward,incoming_signal,new_outgoing_signal)] = matrix_id;
            }
// Now in the neighboring site take all of the connections to the incoming signals that we just removed and replace them with the sum of all of these connections made to the freshly allocated incoming signal.
            boost::container::vector<SiteConnections::key_type> keys_to_remove;
            typedef boost::container::map<unsigned int,vector<unsigned int> > OutgoingToMatricesMap;
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
                = DataTable::lookupIdOfSum(matrices);
            }
        }
    }
    return changed;
}

public:

bool mergeLeftSignals() {
    return mergeSignalsGeneric(true);
}

bool mergeRightSignals() {
    return mergeSignalsGeneric(false);
}

bool mergeSignals() {
    return mergeLeftSignals()
        || mergeRightSignals()
    ;
}
void optimize() {
    for(bool keep_going = true;
        keep_going;
        keep_going =
            eliminateNullMatrices()
         || eliminateDeadSignals()
         || mergeSignals()
    ) ;
}
Facade& operator=(BOOST_COPY_ASSIGN_REF(Specification) other)
{
    if (this != &other){
        DataTable::operator=(other);
        SignalTable::operator=(other);
        connections = other.connections;
    }
    return static_cast<Facade&>(*this);
}

Facade& operator=(BOOST_RV_REF(Specification) other)
{
    if (this != &other){
        DataTable::operator=(static_cast<BOOST_RV_REF(DataTable)>(other));
        SignalTable::operator=(other);
        connections = boost::move(other.connections);
    }
    return static_cast<Facade&>(*this);
}
};
struct OperatorSpecificationTraits {
typedef Operator Result;
typedef OperatorSite Site;
typedef shared_ptr<OperatorSite const> SiteConstPtr;
static inline Site constructSite(
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
    , vector<OperatorSiteLink> const& links
) {
    return constructOperatorSite(physical_dimension,left_dimension,right_dimension,links);
}
static inline Result postProcess(vector<SiteConstPtr> sites) {
    return boost::move(sites);
}
};
class OperatorSpecification: public Specification<OperatorSpecificationTraits,MatrixTable,OperatorSpecification> {
private:

BOOST_COPYABLE_AND_MOVABLE(OperatorSpecification)
private:

typedef Specification<OperatorSpecificationTraits,MatrixTable,OperatorSpecification> Base;
public:

OperatorSpecification() {}

OperatorSpecification(BOOST_RV_REF(OperatorSpecification) other)
  : Base(static_cast<BOOST_RV_REF(Base)>(other))
{}
public:

OperatorSpecification& operator=(BOOST_COPY_ASSIGN_REF(OperatorSpecification) other)
{
    return Base::operator=(static_cast<BOOST_COPY_ASSIGN_REF(Base)>(other));
}

OperatorSpecification& operator=(BOOST_RV_REF(OperatorSpecification) other)
{
    return Base::operator=(static_cast<BOOST_RV_REF(Base)>(other));
}
};
struct StateSpecificationTraits {
typedef State Result;
typedef StateSite<None> Site;
typedef shared_ptr<Site const> SiteConstPtr;
static inline Site constructSite(
      PhysicalDimension const physical_dimension
    , LeftDimension const left_dimension
    , RightDimension const right_dimension
    , vector<StateSiteLink> const& links
) {
    return constructStateSite(physical_dimension,left_dimension,right_dimension,links);
}
static inline Result postProcess(vector<SiteConstPtr> sites) {
    return State(copyFrom(sites | boost::adaptors::indirected));
}
};
class StateSpecification: public Specification<StateSpecificationTraits,VectorTable,StateSpecification> {
private:

BOOST_COPYABLE_AND_MOVABLE(StateSpecification)
private:

typedef Specification<StateSpecificationTraits,VectorTable,StateSpecification> Base;
public:

StateSpecification() {}

StateSpecification(BOOST_RV_REF(StateSpecification) other)
  : Base(static_cast<BOOST_RV_REF(Base)>(other))
{}
public:

StateSpecification& operator=(BOOST_COPY_ASSIGN_REF(StateSpecification) other)
{
    return Base::operator=(static_cast<BOOST_COPY_ASSIGN_REF(Base)>(other));
}

StateSpecification& operator=(BOOST_RV_REF(StateSpecification) other)
{
    return Base::operator=(static_cast<BOOST_RV_REF(Base)>(other));
}
};
template<typename Specification, typename Facade> class Builder: public SignalTable {
private:

BOOST_COPYABLE_AND_MOVABLE(Builder)
public:

typedef typename Specification::DataTable DataTable;
typedef typename DataTable::Traits DataTraits;
typedef typename DataTraits::Data Data;
typedef typename DataTraits::DataPtr DataPtr;
typedef typename DataTraits::DataConstPtr DataConstPtr;
typedef typename Specification::SiteConstPtr SiteConstPtr;

protected:

typedef boost::container::map<std::pair<unsigned int,unsigned int>,boost::container::vector<DataConstPtr> > UnmergedSiteConnections;
public:

Builder(unsigned int number_of_sites, PhysicalDimension physical_dimension)
  : sites(number_of_sites,*physical_dimension)
  , connections(number_of_sites)
{}

template<typename Dimensions> Builder(Dimensions const& dimensions)
  : sites(dimensions.begin(),dimensions.end())
  , connections(sites.size())
{}

Builder(BOOST_RV_REF(Builder) other)
  : SignalTable(other)
  , connections(boost::move(other.connections))
{}
protected:

vector<unsigned int> const sites;

vector<UnmergedSiteConnections> connections;
public:

template<typename Components> Facade& addProductTerm(Components const& components) {
    if(components.size() != numberOfSites()) throw WrongNumberOfSites(components.size(),numberOfSites());
    if(numberOfSites() == 1) {
        connect(0u,getStartSignal(),getEndSignal(),*components.begin());
    } else {
        unsigned int const signal = allocateSignal();
        typename boost::range_iterator<Components const>::type component = components.begin();
        connect(0u,getStartSignal(),signal,*component++);
        BOOST_FOREACH(unsigned int const site_number, irange(1u,numberOfSites()-1)) {
            connect(site_number,signal,signal,*component++);
        }
        connect(numberOfSites()-1,signal,getEndSignal(),*component++);
    }
    return static_cast<Facade&>(*this);
}
template<typename Callback> Facade& addTerm(Callback const& callback)
{
    callback(static_cast<Facade&>(*this));
    return static_cast<Facade&>(*this);
}
unsigned int dimensionOfSite(unsigned int site_number) const {
    return sites[site_number];
}
Facade& connect(
    unsigned int const site_number,
    unsigned int const left_signal,
    unsigned int const right_signal,
    DataConstPtr data
) {
    if(site_number >= sites.size()) throw SiteNumberTooLarge(site_number,sites.size());
    DataTraits::assertCorrectlyFormed(data);
    if(DataTraits::dimensionOf(data) != sites[site_number])
        throw WrongDimensionForSite(site_number,sites[site_number],DataTraits::dimensionOf(data));
    connections[site_number][std::make_pair(left_signal,right_signal)].push_back(data);
    return static_cast<Facade&>(*this);
}
protected:

Specification generateSpecification(bool add_start_and_end_loops=true) {
    Specification specification;
    specification.reserveSignalsBelow(next_free_signal);
    unsigned int site_number = 0u;
    BOOST_FOREACH(UnmergedSiteConnections const& site_connections, connections) {
        BOOST_FOREACH(typename UnmergedSiteConnections::const_reference p, site_connections) {
            DataPtr total = DataTraits::createBlank(sites[site_number]);
            BOOST_FOREACH(DataConstPtr const& data, p.second) {
                *total += *data;
            }
            specification.connect(site_number,p.first.first,p.first.second,specification.lookupIdOf(total));
        }
        ++site_number;
    }
    return boost::move(specification);
}

public:
unsigned int numberOfSites() const { return sites.size(); }
public:

Facade& operator=(BOOST_COPY_ASSIGN_REF(Builder) other)
{
    if (this != &other){
        SignalTable::operator=(other);
        connections = other.connections;
    }
    return static_cast<Facade&>(*this);
}

Facade& operator=(BOOST_RV_REF(Builder) other)
{
    if (this != &other){
        SignalTable::operator=(other);
        connections = boost::move(other.connections);
    }
    return static_cast<Facade&>(*this);
}
public:

template<typename Callback> Facade& operator+=(Callback const& callback)
{
    return addTerm(callback);
}
};
class OperatorBuilder: public Builder<OperatorSpecification,OperatorBuilder> {
private:

BOOST_COPYABLE_AND_MOVABLE(OperatorBuilder)
private:

typedef Builder<OperatorSpecification,OperatorBuilder> Base;
public:

Operator compile(bool optimize=true, bool add_start_and_end_loops=true);
OperatorSpecification generateSpecification(bool add_start_and_end_loops=true);
public:

OperatorBuilder(unsigned int number_of_sites, PhysicalDimension physical_dimension)
  : Base(number_of_sites,physical_dimension)
{}

template<typename Dimensions> OperatorBuilder(Dimensions const& dimensions)
  : Base(dimensions)
{}

OperatorBuilder(BOOST_RV_REF(OperatorBuilder) other)
  : Base(static_cast<BOOST_RV_REF(Base)>(other))
{}
public:

OperatorBuilder& operator=(BOOST_COPY_ASSIGN_REF(OperatorBuilder) other)
{
    return Base::operator=(static_cast<BOOST_COPY_ASSIGN_REF(Base)>(other));
}

OperatorBuilder& operator=(BOOST_RV_REF(OperatorBuilder) other)
{
    return Base::operator=(static_cast<BOOST_RV_REF(Base)>(other));
}
};
class StateBuilder: public Builder<StateSpecification,StateBuilder> {
private:

BOOST_COPYABLE_AND_MOVABLE(StateBuilder)
private:

typedef Builder<StateSpecification,StateBuilder> Base;
public:

State compile(bool optimize=true);
StateSpecification generateSpecification();
public:

StateBuilder(unsigned int number_of_sites, PhysicalDimension physical_dimension)
  : Base(number_of_sites,physical_dimension)
{}

template<typename Dimensions> StateBuilder(Dimensions const& dimensions)
  : Base(dimensions)
{}

StateBuilder(BOOST_RV_REF(StateBuilder) other)
  : Base(static_cast<BOOST_RV_REF(Base)>(other))
{}
public:

StateBuilder& operator=(BOOST_COPY_ASSIGN_REF(StateBuilder) other)
{
    return Base::operator=(static_cast<BOOST_COPY_ASSIGN_REF(Base)>(other));
}

StateBuilder& operator=(BOOST_RV_REF(StateBuilder) other)
{
    return Base::operator=(static_cast<BOOST_RV_REF(Base)>(other));
}
};
template<typename Builder, typename Facade> struct Term {
    Facade operator*(complex<double> const& coefficient) const {
        Facade x = static_cast<Facade const&>(*this);
        x *= coefficient;
        return x;
    }

    Facade& operator*=(complex<double> const& coefficient) {
        Facade& x = static_cast<Facade&>(*this);
        x.multiplyBy(coefficient);
        return x;
    }
};
template<typename Builder, typename Facade1, typename Facade2> struct SumTerm : Term<Builder,SumTerm<Builder,Facade1,Facade2> > {
    Facade1 term1;
    Facade2 term2;
    SumTerm(Facade1 const& term1, Facade2 const& term2)
      : term1(term1)
      , term2(term2)
    {}

    void operator()(Builder& builder) const {
        builder += term1;
        builder += term2;
    }

    void multiplyBy(std::complex<double> const& coefficient) {
        term1.multiplyBy(coefficient);
    }
};

template<typename Builder, typename Facade1, typename Facade2> SumTerm<Builder,Facade1,Facade2> operator+(Facade1 const& facade1, Facade2 const& facade2) {
    return SumTerm<Builder,Facade1,Facade2>(facade1,facade2);
}
struct LocalExternalField : Term<OperatorBuilder,LocalExternalField> {
    unsigned int site_number;
    MatrixConstPtr field_matrix;

    LocalExternalField(unsigned int site_number, MatrixConstPtr const& field_matrix)
      : site_number(site_number)
      , field_matrix(field_matrix)
    {}

    void operator()(OperatorBuilder& builder) const {
        builder.connect(site_number,builder.getStartSignal(),builder.getEndSignal(),field_matrix);
    }

    void multiplyBy(std::complex<double> const& coefficient) {
        field_matrix = field_matrix * coefficient;
    }
};
struct GlobalExternalField : Term<OperatorBuilder,GlobalExternalField> {
    MatrixConstPtr field_matrix;

    GlobalExternalField(MatrixConstPtr const& field_matrix)
      : field_matrix(field_matrix)
    {}

    void operator()(OperatorBuilder& builder) const {
        BOOST_FOREACH(unsigned int const site_number, irange(0u,builder.numberOfSites())) {
            builder += LocalExternalField(site_number,field_matrix);
        }
    }

    void multiplyBy(std::complex<double> const& coefficient) {
        field_matrix = field_matrix * coefficient;
    }
};
struct LocalNeighborCouplingField : Term<OperatorBuilder,LocalNeighborCouplingField> {
    unsigned int left_site_number;
    MatrixConstPtr left_field_matrix, right_field_matrix;

    LocalNeighborCouplingField(unsigned int left_site_number, MatrixConstPtr const& left_field_matrix, MatrixConstPtr const& right_field_matrix)
      : left_site_number(left_site_number)
      , left_field_matrix(left_field_matrix)
      , right_field_matrix(right_field_matrix)
    {}

    void operator()(OperatorBuilder& builder) const {
        unsigned int const signal = builder.allocateSignal();
        builder.connect(left_site_number,builder.getStartSignal(),signal,left_field_matrix);
        builder.connect(left_site_number+1,signal,builder.getEndSignal(),right_field_matrix);
    }

    void multiplyBy(std::complex<double> const& coefficient) {
        left_field_matrix = left_field_matrix * coefficient;
    }
};
struct GlobalNeighborCouplingField : Term<OperatorBuilder,GlobalNeighborCouplingField> {
    MatrixConstPtr left_field_matrix, right_field_matrix;

    GlobalNeighborCouplingField(MatrixConstPtr const& left_field_matrix, MatrixConstPtr const& right_field_matrix)
      : left_field_matrix(left_field_matrix)
      , right_field_matrix(right_field_matrix)
    {}

    void operator()(OperatorBuilder& builder) const {
        unsigned int const signal = builder.allocateSignal();
        BOOST_FOREACH(unsigned int const site_number, irange(0u,builder.numberOfSites())) {
            builder.connect(site_number,builder.getStartSignal(),signal,left_field_matrix);
            builder.connect(site_number,signal,builder.getEndSignal(),right_field_matrix);
        }
    }

    void multiplyBy(std::complex<double> const& coefficient) {
        left_field_matrix = left_field_matrix * coefficient;
    }
};
struct TransverseIsingField : SumTerm<OperatorBuilder,GlobalExternalField,GlobalNeighborCouplingField> {
    typedef SumTerm<OperatorBuilder,GlobalExternalField,GlobalNeighborCouplingField> Base;

    TransverseIsingField(MatrixConstPtr const& external_field_matrix, MatrixConstPtr const& left_field_matrix, MatrixConstPtr const& right_field_matrix)
      : Base(GlobalExternalField(external_field_matrix),GlobalNeighborCouplingField(left_field_matrix,right_field_matrix))
    {}
};
struct ProductWithOneSiteDifferentTerm : Term<StateBuilder,ProductWithOneSiteDifferentTerm> {
    unsigned int site_number;
    VectorConstPtr common_observation, special_observation;

    ProductWithOneSiteDifferentTerm(unsigned int site_number, VectorConstPtr const& common_observation, VectorConstPtr const& special_observation)
      : site_number(site_number)
      , common_observation(common_observation)
      , special_observation(special_observation)
    {}

    void operator()(StateBuilder& builder) const {
        vector<VectorConstPtr> components(builder.numberOfSites(),common_observation);
        components[site_number] = special_observation;
        builder.addProductTerm(components);
    }

    void multiplyBy(std::complex<double> const& coefficient) {
        special_observation = special_observation * coefficient;
    }
};
struct WTerm : Term<StateBuilder,WTerm> {
    VectorConstPtr common_observation, special_observation;
    bool normalized;

    WTerm(VectorConstPtr const& common_observation, VectorConstPtr const& special_observation, bool normalized=true)
      : common_observation(common_observation)
      , special_observation(special_observation)
    {}

    void operator()(StateBuilder& builder) const {
        if(builder.numberOfSites() == 1u) {
            builder.connect(0u,builder.getStartSignal(),builder.getEndSignal(),special_observation);
        } else {
            VectorConstPtr normalized_special_observation = normalized ? (1.0/builder.numberOfSites()) * special_observation : special_observation;
            unsigned int const
                  signal_1 = builder.allocateSignal()
                , signal_2 = builder.allocateSignal()
                , last_site_number = builder.numberOfSites()-1
                ;
            builder.connect(0u,builder.getStartSignal(),signal_1,common_observation);
            builder.connect(0u,builder.getStartSignal(),signal_2,special_observation);
            builder.connect(last_site_number,signal_1,builder.getEndSignal(),special_observation);
            builder.connect(last_site_number,signal_2,builder.getEndSignal(),common_observation);
            BOOST_FOREACH(unsigned int const site_number, irange(1u,last_site_number)) {
                builder.connect(site_number,signal_1,signal_1,common_observation);
                builder.connect(site_number,signal_1,signal_2,special_observation);
                builder.connect(site_number,signal_2,signal_2,common_observation);
            }
        }
    }

    void multiplyBy(std::complex<double> const& coefficient) {
        special_observation = special_observation * coefficient;
    }
};

}

#endif
