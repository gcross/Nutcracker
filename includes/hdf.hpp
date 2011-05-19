//@+leo-ver=5-thin
//@+node:gcross.20110511141322.2199: * @file hdf.hpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110511141322.2202: ** << License >>
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

//@+<< Documentation >>
//@+node:gcross.20110511141322.2204: ** << Documentation >>
/*!
\file hdf5.hpp
\brief HDF serialization functions
*/
//@-<< Documentation >>

#ifndef NUTCRACKER_HDF_HPP
#define NUTCRACKER_HDF_HPP

//@+<< Includes >>
//@+node:gcross.20110511141322.2205: ** << Includes >>
#include <algorithm>
#include <complex>
#include <boost/container/vector.hpp>
#include <boost/format.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/lexical_cast.hpp>
#include <boost/range/adaptor/indirected.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/algorithm/for_each.hpp>
#include <boost/scoped_array.hpp>
#include <hdf5.h>
#include <hdf5_hl.h>
#include <iterator>
#include <string>

#include "states.hpp"
#include "tensors.hpp"
#include "utilities.hpp"
//@-<< Includes >>

namespace Nutcracker { namespace HDF {

//@+<< Usings >>
//@+node:gcross.20110511141322.2206: ** << Usings >>
namespace lambda = boost::lambda;

using boost::adaptors::indirected;
using boost::container::vector;
using boost::copy;
using boost::for_each;
using boost::format;
using boost::lexical_cast;
using boost::scoped_array;

using std::back_inserter;
using std::complex;
using std::string;
//@-<< Usings >>

//! \defgroup HDF HDF serialization
//! @{

//@+others
//@+node:gcross.20110511190907.2258: ** Type functions
template<typename T> struct datatypeOf {};
template<> struct datatypeOf<double> { static hid_t get() { return H5T_NATIVE_DOUBLE; } };
template<> struct datatypeOf<complex<double > > { static hid_t get(); };
template<> struct datatypeOf<uint32_t> { static hid_t get() { return H5T_NATIVE_UINT; } };
//@+node:gcross.20110511190907.3456: ** Parameter wrappers
DEFINE_TEMPLATIZED_PARAMETER(TakeOwnershipOf,takeOwnershipOf)
DEFINE_TEMPLATIZED_PARAMETER(CreateAt,createAt)
DEFINE_TEMPLATIZED_PARAMETER(RangeOf,rangeOf)
//@+node:gcross.20110511150727.2222: ** Exceptions
//@+node:gcross.20110511150727.2223: *3* ErrorStack
struct ErrorStack : public Exception {
    ErrorStack(const char* action);
};
//@+node:gcross.20110511190907.2263: *3* NoSuchAttributeException
struct NoSuchAttributeException : public Exception {
    template<typename NameType> NoSuchAttributeException(NameType name)
        : Exception((format("No such attribute named %1%") % name).str()) {}
};
//@+node:gcross.20110511190907.2313: *3* InconsistentTensorDimensions
struct InconsistentTensorDimensions : public Exception {
    InconsistentTensorDimensions() : Exception("The tensor dimensions are inconsistent.") {}
};
//@+node:gcross.20110511190907.2285: *3* NoSuchObjectException
struct NoSuchObjectException : public Exception {
    NoSuchObjectException(const char* kind, const char* name)
        : Exception((format("No such %1% named %2%") % kind % name).str()) {}
};
//@+node:gcross.20110511190907.2310: *3* WrongTensorNormalizationException
struct WrongTensorNormalizationException : public Exception {
    WrongTensorNormalizationException(string const& expected_normalization, string const& actual_normalization)
        : Exception((format("Expected a tensor with %1% normalization but encountered a tensor with %2% normalization.") % expected_normalization % actual_normalization).str())
    {}
};
//@+node:gcross.20110511190907.2271: *3* WrongTensorRankException
struct WrongTensorRankException : public Exception {
    WrongTensorRankException(unsigned int const expected_rank, unsigned int const actual_rank)
        : Exception((format("Expected a tensor with rank %1% but encountered a tensor with rank %2%.") % expected_rank % actual_rank).str())
    {}
};
//@+node:gcross.20110511190907.2292: ** Classes
//@+node:gcross.20110511190907.3507: *3* Attribute
class Attribute {
    protected:

    hid_t id;
    string name;

    public:

    Attribute(hid_t id, string name)
      : id(id), name(name)
    {}

    operator string() const;
    operator unsigned int() const;
    operator double() const;

    void operator=(string const& value);
    void operator=(unsigned int value);
    void operator=(double value);
};
//@+node:gcross.20110511190907.2293: *3* Location
template<typename T> T assertSuccess(const char* action, T const result);
class Location {

protected:

    hid_t parent;
    string name;

public:

    Location() {}

    template<typename NameType> Location(hid_t const& parent, NameType name)
      : parent(parent)
      , name(lexical_cast<string>(name))
    {}

    template<typename NestedNameType> Location operator/(NestedNameType nested_name) const {
        return Location(parent,(format("%1%/%2%") % name % nested_name).str());
    }

    template<typename NestedNameType> Location& operator/=(NestedNameType nested_name) {
        name = (format("%1%/%2%") % name % nested_name).str();
        return *this;
    }

    bool operator==(Location const& other) const {
        return other.parent == parent
            && other.name == name;
    }

    operator hid_t() const { return parent; }
    operator char const* () const { return name.c_str(); }

    bool exists() {
        return assertSuccess(
            "querying link existence",
            H5Lexists(parent,name.c_str(),H5P_DEFAULT)
        );
    }
};
//@+node:gcross.20110511190907.3450: *3* LocationIterator
class LocationIterator
  : public iterator_facade<
         LocationIterator
        ,Location const
        ,random_access_traversal_tag
        ,Location const&
        ,int
    >
{
    //@+others
    //@+node:gcross.20110511190907.3471: *4* Constructors
    public:

    LocationIterator() {}

    LocationIterator(LocationIterator const& other)
      : parent_location(other.parent_location)
      , my_location(other.my_location)
      , index(other.index)
      , direction(other.direction)
    {}

    LocationIterator(Location const& parent_location, int index=0, int direction=1)
      : parent_location(parent_location)
      , index(index)
      , direction(direction)
    {
        updateMyLocation();
    }
    //@+node:gcross.20110511190907.3470: *4* Fields
    protected:

    Location parent_location, my_location;
    int index, direction;
    //@+node:gcross.20110511190907.3472: *4* Facade requirements
    public:

    Location const& dereference() const { return my_location; }

    bool equal(LocationIterator const& other) const {
        return parent_location == other.parent_location
            && index == other.index;
    }

    void increment() { advance(1); }
    void decrement() { advance(-1); }
    void advance(int n) { index += n*direction; updateMyLocation(); }

    int distance_to(LocationIterator const& other) const { return (other.index - index)*direction; }
    //@+node:gcross.20110511190907.3473: *4* Miscellaneous
    protected:

    void updateMyLocation() {
        my_location = parent_location / lexical_cast<string>(index);
    }
    //@+node:gcross.20110511190907.3514: *4* I/O
    public:

    template<typename T> LocationIterator& operator<<(T const& value) {
        dereference() << value;
        increment();
        return *this;
    }

    template<typename T> LocationIterator& operator>>(T& value) {
        my_location >> value;
        increment();
        return *this;
    }

    template<typename T> LocationIterator& operator<<(RangeOf<T> values) {
        for_each(*values,lambda::var(*this) << lambda::_1);
        return *this;
    }
    //@-others
};
//@+node:gcross.20110511190907.2342: *3* Identified
class Identified {
    //@+others
    //@+node:gcross.20110511190907.2362: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(Identified)
    //@+node:gcross.20110511190907.3685: *4* Assignment
    protected:

    void operator=(BOOST_RV_REF(Identified) other) {
        if(id >= 0) close();
        id = other.id;
        other.id = -1;
    }

    void swap(Identified& other) {
        std::swap(id,other.id);
    }
    //@+node:gcross.20110511190907.3451: *4* Comparison
    public:

    bool operator==(Identified const& other) { return id == other.id; }
    //@+node:gcross.20110511190907.2364: *4* Constructors
    protected:

    Identified() : id(-1) {}
    Identified(hid_t id) : id(id) {}
    Identified(BOOST_RV_REF(Identified) other) : id(other.id) { other.id = -1; }
    //@+node:gcross.20110511190907.3690: *4* Destructors
    public:

    virtual ~Identified() {};
    virtual void close() = 0;
    //@+node:gcross.20110511190907.2363: *4* Fields
    protected:

    hid_t id;
    //@+node:gcross.20110511190907.2389: *4* Informational
    public:

    hid_t getId() const { return id; }
    //@-others
};
//@+node:gcross.20110511190907.2339: *3* Dataspace
struct Dataset;
struct Dataspace : public Identified {
    //@+others
    //@+node:gcross.20110511190907.2374: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(Dataspace)
    //@+node:gcross.20110511190907.3692: *4* Assignment
    public:

    Dataspace& operator=(BOOST_RV_REF(Dataspace) other) {
        Identified::operator=(static_cast<BOOST_RV_REF(Identified)>(other));
        return *this;
    }

    void swap(Dataspace& other) {
        Identified::swap(other);
    }
    //@+node:gcross.20110511190907.2372: *4* Constructors
    public:

    Dataspace() {}
    Dataspace(BOOST_RV_REF(Dataspace) other) : Identified(static_cast<BOOST_RV_REF(Identified)>(other)) {}

    explicit Dataspace(Dataset const& dataset);
    Dataspace(unsigned int const rank, hsize_t const* dimensions);
    explicit Dataspace(TakeOwnershipOf<hid_t const> wrapped_id) : Identified(*wrapped_id) {}
    //@+node:gcross.20110511190907.2375: *4* Destructors
    public:

    virtual ~Dataspace();
    virtual void close();
    //@+node:gcross.20110511190907.2376: *4* Dimension information
    public:

    //! Retrieves the dimensions of this dataspace.
    vector<hsize_t> dimensions() const;

    //! Retrieves the rank of this dataspace.
    unsigned int rank() const;

    //! Retrieves the size of this dataspace.
    unsigned int size() const;
    //@-others
};
//@+node:gcross.20110511190907.3660: *3* Locatable
class Locatable : public Identified {
    //@+others
    //@+node:gcross.20110511190907.3670: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(Locatable)
    //@+node:gcross.20110511190907.3657: *4* Attributes
    public:

    Attribute operator[] (string name) const { return Attribute(id,name); }
    //@+node:gcross.20110511190907.3662: *4* Constructors
    protected:

    Locatable() {}
    Locatable(hid_t id) : Identified(id) {}
    Locatable(BOOST_RV_REF(Locatable) other) : Identified(static_cast<BOOST_RV_REF(Identified)>(other)) {}
    //@+node:gcross.20110511190907.3659: *4* Informational
    public:

    virtual Location getLocation() const { return Location(id,"."); }
    //@+node:gcross.20110511190907.3457: *4* Subpathable
    public:

    template<typename NameType> Location operator/ (NameType name) const {
        return Location(id,name);
    }
    //@-others
};
//@+node:gcross.20110511190907.3651: *3* File
class File : public Locatable {
    //@+others
    //@+node:gcross.20110511190907.3653: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(File)
    //@+node:gcross.20110511190907.3694: *4* Assignment
    public:

    File& operator=(BOOST_RV_REF(File) other) {
        Identified::operator=(static_cast<BOOST_RV_REF(Identified)>(other));
        return *this;
    }

    void swap(File& other) {
        Identified::swap(other);
    }
    //@+node:gcross.20110511190907.3655: *4* Constructors
    protected:

    File(hid_t id) : Locatable(id) {}

    public:

    File() {}
    File(BOOST_RV_REF(File) other) : Locatable(static_cast<BOOST_RV_REF(Locatable)>(other)) {}

    explicit File(char const* filepath);
    File(CreateAt<char const* const> wrapped_filepath);
    //@+node:gcross.20110511190907.3668: *4* Destructors
    public:

    virtual ~File();
    virtual void close();
    //@+node:gcross.20110511190907.3684: *4* File commands
    public:

    virtual void flush();
    //@+node:gcross.20110511190907.3820: *4* Informational
    public:

    Location getLocation() const { return Location(id,"."); }
    //@+node:gcross.20110517164912.2517: *4* Links
    public:

    void remove(char const* name) {
        assertSuccess(
            "deleting child",
            H5Ldelete(id,name,H5P_DEFAULT)
        );
    } 
    //@-others
};
//@+node:gcross.20110511190907.2338: *3* Object
struct Object : public Locatable {
    //@+others
    //@+node:gcross.20110511190907.2366: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(Object)
    //@+node:gcross.20110511190907.2367: *4* Constructors
    protected:

    Object() {}
    Object(BOOST_RV_REF(Object) other) : Locatable(static_cast<BOOST_RV_REF(Locatable)>(other)) {}

    public:

    explicit Object(TakeOwnershipOf<hid_t const> wrapped_id) : Locatable(*wrapped_id) {}
    //@+node:gcross.20110511190907.2368: *4* Destructors
    public:

    virtual ~Object();
    virtual void close();
    //@-others
};
//@+node:gcross.20110511190907.2345: *3* Dataset
struct Dataset : public Object {
    //@+others
    //@+node:gcross.20110511190907.2360: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(Dataset)
    //@+node:gcross.20110511190907.3696: *4* Assignment
    public:

    Dataset& operator=(BOOST_RV_REF(Dataset) other) {
        Identified::operator=(static_cast<BOOST_RV_REF(Identified)>(other));
        return *this;
    }

    void swap(Dataset& other) {
        Identified::swap(other);
    }
    //@+node:gcross.20110511190907.2358: *4* Constructors
    public:

    Dataset() {}
    explicit Dataset(TakeOwnershipOf<hid_t const> wrapped_id) : Object(wrapped_id) {}
    Dataset(BOOST_RV_REF(Dataset) other) : Object(static_cast<BOOST_RV_REF(Object)>(other)) {}

    explicit Dataset(Location const& location);
    //@+node:gcross.20110511190907.2378: *4* Data access
    public:

    //! Reads the tensor data in into memory.
    /*!
    \param datatype the type of the data in memory
    \param destination the location in memory to which the data should be written
    */
    void readTensorData(
          hid_t const datatype
        , void* destination
    ) const;

    //! Reads the tensor data into memory.
    /*!
    \param dataset the dataset from which the data should be read
    \param destination the location in memory to which the data should be written
    */
    template<typename T> void readTensorData(T* destination) const {
        readTensorData(datatypeOf<T>::get(),destination);
    }

    //! Reads the tensor data into memory.
    /*!
    */
    template<typename T> vector<T> readTensorData() const {
        vector<T> data(size());
        readTensorData(datatypeOf<T>::get(),&data.front());
        return boost::move(data);
    }
    //@+node:gcross.20110511190907.2377: *4* Dimension information
    public:

    //! Retrives the dimensions of this dataset.
    vector<hsize_t> dimensions() const;

    //! Retrieves the dimensions of this dataset and asserts that the rank is \c expected_rank, throwing an exception otherwise.
    vector<hsize_t> dimensionsWithAssertedRank(unsigned int expected_rank) const;

    //! Retrieves the rank of this dataset.
    unsigned int rank() const;

    //! Retrieves the size of this dataset.
    unsigned int size() const;
    //@-others
};
//@+node:gcross.20110511190907.3458: *3* Group
struct Group : public Object {
    //@+others
    //@+node:gcross.20110511190907.3460: *4* [Move support]
    private:

    BOOST_MOVABLE_BUT_NOT_COPYABLE(Group)
    //@+node:gcross.20110511190907.3698: *4* Assignment
    public:

    Group& operator=(BOOST_RV_REF(Group) other) {
        Identified::operator=(static_cast<BOOST_RV_REF(Identified)>(other));
        return *this;
    }

    void swap(Group& other) {
        Identified::swap(other);
    }
    //@+node:gcross.20110511190907.3476: *4* Associated types
    public:

    typedef LocationIterator iterator;
    typedef LocationIterator const_iterator;
    typedef LocationIterator reverse_iterator;
    typedef LocationIterator reverse_const_iterator;
    //@+node:gcross.20110511190907.3462: *4* Constructors
    public:

    Group() {}
    explicit Group(TakeOwnershipOf<hid_t const> wrapped_id) : Object(wrapped_id) {}
    Group(BOOST_RV_REF(Group) other) : Object(static_cast<BOOST_RV_REF(Object)>(other)) {}

    explicit Group(Location const& location);
    Group(CreateAt<Location const> wrapped_location);
    //@+node:gcross.20110511190907.3567: *4* I/O support
    template<typename T> void operator<<(RangeOf<T> values) const {
        (*this)["size"] = static_cast<unsigned int>(values->size());
        begin() << values;
    }
    //@+node:gcross.20110511190907.3504: *4* Informational
    public:

    unsigned int size() const { return (*this)["size"]; }
    //@+node:gcross.20110511190907.3682: *4* Links
    public:

    void remove(char const* name) {
        assertSuccess(
            "deleting child",
            H5Ldelete(id,name,H5P_DEFAULT)
        );
    } 
    //@+node:gcross.20110511190907.3475: *4* Range interface
    public:

    iterator begin() const { return LocationIterator(getLocation()); }
    iterator end() const { return LocationIterator(getLocation(),size()); }

    reverse_iterator rbegin() const { return LocationIterator(getLocation(),size()-1,-1); }
    reverse_iterator rend() const { return LocationIterator(getLocation(),-1,-1); }
    //@-others
};
//@+node:gcross.20110511190907.2392: *3* Properties
struct Properties : public Identified {
    explicit Properties(hid_t const cls_id);
    virtual ~Properties();
    virtual void close();
};
//@+node:gcross.20110511141322.2207: ** Functions
//@+node:gcross.20110511150727.2221: *3* assertSuccess
//! Asserts that result is non-negative;  throws an exception otherwise.
template<typename T> T assertSuccess(const char* action, T const result) {
    if(result >= 0)
        return result;
    else
        throw ErrorStack(action);
}
//@+node:gcross.20110511150727.2226: *3* constructMessageFromErrorStack
//! Constructs an error message by walking the HD5 error stack.
/*!
\param action the action that caused the error, used to form a heading for the message;  should start with a lower-case letter
\returns the constructed error message 
*/
string constructMessageFromErrorStack(string const& action);
//@+node:gcross.20110511190907.3574: *3* writeRangeData
Dataset writeTensorData(
      Location const& location
    , unsigned int const rank
    , hsize_t const* dimensions
    , hid_t const datatype
    , void const* data
);

template<typename RangeType> Dataset writeRangeData(
      Location const& location
    , RangeType data
) {
    typedef typename RangeType::value_type T;

    hsize_t dimension = data.size();

    scoped_array<T> buffer(new T[dimension]);
    copy(data,buffer.get());

    hid_t const datatype = datatypeOf<T>::get();

    return writeTensorData(location,1u,&dimension,datatype,buffer.get());
}
//@+node:gcross.20110511141322.2222: *3* writeTensorData
//! Writes the tensor data located at \c data to a new dataset created in the group \c parent with the name \c name.
/*!
\param location the location at which the dataset should be created
\param rank the rank of the tensor
\param dimensions an array specifying the dimensions of the tensor (in row-major order)
\param datatype the memory datatype of the data
\param data a (read-only) pointer to the tensor data
\returns the id of the dataset that was created
*/
Dataset writeTensorData(
      Location const& location
    , unsigned int const rank
    , hsize_t const* dimensions
    , hid_t const datatype
    , void const* data
);

//! Writes the tensor data located at \c data to a new dataset created in the group \c parent with the name \c name.
/*!
\tparam Dimensions the type of the list of the dimensions of the tensor
\tparam T the type of the data
\param parent the location at which to create the dataset
\param dimensions an range specifying the dimensions of the tensor (in row-major order)
\param data a (read-only) pointer to the tensor data
\returns the id of the dataset that was created
*/
template<typename Dimensions, typename T> Dataset writeTensorData(
      Location const& location
    , Dimensions const& dimensions
    , T const* data
) {
    unsigned int const rank = dimensions.size();
    scoped_array<hsize_t> dims(new hsize_t[rank]);
    copy(dimensions,dims.get());

    hid_t const datatype = datatypeOf<T>::get();

    return writeTensorData(location,rank,dims.get(),datatype,data);
}
//@+node:gcross.20110511190907.3568: *3* writeVectorData
template<typename T> Dataset writeVectorData(
      Location const& location
    , vector<T> data
) {
    hsize_t dimension = data.size();
    return writeTensorData(location,1,&dimension,datatypeOf<T>::get(),&data.front());
}
//@+node:gcross.20110517164912.2518: *3* isSlash
inline bool isSlash(char c) { return c == '/'; }
//@+node:gcross.20110511190907.2294: ** I/O Operators
//@+node:gcross.20110511190907.2299: *3* OperatorSite
//@+node:gcross.20110511190907.2300: *4* <<
//! Write an object site tensor to an HDF object.
/*!
\param location the location of the object to write
\param operator_site_tensor the operator site tensor to write
\returns the id of the object that was created
*/
Group operator<<(Location const& location, OperatorSite const& operator_site_tensor);
//@+node:gcross.20110511190907.2306: *4* >>
//! Read an object site tensor from an HDF object.
/*!
\param location the location of the object to write
\param operator_site_tensor the operator site tensor to be overrwritten with the read data
\returns the id of the object that was read
*/
Group operator>>(Location const& location, OperatorSite& operator_site_tensor);
//@+node:gcross.20110511190907.2295: *3* StateSite
//@+node:gcross.20110511190907.2296: *4* <<
//! Write a state site tensor to an HDF object.
/*!
\note If the side tag is Left or Right then the "normalization" attribute will be set to respectively "left" or "right".

\tparam side the normalization of the state site tensor
\param location the location of the object to write
\param state_site_tensor the state site tensor to write
\returns the id of the object that was created
*/
template<typename side> Dataset operator<< (Location const& location, StateSite<side> const& state_site_tensor) {
    Dataset dataset(writeTensorData(
        location,
        state_site_tensor.dataDimensions(),
        (complex<double> const*)state_site_tensor
    ));

    optional<string> const& maybe_normalization = normalizationOf<side>::value;
    if(maybe_normalization) {
        dataset["normalization"] = *maybe_normalization;
    }

    return boost::move(dataset);
}
//@+node:gcross.20110511190907.2297: *4* >>
//! Read a state site tensor from an HDF object.
/*!
\note If the side tag is Left or Right then the "normalization" attribute of the object must be respectively "left" or "right".

\tparam side the normalization of the state site tensor
\param location the location of the object to write
\param state_site_tensor the state site tensor to be overritten with the read data
\returns the id of the object that was read
*/
template<typename side> Dataset operator>> (Location const& location, StateSite<side>& state_site_tensor) {
    Dataset dataset(location);

    optional<string> const& maybe_normalization = normalizationOf<side>::value;
    if(maybe_normalization) {
        string const& expected_normalization = *maybe_normalization;
        try {
            string const actual_normalization = dataset["normalization"];
            if(expected_normalization != actual_normalization) {
                throw WrongTensorNormalizationException(expected_normalization,actual_normalization);
            }
        } catch (NoSuchAttributeException const& _) {
            throw WrongTensorNormalizationException(expected_normalization,"unspecified");
        }
    }

    vector<hsize_t> dimensions = dataset.dimensionsWithAssertedRank(3);

    state_site_tensor =
        StateSite<side>(
            PhysicalDimension(dimensions[0]),
            LeftDimension(dimensions[1]),
            RightDimension(dimensions[2])
        );

    dataset.readTensorData<complex<double> >(state_site_tensor);

    return boost::move(dataset);
}
//@+node:gcross.20110511190907.3510: *3* State
Group operator<<(Location const& location, State const& state);
Group operator>>(Location const& location, State& state);
//@+node:gcross.20110511190907.3573: *3* Operator
Group operator<<(Location const& location, Operator const& operator_sites);
void operator>>(Location const& location, Operator& operator_sites);
//@-others

//! @}

} }

#endif
//@-leo
