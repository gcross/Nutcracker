//@+leo-ver=5-thin
//@+node:gcross.20110511141322.2209: * @file hdf.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110511141322.2213: ** << License >>
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
//@+node:gcross.20110511141322.2211: ** << Documentation >>
/*!
\file hdf5.hpp
\brief HDF serialization functions
*/
//@-<< Documentation >>

//@+<< Includes >>
//@+node:gcross.20110511141322.2214: ** << Includes >>
#include <boost/algorithm/string/trim.hpp>
#include <boost/assign/list_of.hpp>
#include <boost/bind.hpp>
#include <boost/filesystem.hpp>
#include <boost/function.hpp>
#include <boost/move/move.hpp>
#include <boost/signals/trackable.hpp>
#include <complex>
#include <hdf5.h>
#include <hdf5_hl.h>
#include <iomanip>
#include <ostream>

#include "chain.hpp"
#include "hdf.hpp"
#include "io.hpp"
//@-<< Includes >>

namespace Nutcracker { namespace HDF {

//@+<< Usings >>
//@+node:gcross.20110511141322.2215: ** << Usings >>
using boost::algorithm::trim_if;
using boost::assign::list_of;
using boost::bind;
using boost::filesystem::exists;
using boost::filesystem::path;
using boost::function;
using boost::signals::trackable;

using std::complex;
using std::endl;
using std::ostringstream;
using std::setw;
//@-<< Usings >>

//@+others
//@+node:gcross.20110511150727.2224: ** Exceptions
//@+node:gcross.20110511150727.2225: *3* ErrorStack
ErrorStack::ErrorStack(const char* action) : Exception(constructMessageFromErrorStack(action)) {
    H5Eclear(H5E_DEFAULT);
}
//@+node:gcross.20110511190907.2336: ** Classes
//@+node:gcross.20110511190907.3505: *3* Attribute
//@+node:gcross.20110511190907.2350: *4* operator string
Attribute::operator string() const {
    if(
        assertSuccess(
            "querying string attribute existence",
            H5LTfind_attribute(id,name.c_str())
        ) == 0
    ) {
        throw NoSuchAttributeException(name);
    }

    hsize_t dummy1;
    H5T_class_t dummy2;
    size_t buffer_size;
    assertSuccess(
        "retrieving string attribute length",
        H5LTget_attribute_info(
            id,".",
            name.c_str(),
            &dummy1,&dummy2,&buffer_size
        )
    );
    scoped_array<char> buffer(new char[buffer_size]);

    assertSuccess(
        "retrieving string attribute value",
        H5LTget_attribute_string(
            id,".",
            name.c_str(),
            buffer.get()
        )
    );

    return string(buffer.get());
}
//@+node:gcross.20110511190907.2315: *4* operator unsigned int
Attribute::operator unsigned int() const {
    if(
        assertSuccess(
            "querying uint attribute existence",
            H5LTfind_attribute(id,name.c_str())
        ) == 0
    ) {
        throw NoSuchAttributeException(name);
    }

    unsigned int value;

    assertSuccess(
        "retrieving uint attribute value",
        H5LTget_attribute_uint(
            id,".",
            name.c_str(),
            &value
        )
    );

    return value;
}
//@+node:gcross.20110517164912.2525: *4* operator double
Attribute::operator double() const {
    if(
        assertSuccess(
            "querying double attribute existence",
            H5LTfind_attribute(id,name.c_str())
        ) == 0
    ) {
        throw NoSuchAttributeException(name);
    }

    double value;

    assertSuccess(
        "retrieving double attribute value",
        H5LTget_attribute_double(
            id,".",
            name.c_str(),
            &value
        )
    );

    return value;
}
//@+node:gcross.20110511190907.2351: *4* operator=(string)
void Attribute::operator=(string const& value) {
    assertSuccess(
        "writing string attribute to the object",
        H5LTset_attribute_string(id,".",name.c_str(),value.c_str())
    );
}
//@+node:gcross.20110511190907.2316: *4* operator=(unsigned int)
void Attribute::operator=(unsigned int value) {
    assertSuccess(
        "writing uint attribute to the object",
        H5LTset_attribute_uint(id,".",name.c_str(),&value,1)
    );
}
//@+node:gcross.20110517164912.2523: *4* operator=(double)
void Attribute::operator=(double value) {
    assertSuccess(
        "writing double attribute to the object",
        H5LTset_attribute_double(id,".",name.c_str(),&value,1)
    );
}
//@+node:gcross.20110511190907.2346: *3* Dataset
//@+node:gcross.20110511190907.2347: *4* (constructors)
Dataset::Dataset(Location const& location) {
    if(
        assertSuccess(
            "querying object existence",
            H5Lexists(location,location,H5P_DEFAULT)
        ) == 0
    ) {
        throw NoSuchObjectException("dataset",location);
    }

    id = assertSuccess(
        "opening dataset",
        H5Dopen(location,location,H5P_DEFAULT)
    );
}
//@+node:gcross.20110511190907.2353: *4* dimensions
vector<hsize_t> Dataset::dimensions() const {
    return Dataspace(*this).dimensions();
}
//@+node:gcross.20110511190907.2354: *4* dimensionsWithAssertedRank
vector<hsize_t> Dataset::dimensionsWithAssertedRank(unsigned int expected_rank) const {
    vector<hsize_t> dimensions(Dataspace(*this).dimensions());

    unsigned int const actual_rank = dimensions.size();
    if(expected_rank != actual_rank)
        throw WrongTensorRankException(expected_rank,actual_rank);

    return boost::move(dimensions);
}
//@+node:gcross.20110511190907.2352: *4* rank
unsigned int Dataset::rank() const {
    return Dataspace(*this).rank();
}
//@+node:gcross.20110511190907.2269: *4* readTensorData
void Dataset::readTensorData(
      hid_t const datatype
    , void* destination
) const {
    assertSuccess(
        "reading data into memory",
        H5Dread(id,datatype,H5S_ALL,H5S_ALL,H5P_DEFAULT,destination)
    );
}
//@+node:gcross.20110512151124.2508: *4* size
unsigned int Dataset::size() const {
    return Dataspace(*this).size();
}
//@+node:gcross.20110511190907.2341: *3* Dataspace
//@+node:gcross.20110512151124.2502: *4* (constructors)
Dataspace::Dataspace(Dataset const& dataset)
  : Identified(
        assertSuccess(
            "retrieving dataspace associated with the object",
            H5Dget_space(dataset.getId())
        )
    )
{}

Dataspace::Dataspace(unsigned int rank, hsize_t const* dimensions)
  : Identified(
        assertSuccess(
            "creating the dataspace for the tensor data",
            H5Screate_simple(rank,dimensions,dimensions)
        )
    )
{}
//@+node:gcross.20110512151124.2503: *4* (destructors)
Dataspace::~Dataspace() { if(id >= 0) H5Sclose(id); }
void Dataspace::close() { assertSuccess("closing dataspace",H5Sclose(id)); }
//@+node:gcross.20110512151124.2504: *4* dimensions
vector<hsize_t> Dataspace::dimensions() const {
    vector<hsize_t> dims(rank());
    assertSuccess(
        "retrieving the dimensions of the dataspace",
        H5Sget_simple_extent_dims(id,&dims.front(),NULL)
    );
    return boost::move(dims);
}
//@+node:gcross.20110512151124.2505: *4* rank
unsigned int Dataspace::rank() const {
    return assertSuccess(
        "retrieving the rank of the dataspace",
        H5Sget_simple_extent_ndims(id)
    );
}
//@+node:gcross.20110512151124.2506: *4* size
unsigned int Dataspace::size() const {
    return assertSuccess(
        "retrieving the size of the dataspace",
        H5Sget_simple_extent_npoints(id)
    );
}
//@+node:gcross.20110511190907.3663: *3* File
//@+node:gcross.20110511190907.3667: *4* (constructors)
File::File(char const* filepath)
  : Locatable(assertSuccess(
        "opening file",
        H5Fopen(filepath,H5F_ACC_RDWR,H5P_DEFAULT)
    ))
{}

File::File(CreateAt<char const* const> wrapped_filepath)
  : Locatable(assertSuccess(
        "creating file",
        H5Fcreate(*wrapped_filepath,H5F_ACC_EXCL,H5P_DEFAULT,H5P_DEFAULT)
    ))
{}
//@+node:gcross.20110511190907.3665: *4* (destructors)
File::~File() { if(id >=0) H5Fclose(id); }
void File::close() { assertSuccess("closing file",H5Fclose(id)); }
//@+node:gcross.20110511190907.3689: *4* flush
void File::flush() {
    assertSuccess(
        "flushing file",
        H5Fflush(id,H5F_SCOPE_LOCAL)
    );
}
//@+node:gcross.20110511190907.3463: *3* Group
//@+node:gcross.20110511190907.3465: *4* (constructors)
Group::Group(Location const& location) {
    if(
        assertSuccess(
            "querying object existence",
            H5Lexists(location,location,H5P_DEFAULT)
        ) == 0
    ) {
        throw NoSuchObjectException("group",location);
    }

    id = assertSuccess(
        "opening group",
        H5Gopen(location,location,H5P_DEFAULT)
    );
}

Group::Group(CreateAt<Location const> location) {
    id = assertSuccess(
        "creating group",
        H5Gcreate(
            *location,*location,
            H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT
        )
    );
}
//@+node:gcross.20110511190907.2289: *3* Object
//@+node:gcross.20110511190907.2348: *4* (destructors)
Object::~Object() { if(id >= 0) H5Oclose(id); }
void Object::close() { assertSuccess("closing object",H5Oclose(id)); }
//@+node:gcross.20110511190907.2394: *3* Properties
//@+node:gcross.20110511190907.3791: *4* (constructors)
Properties::Properties(hid_t const cls_id)
 : Identified(
    assertSuccess(
        "creating property list",
        H5Pcreate(cls_id)
    )
   )
{}
//@+node:gcross.20110511190907.3792: *4* (destructors)
Properties::~Properties() { if(id >= 0) H5Pclose(id); }
void Properties::close() { assertSuccess("closing properties",H5Pclose(id)); }
//@+node:gcross.20110511190907.2259: ** Type functions
//@+node:gcross.20110511141322.2218: *3* datatypeOf
hid_t datatypeOf<complex<double> >::get() {
    static struct ConstructDatatype {
        hid_t datatype;
        ConstructDatatype() {
            datatype = H5Tcreate(H5T_COMPOUND,sizeof(complex<double>));
            complex<double> c;
            H5Tinsert(datatype,"real",0,H5T_NATIVE_DOUBLE);
            H5Tinsert(datatype,"imag",sizeof(double),H5T_NATIVE_DOUBLE);
        }
    } construct_datatype;
    return construct_datatype.datatype;
}
//@+node:gcross.20110511141322.2216: ** Functions
//@+node:gcross.20110511150727.2227: *3* constructMessageFromErrorStack
static herr_t error_stack_walker_callback(unsigned const n, H5E_error2_t const *err_desc, void* client_data) {
    ostringstream& message = *(ostringstream*)client_data;

    size_t const class_name_size = H5Eget_class_name(err_desc->cls_id,NULL,0);
    scoped_array<char> class_name(new char[class_name_size]);
    H5Eget_class_name(err_desc->cls_id,class_name.get(),class_name_size);

    size_t const major_message_size = H5Eget_msg(err_desc->maj_num,NULL,NULL,0);
    scoped_array<char> major_message(new char[major_message_size]);
    H5Eget_msg(err_desc->maj_num,NULL,major_message.get(),major_message_size);

    size_t const minor_message_size = H5Eget_msg(err_desc->min_num,NULL,NULL,0);
    scoped_array<char> minor_message(new char[minor_message_size]);
    H5Eget_msg(err_desc->min_num,NULL,minor_message.get(),minor_message_size);

    message << "    " << setw(3) << n << ": " << err_desc->file_name << " line " << err_desc->line << " in " << err_desc->func_name << "()" << endl;
    message << "    " << " - class:  " << class_name << endl;
    message << "    " << " - major: " << major_message << endl;
    message << "    " << " - minor: " << minor_message << endl;

    return 0;
}

string constructMessageFromErrorStack(string const& action) {
    ostringstream message;
    message << "Error occurred while " << action << ":\n";
    H5Ewalk(H5E_DEFAULT,H5E_WALK_UPWARD,&error_stack_walker_callback,&message);
    return message.str();
}
//@+node:gcross.20110510004855.3254: *3* writeTensorData
Dataset writeTensorData(
      Location const& location
    , unsigned int const rank
    , hsize_t const* dimensions
    , hid_t const datatype
    , void const* data
) {    
    Dataspace const dataspace(rank,dimensions);

    Dataset dataset(takeOwnershipOf(
        assertSuccess(
            "creating the dataset for the tensor data",
            H5Dcreate(location,location,datatype,dataspace.getId(),H5P_DEFAULT,H5P_DEFAULT,H5P_DEFAULT)
        )
    ));

    assertSuccess(
        "writing the tensor data to the dataset",
        H5Dwrite(dataset.getId(),datatype,H5S_ALL,H5S_ALL,H5P_DEFAULT,data)
    );

    return boost::move(dataset);
}
//@+node:gcross.20110511190907.2301: ** I/O Operators
//@+node:gcross.20110511190907.3531: *3* Operator
//@+node:gcross.20110511190907.3532: *4* >>
void operator >> (Location const& location, Operator& operator_sites) {
    operator_sites = constructOperatorFrom(
        readUniqueOperatorSites(Group(location / "sites")),
        Dataset(location / "sequence").readTensorData<unsigned int>()
    );
}
//@+node:gcross.20110511190907.3533: *4* <<
Group operator << (Location const& location, Operator const& operator_sites) {
    Group group(createAt(location));

    vector<shared_ptr<OperatorSite const> > unique_operator_sites;
    vector<unsigned int> sequence;

    deconstructOperatorTo(operator_sites,unique_operator_sites,sequence);

    Group(createAt(location / "sites")) << rangeOf(unique_operator_sites | indirected);

    writeVectorData(location / "sequence",sequence);

    return boost::move(group);
}
//@+node:gcross.20110511190907.2302: *3* OperatorSite
//@+node:gcross.20110511190907.2304: *4* <<
Group operator<<(Location const& location, OperatorSite const& operator_site_tensor) {
    Group object(createAt(location));
    object["left dimension"] = operator_site_tensor.leftDimension(as_unsigned_integer);
    object["right dimension"] = operator_site_tensor.rightDimension(as_unsigned_integer);
    writeTensorData(
        location / "matrices",
        operator_site_tensor.matrixDataDimensions(),
        (complex<double> const*)operator_site_tensor
    );
    writeTensorData(
        location / "indices",
        operator_site_tensor.indexDataDimensions(),
        (uint32_t const*)operator_site_tensor
    );
    return boost::move(object);
}
//@+node:gcross.20110511190907.2311: *4* >>
Group operator>>(Location const& location, OperatorSite& operator_site_tensor) {
    Group object(location);
    Dataset const
        matrices_dataset(location / "matrices"),
        indices_dataset(location / "indices");

    vector<hsize_t> matrices_dimensions = matrices_dataset. dimensionsWithAssertedRank(3);
    vector<hsize_t> indices_dimensions = indices_dataset.dimensionsWithAssertedRank(2);

    if(matrices_dimensions[0] != indices_dimensions[0]
    || matrices_dimensions[1] != matrices_dimensions[2]
    || indices_dimensions[1] != 2
    ) throw InconsistentTensorDimensions();

    {
        OperatorSite new_operator_site_tensor(
            matrices_dimensions[0],
            PhysicalDimension(matrices_dimensions[1]),
            LeftDimension(object["left dimension"]),
            RightDimension(object["right dimension"])
        );
        operator_site_tensor = boost::move(new_operator_site_tensor);
    }

    matrices_dataset.readTensorData<complex<double> >(operator_site_tensor);
    indices_dataset.readTensorData<uint32_t>(operator_site_tensor);

    return boost::move(object);
}
//@+node:gcross.20110511190907.3512: *3* State
//@+node:gcross.20110511190907.3513: *4* <<
Group operator<<(Location const& location, State const& state) {
    Group group(createAt(location));
    group["size"] = state.numberOfSites();
    LocationIterator iter(location);
    iter << state.getFirstSite() << rangeOf(state.getRestSites());
    return boost::move(group);
}
//@+node:gcross.20110511190907.3516: *4* >>
Group operator>>(Location const& location, State& state) {
    Group group(location);
    unsigned int size = group["size"];
    LocationIterator iter(location);

    StateSite<Middle> first_site;
    iter >> first_site;

    vector<StateSite<Right> > rest_sites(size-1);
    for_each(rest_sites,lambda::var(iter) >> lambda::_1);

    state = State(boost::move(first_site),boost::move(rest_sites));

    return boost::move(group);
}
//@+node:gcross.20110511190907.3646: ** Formats
//@+others
//@+node:gcross.20110511190907.3650: *3* Input
static Operator readOperator(optional<string> const& maybe_filename, optional<string> const& maybe_location) {
    assert(maybe_filename);
    File file(maybe_filename->c_str());

    Location location = file.getLocation();

    if(maybe_location) {
        BOOST_FOREACH(string const& name, LocationSlashTokenizer(maybe_location.get())) {
            location /= name;
        }
    }

    Operator hamiltonian;
    location >> hamiltonian;
    return boost::move(hamiltonian);
}
//@+node:gcross.20110511190907.3673: *3* Output
struct Outputter : public Destructable, public trackable {
    Chain const& chain;
    File file;

    unsigned int number_of_levels;

    Location levels_location;
    optional<Location> maybe_states_location;

    Outputter(
        optional<string> const& maybe_filename
      , optional<string> const& maybe_location
      , bool output_states
      , bool overwrite
      , Chain& chain
    )
      : chain(chain)
      , number_of_levels(0)
    {
        assert(maybe_filename);
        const string& filename = *maybe_filename;

        if(exists(path(filename))) {
            file = File(filename.c_str());
        } else {
            file = File(createAt(filename.c_str()));
        }

        string specified_location;
        if(maybe_location) {
            string location = *maybe_location;
            trim_if(location,isSlash);
        }

        Location location = file / specified_location;

        // This would really be a lot easier if HDF would just let us open
        // the root of the file as a group, as I had been (naively) expecting
        // it to when I first wrote this constructor...

        function<void (char const*)> remove;
        hid_t parent_id;

        Group group;
        if(!specified_location.empty()) {
            BOOST_FOREACH(string const& name, LocationSlashTokenizer(specified_location)) {
                location /= name;
                if(!location.exists()) Group(createAt(location));
            }
            Group g(location);
            group = boost::move(g);
            remove = bind(&Group::remove,&group,_1);
            parent_id = group.getId();
        } else {
            remove = bind(&File::remove,&file,_1);
            parent_id = file.getId();
        }

        Location
            configuration_location = location / "configuration",
            states_location = location / "states";

        levels_location = location / "levels";

        if(!overwrite && (
               configuration_location.exists()
            || levels_location.exists()
            || states_location.exists()
        )) throw OutputLocationAlreadyExists(filename,*maybe_location);

        {
            if(configuration_location.exists()) remove("configuration");
            Group configuration(createAt(location / "configuration"));
            configuration["site convergence tolerance"] = chain.options.site_convergence_threshold;
            configuration["sweep convergence tolerance"] = chain.options.sweep_convergence_threshold;
            configuration["chain convergence tolerance"] = chain.options.chain_convergence_threshold;
            configuration["sanity check tolerance"] = chain.options.sanity_check_threshold;
        }


        {        
            if(levels_location.exists()) remove("levels");

            Dataspace dataspace;
            {
                hsize_t const current_dimension = 0, maximum_dimension = H5S_UNLIMITED;
                dataspace = Dataspace(takeOwnershipOf(
                    assertSuccess(
                        "creating levels dataspace",
                        H5Screate_simple(1,&current_dimension,&maximum_dimension)
                    )
                ));
            }

            hsize_t const chunk_size = 16;
            Properties dataset_create_properties(H5P_DATASET_CREATE);
            assertSuccess(
                "setting levels dataset to be chunked",
                H5Pset_chunk(dataset_create_properties.getId(),1u,&chunk_size)
            );

            Dataset(takeOwnershipOf(assertSuccess(
                "creating levels dataset",
                H5Dcreate(
                    parent_id,"levels",
                    datatypeOf<double>::get(),
                    dataspace.getId(),
                    H5P_DEFAULT,
                    dataset_create_properties.getId(), H5P_DEFAULT
                )
            )));
        }

        if(output_states) {
            if(states_location.exists()) remove("states");
            Group(createAt(states_location))["size"] = 0u;
            maybe_states_location = states_location;
        } else maybe_states_location = none;

        chain.signalChainOptimized.connect(boost::bind(&Outputter::reactToChainOptimizedSignal,this));
    }

    virtual ~Outputter() {}

    void reactToChainOptimizedSignal() {
        hsize_t const index = number_of_levels;
        ++number_of_levels;

        {
            double const energy = chain.getEnergy();

            hsize_t const new_extent = number_of_levels;
            Dataset levels(levels_location);  
            assertSuccess(
                "extending levels dimensions",
                H5Dset_extent(levels.getId(),&new_extent)
            );

            Dataspace levels_space(levels);
            assertSuccess(
                "selecting last (new) entry in the levels list",
                H5Sselect_elements(
                    levels_space.getId(),
                    H5S_SELECT_SET,
                    1,
                    &index
                )
            );

            hsize_t const one = 1;
            Dataspace memory_space(1,&one);

            assertSuccess(
                "writing new level energy",
                H5Dwrite(
                    levels.getId(),
                    datatypeOf<double>::get(),
                    memory_space.getId(),levels_space.getId(),
                    H5P_DEFAULT,
                    &energy
                )
            );
        }

        if(maybe_states_location) {
            Location const& states_location = *maybe_states_location;
            Group states(states_location);
            {
                Group state(createAt(states.begin()[index]));
                LocationIterator state_sites = state.begin();
                chain.writeStateTo(state_sites);
                state["size"] = chain.number_of_sites;
            }
            states["size"] = number_of_levels;
        }

        file.flush();
    }
};

auto_ptr<Destructable const> connectToChain(
    optional<string> const& maybe_filename
  , optional<string> const& maybe_location
  , bool output_states
  , bool overwrite
  , Chain& chain
) {
    return auto_ptr<Destructable const>(new Outputter(maybe_filename,maybe_location,output_states,overwrite,chain));
}
//@-others

void installFormat() {
    static InputFormat input_format("hdf","HDF format",false,true,list_of("hdf")("hdf5")("h5"),readOperator);
    static OutputFormat output_format("hdf","HDF format",false,true,list_of("hdf")("hdf5")("h5"),true,connectToChain);
}
//@-others

} }
//@-leo
