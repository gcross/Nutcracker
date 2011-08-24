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
#include <boost/range/adaptor/indirected.hpp>
#include <boost/signals/trackable.hpp>
#include <complex>
#include <hdf++/container.hpp>
#include <hdf++/dataspace.hpp>
#include <hdf++/file.hpp>
#include <iomanip>
#include <ostream>

#include "chain.hpp"
#include "hdf.hpp"
#include "io.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110511141322.2215: ** << Usings >>
using boost::adaptors::indirected;
using boost::algorithm::trim_if;
using boost::assign::list_of;
using boost::bind;
using boost::filesystem::exists;
using boost::filesystem::path;
using boost::function;
using boost::none;
using boost::signals::trackable;

using std::complex;
using std::endl;
using std::ostringstream;
using std::setw;

using HDF::assertSuccess;
using HDF::Container;
using HDF::DatasetCreationProperties;
using HDF::Dataspace;
using HDF::datatypeOf;
using HDF::FailIfFileExisting;
using HDF::File;
using HDF::LinkCreationProperties;
using HDF::Location;
using HDF::LocationIterator;
using HDF::OpenReadOnly;
using HDF::OpenReadWrite;
using HDF::rangeOf;
//@-<< Usings >>

namespace Nutcracker { namespace HDF {

//@+others
//@+node:gcross.20110511190907.3646: ** Formats
//@+others
//@+node:gcross.20110511190907.3650: *3* Input
static Operator readOperator(optional<string> const& maybe_filename, optional<string> const& maybe_location) {
    assert(maybe_filename);
    File file(maybe_filename->c_str(),OpenReadOnly);

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
            file = File(filename.c_str(),OpenReadWrite);
        } else {
            file = File(filename.c_str(),FailIfFileExisting);
        }

        Location location = file.getLocation();
        if(maybe_location) {
            location /= *maybe_location;
        }

        Container group =
            location.exists()
                ? Container(location)
                : Container(createAt(location),LinkCreationProperties().createMissingIntermediateGroups());

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
            if(configuration_location.exists()) group.remove("configuration");
            Group configuration(createAt(configuration_location));
            configuration["site convergence tolerance"] = chain.site_convergence_threshold;
            configuration["sweep convergence tolerance"] = chain.sweep_convergence_threshold;
            configuration["chain convergence tolerance"] = chain.chain_convergence_threshold;
            configuration["sanity check tolerance"] = chain.sanity_check_threshold;
        }


        {        
            if(levels_location.exists()) group.remove("levels");

            Dataset(
                createAt(levels_location),
                datatypeOf<double>::get(),
                Dataspace(0,H5S_UNLIMITED),
                none,
                DatasetCreationProperties().setChunkSize(16)
            );
        }

        if(output_states) {
            if(states_location.exists()) group.remove("states");
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

            Dataset levels(levels_location);
            levels.resize(number_of_levels);

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

            levels.write(
                &energy,
                Dataspace(1),
                levels_space
            );
        }

        if(maybe_states_location) {
            Location const& states_location = *maybe_states_location;
            GroupArray states(states_location);
            {
                GroupArray state(createAt(states.begin()[index]));
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

//@+<< Outside namespace >>
//@+node:gcross.20110524225044.2432: ** << Outside namespace >>
using namespace HDF;
using namespace Nutcracker;

//@+others
//@+node:gcross.20110511190907.2301: *3* I/O Operators
//@+node:gcross.20110511190907.3531: *4* Operator
//@+node:gcross.20110511190907.3532: *5* >>
void operator >> (Location const& location, Operator& operator_sites) {
    operator_sites = constructOperatorFrom(
        readUniqueOperatorSites(GroupArray(location / "sites")),
        Dataset(location / "sequence").readVector<unsigned int>()
    );
}
//@+node:gcross.20110511190907.3533: *5* <<
Container operator << (Location const& location, Operator const& operator_sites) {
    Container group(createAt(location));

    vector<shared_ptr<OperatorSite const> > unique_operator_sites;
    vector<unsigned int> sequence;

    deconstructOperatorTo(operator_sites,unique_operator_sites,sequence);

    GroupArray(createAt(location / "sites")) << rangeOf(unique_operator_sites | indirected);

    Dataset(
        createAt(location / "sequence"),
        sequence.size(),
        &sequence.front()
    );

    return group;
}
//@+node:gcross.20110511190907.2302: *4* OperatorSite
//@+node:gcross.20110511190907.2304: *5* <<
Group operator<<(Location const& location, OperatorSite const& operator_site_tensor) {
    Group object(createAt(location));
    object["left dimension"] = operator_site_tensor.leftDimension();
    object["right dimension"] = operator_site_tensor.rightDimension();
    Dataset(
        createAt(location / "matrices"),
        rangeOf(operator_site_tensor.matrixDataDimensions()),
        (complex<double> const*)operator_site_tensor
    );
    Dataset(
        createAt(location / "indices"),
        rangeOf(operator_site_tensor.indexDataDimensions()),
        (uint32_t const*)operator_site_tensor
    );
    return object;
}
//@+node:gcross.20110511190907.2311: *5* >>
Group operator>>(Location const& location, OperatorSite& operator_site_tensor) {
    Group object(location);
    Dataset const
        matrices_dataset(location / "matrices"),
        indices_dataset(location / "indices");

    std::vector<hsize_t> matrices_dimensions = matrices_dataset.dimensionsWithAssertedRank(3);
    std::vector<hsize_t> indices_dimensions = indices_dataset.dimensionsWithAssertedRank(2);

    if(matrices_dimensions[0] != indices_dimensions[0]
    || matrices_dimensions[1] != matrices_dimensions[2]
    || indices_dimensions[1] != 2
    ) throw Nutcracker::HDF::InconsistentTensorDimensions();

    {
        OperatorSite new_operator_site_tensor(
            matrices_dimensions[0],
            PhysicalDimension(matrices_dimensions[1]),
            LeftDimension(object["left dimension"]),
            RightDimension(object["right dimension"])
        );
        operator_site_tensor = boost::move(new_operator_site_tensor);
    }

    matrices_dataset.read<complex<double> >(operator_site_tensor);
    indices_dataset.read<uint32_t>(operator_site_tensor);

    return object;
}
//@+node:gcross.20110511190907.3512: *4* State
//@+node:gcross.20110511190907.3513: *5* <<
GroupArray operator<<(Location const& location, State const& state) {
    GroupArray group(createAt(location));
    group["size"] = state.numberOfSites();
    LocationIterator iter(group);
    iter << state.getFirstSite() << rangeOf(state.getRestSites());
    return group;
}
//@+node:gcross.20110511190907.3516: *5* >>
GroupArray operator>>(Location const& location, State& state) {
    GroupArray group(location);
    unsigned int size = group["size"];
    LocationIterator iter(group);

    StateSite<Middle> first_site;
    iter >> first_site;

    vector<StateSite<Right> > rest_sites(size-1);
    for_each(rest_sites,lambda::var(iter) >> lambda::_1);

    state = State(boost::move(first_site),boost::move(rest_sites));

    return group;
}
//@-others
//@-<< Outside namespace >>
//@-leo
