//@+leo-ver=5-thin
//@+node:gcross.20110430163445.2609: * @file yaml.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110430163445.2610: ** << License >>
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
//@+node:gcross.20110430163445.2611: ** << Documentation >>
/*!
\file yaml.cpp
\brief YAML serialization functions
*/
//@-<< Documentation >>

//@+<< Includes >>
//@+node:gcross.20110430163445.2612: ** << Includes >>
#include <boost/assign/list_of.hpp>
#include <boost/bind.hpp>
#include <boost/filesystem.hpp>
#include <boost/signals/trackable.hpp>
#include <iomanip>
#include <iostream>
#include <fstream>

#include "chain.hpp"
#include "io.hpp"
#include "yaml.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110430163445.2613: ** << Usings >>
using boost::assign::list_of;
using boost::filesystem::exists;
using boost::filesystem::path;
using boost::signals::trackable;

using std::endl;
using std::ifstream;
using std::ofstream;
using std::ostream;
using std::setprecision;

using YAML::BeginMap;
using YAML::BeginSeq;
using YAML::Emitter;
using YAML::EndMap;
using YAML::EndSeq;
using YAML::Flow;
using YAML::Iterator;
using YAML::Key;
using YAML::Node;
using YAML::Parser;
using YAML::Value;
//@-<< Usings >>

//@+others
//@+node:gcross.20110430163445.2632: ** I/O Operators
//@+node:gcross.20110430163445.2647: *3* Operator
//@+node:gcross.20110430163445.2648: *4* >>
void operator >> (Node const& node, Operator& operator_sites) {
    vector<shared_ptr<OperatorSite const> > unique_operator_sites(readUniqueOperatorSites(node["sites"]));

    vector<unsigned int> sequence;
    BOOST_FOREACH(Node const& node, node["sequence"]) {
        unsigned int index;
        node >> index;
        sequence.push_back(index-1);
    }

    operator_sites = constructOperatorFrom(unique_operator_sites,sequence);
}
//@+node:gcross.20110430163445.2649: *4* <<
Emitter& operator << (Emitter& out, Operator const& operator_sites) {
    vector<shared_ptr<OperatorSite const> > unique_operator_sites;
    vector<unsigned int> sequence;

    deconstructOperatorTo(operator_sites,unique_operator_sites,sequence);

    out << BeginMap;
    out << Key << "sequence" << Value;
    out << Flow << BeginSeq;
    BOOST_FOREACH(unsigned int index, sequence) {
        out << index+1;
    }
    out << EndSeq;
    out << Key << "sites" << Value;
    out << BeginSeq;
    BOOST_FOREACH(shared_ptr<OperatorSite const> const operator_site_ptr, unique_operator_sites) {
        out << *operator_site_ptr;
    }
    out << EndSeq;
    out << EndMap;
    return out;
}
//@+node:gcross.20110430163445.2636: *3* OperatorLink
//@+node:gcross.20110430163445.2637: *4* >>
void operator >> (Node const& node, OperatorLink& link) {
    node["from"] >> link.from;
    node["to"] >> link.to;
    Node const& data = node["data"];
    unsigned int nsq = data.size(), n = (unsigned int)sqrt(nsq);
    assert(n*n == nsq);
    link.matrix.resize(n,n);
    Iterator node_iter = data.begin();
    Matrix::array_type::iterator matrix_iter = link.matrix.data().begin();
    REPEAT(n*n) {
        using namespace std;
        using namespace YAML;
        *node_iter++ >> *matrix_iter++;
    }
}
//@+node:gcross.20110430163445.2638: *4* <<
Emitter& operator << (Emitter& out, OperatorLink const& link) {
    out << BeginMap;
    out << Key << "from" << Value << link.from;
    out << Key << "to" << Value << link.to;
    out << Key << "data" << Value;
    {
        out << Flow << BeginSeq;
        BOOST_FOREACH(complex<double> const x, link.matrix.data()) { out << x; }
        out << EndSeq;
    }
    out << EndMap;
    return out;
}
//@+node:gcross.20110430163445.2650: *3* OperatorSite
//@+node:gcross.20110430163445.2651: *4* >>
void operator >> (Node const& node, OperatorSite& output_operator_site) {
    unsigned int physical_dimension, left_dimension, right_dimension;
    node["physical dimension"] >> physical_dimension;
    node["left dimension"] >> left_dimension;
    node["right dimension"] >> right_dimension;
    Node const& matrices = node["matrices"];
    unsigned int const number_of_matrices = matrices.size();

    OperatorSite operator_site
        (number_of_matrices
        ,PhysicalDimension(physical_dimension)
        ,LeftDimension(left_dimension)
        ,RightDimension(right_dimension)
        );

    unsigned int const matrix_length = physical_dimension*physical_dimension;
    complex<double>* matrix_data = operator_site;
    uint32_t* index_data = operator_site;
    Iterator matrix_iterator = matrices.begin();
    REPEAT(number_of_matrices) {
        Node const& matrix = *matrix_iterator++;
        unsigned int from, to;
        matrix["from"] >> from;
        matrix["to"] >> to;
        assert(from >= 1);
        assert(from <= left_dimension);
        assert(to >= 1);
        assert(to <= right_dimension);
        *index_data++ = from;
        *index_data++ = to;
        Node const& data = matrix["data"];
        assert(data.size() == matrix_length);
        Iterator data_iterator = data.begin();
        REPEAT(matrix_length) { *data_iterator++ >> *matrix_data++; }
    }

    output_operator_site = boost::move(operator_site);
}
//@+node:gcross.20110430163445.2652: *4* <<
Emitter& operator << (Emitter& out, OperatorSite const& operator_site) {
    out << BeginMap;
    out << Key << "physical dimension" << Value << operator_site.physicalDimension(as_unsigned_integer);
    out << Key << "left dimension" << Value << operator_site.leftDimension(as_unsigned_integer);
    out << Key << "right dimension" << Value << operator_site.rightDimension(as_unsigned_integer);
    out << Key << "matrices" << Value;
    {
        out << BeginSeq;
        unsigned int n = operator_site.physicalDimension(as_unsigned_integer), nsq = n*n;
        complex<double> const* matrix_data = operator_site;
        uint32_t const* index_data = operator_site;
        REPEAT(operator_site.numberOfMatrices()) {
            out << BeginMap;
            out << Key << "from" << Value << *index_data++;
            out << Key << "to" << Value << *index_data++;
            out << Key << "data" << Value;
            {
                out << Flow << BeginSeq;
                REPEAT(nsq) {
                    out << *matrix_data++;
                }
                out << EndSeq;
            }
            out << EndMap;
        }
        out << EndSeq;
    }
    out << EndMap;
    return out;
}
//@+node:gcross.20110511190907.3617: ** Formats
//@+others
//@+node:gcross.20110511190907.3637: *3* Input
static Operator readOperator(optional<string> const& maybe_filename, optional<string> const& maybe_location) {
    Node root;
    if(maybe_filename) {
        ifstream in(maybe_filename->c_str());
        Parser parser(in);
        parser.GetNextDocument(root);
    } else {
        Parser parser(std::cin);
        parser.GetNextDocument(root);
    }

    Node const* node = &root;

    if(maybe_location) {
        string const& location = maybe_location.get();
        BOOST_FOREACH(string const& name, LocationSlashTokenizer(location)) {
            Node const* nested_node = node->FindValue(name.c_str());
            if(nested_node == NULL) throw NoSuchLocationError(location);
            node = nested_node;
        }
    }

    Operator hamiltonian;
    (*node) >> hamiltonian;
    return boost::move(hamiltonian);
}
//@+node:gcross.20110511190907.3638: *3* Output
struct YAMLOutputter : public Destructable, public trackable {
    Chain const& chain;
    ofstream file;
    ostream& out;
    unsigned const digits_of_precision;

    YAMLOutputter(
        optional<string> const& maybe_filename
      , optional<string> const& maybe_location
      , bool output_states
      , bool overwrite
      , Chain& chain
    )
      : chain(chain)
      , out(maybe_filename ? file : std::cout)
      , digits_of_precision(computeDigitsOfPrecision(chain.options.chain_convergence_threshold))
    {
        assert(!maybe_location);
        assert(!output_states);

        if(maybe_filename) {
            const string& filename = *maybe_filename;
            if(!overwrite && exists(path(filename))) throw OutputFileAlreadyExists(filename);
            file.open(filename.c_str());
        }

        out
            << "Configuration:" << endl
            << "  site convergence tolerance: " << chain.options.site_convergence_threshold << endl
            << "  sweep convergence tolerance: " << chain.options.sweep_convergence_threshold << endl
            << "  chain convergence tolerance: " << chain.options.chain_convergence_threshold << endl
            << "  sanity check threshold: " << chain.options.sanity_check_threshold << endl
        ;

        out << "Energy levels:" << endl;
        out.flush();

        chain.signalChainOptimized.connect(boost::bind(&YAMLOutputter::printChainEnergy,this));
    }

    virtual ~YAMLOutputter() {
        out.flush();
        if(file.is_open()) { file.close(); }
    }

    void printChainEnergy() {
        out << "  - " << setprecision(digits_of_precision) << chain.getEnergy() << endl;
        out.flush();
    }
};

auto_ptr<Destructable const> connectToChain(
    optional<string> const& maybe_filename
  , optional<string> const& maybe_location
  , bool output_states
  , bool overwrite
  , Chain& chain
) {
    return auto_ptr<Destructable const>(new YAMLOutputter(maybe_filename,maybe_location,output_states,overwrite,chain));
}
//@-others

void installYAMLFormat() {
    static InputFormat yaml_input_format("yaml","YAML format",true,true,list_of("yaml"),readOperator);
    static OutputFormat yaml_output_format("yaml","YAML format",true,false,list_of("yaml"),false,connectToChain);
}
//@-others

}
//@-leo
