/*!
\file yaml.cpp
\brief YAML serialization functions
*/

#include <boost/assign/list_of.hpp>
#include <boost/bind.hpp>
#include <boost/filesystem.hpp>
#include <boost/signals/trackable.hpp>
#include <iomanip>
#include <iostream>
#include <fstream>

#include "nutcracker/chain.hpp"
#include "nutcracker/io.hpp"
#include "nutcracker/yaml.hpp"

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

namespace Nutcracker {

YAMLInputError::YAMLInputError(YAML::Mark const& mark, string const& message)
  : std::runtime_error((format("Error in input line %1% column %2%:\n%3%") % mark.line % mark.column % message).str())
  , mark(mark)
{}
NonSquareMatrixYAMLInputError::NonSquareMatrixYAMLInputError(YAML::Mark const& mark, unsigned int const length)
  : YAMLInputError(mark,(format("Matrix data length %1% is not a square.") % length).str())
  , length(length)
{}
IndexTooLowYAMLInputError::IndexTooLowYAMLInputError(YAML::Mark const& mark, string const& name, int const index)
  : YAMLInputError(mark,(format("The '%1%' index is too low. (%2% < 1)") % name % index).str())
  , name(name)
  , index(index)
{}

IndexTooLowYAMLInputError::~IndexTooLowYAMLInputError() throw () {}
IndexTooHighYAMLInputError::IndexTooHighYAMLInputError(YAML::Mark const& mark, string const& name, unsigned int const index, unsigned int const dimension)
  : YAMLInputError(mark,(format("The '%1%' index is too high. (%2% > %3%)") % name % index % dimension).str())
  , name(name)
  , index(index)
  , dimension(dimension)
{}

IndexTooHighYAMLInputError::~IndexTooHighYAMLInputError() throw () {}
WrongDataLengthYAMLInputError::WrongDataLengthYAMLInputError(YAML::Mark const& mark, unsigned int const length, unsigned int const correct_length)
  : YAMLInputError(mark,(format("The length of the data (%1%) does not match the correct length (%2%).") % length % correct_length).str())
  , length(length)
  , correct_length(correct_length)
{}
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
      , digits_of_precision(computeDigitsOfPrecision(chain.chain_convergence_threshold))
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
            << "  site convergence tolerance: " << chain.site_convergence_threshold << endl
            << "  sweep convergence tolerance: " << chain.sweep_convergence_threshold << endl
            << "  chain convergence tolerance: " << chain.chain_convergence_threshold << endl
            << "  sanity check threshold: " << chain.sanity_check_threshold << endl
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

void installYAMLFormat() {
    static InputFormat yaml_input_format("yaml","YAML format",true,true,list_of("yaml"),readOperator);
    static OutputFormat yaml_output_format("yaml","YAML format",true,false,list_of("yaml"),false,connectToChain);
}

}

using namespace Nutcracker;
using namespace std;

namespace YAML {

void operator >> (Node const& node, Operator& operator_sites) {
    Nutcracker::vector<shared_ptr<OperatorSite const> > unique_operator_sites(readUniqueOperatorSites(node["sites"]));

    Nutcracker::vector<unsigned int> sequence;
    BOOST_FOREACH(Node const& node, node["sequence"]) {
        unsigned int index;
        node >> index;
        sequence.push_back(index-1);
    }

    try {
        operator_sites = constructOperatorFrom(unique_operator_sites,sequence);
    } catch(NoSuchOperatorSiteNumberError& e) {
        ++e.index;
        throw e;
    }
}
Emitter& operator << (Emitter& out, Operator const& operator_sites) {
    Nutcracker::vector<shared_ptr<OperatorSite const> > unique_operator_sites;
    Nutcracker::vector<unsigned int> sequence;

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
void operator >> (Node const& node, OperatorSiteLink& link) {
    node["from"] >> link.from;
    node["to"] >> link.to;
    Node const& data = node["data"];
    unsigned int const nsq = data.size(), n = (unsigned int)sqrt(nsq);
    if(n*n != nsq) throw NonSquareMatrixYAMLInputError(data.GetMark(),nsq);
    Matrix* matrix = new Matrix(n,n);
    Iterator node_iter = data.begin();
    Matrix::array_type::iterator matrix_iter = matrix->data().begin();
    REPEAT(n*n) {
        using namespace std;
        using namespace YAML;
        *node_iter++ >> *matrix_iter++;
    }
    link.label = MatrixConstPtr(matrix);
}
Emitter& operator << (Emitter& out, OperatorSiteLink const& link) {
    out << BeginMap;
    out << Key << "from" << Value << link.from;
    out << Key << "to" << Value << link.to;
    out << Key << "data" << Value;
    {
        out << Flow << BeginSeq;
        BOOST_FOREACH(complex<double> const x, link.label->data()) { out << x; }
        out << EndSeq;
    }
    out << EndMap;
    return out;
}
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
        if(from < 1) throw IndexTooLowYAMLInputError(matrix["from"].GetMark(),"from",from);
        if(from > left_dimension) throw IndexTooHighYAMLInputError(matrix["from"].GetMark(),"from",from,left_dimension);
        matrix["to"] >> to;
        if(to < 1) throw IndexTooLowYAMLInputError(matrix["to"].GetMark(),"to",to);
        if(to > right_dimension) throw IndexTooHighYAMLInputError(matrix["to"].GetMark(),"to",to,right_dimension);
        *index_data++ = from;
        *index_data++ = to;
        Node const& data = matrix["data"];
        if(data.size() != matrix_length) throw WrongDataLengthYAMLInputError(data.GetMark(),data.size(),matrix_length);
        Iterator data_iterator = data.begin();
        REPEAT(matrix_length) { *data_iterator++ >> *matrix_data++; }
    }

    output_operator_site = boost::move(operator_site);
}
Emitter& operator << (Emitter& out, OperatorSite const& operator_site) {
    out << BeginMap;
    out << Key << "physical dimension" << Value << operator_site.physicalDimension();
    out << Key << "left dimension" << Value << operator_site.leftDimension();
    out << Key << "right dimension" << Value << operator_site.rightDimension();
    out << Key << "matrices" << Value;
    {
        out << BeginSeq;
        unsigned int n = operator_site.physicalDimension(), nsq = n*n;
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

}

