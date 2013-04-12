#include "nutcracker/io.hpp"

namespace Nutcracker {


const char* input_format_type_name = "input";
const char* output_format_type_name = "output";

FormatInstaller const FormatInstaller::_;
namespace HDF { void installFormat(); }
namespace Protobuf { void installFormat(); }
void installYAMLFormat();

FormatInstaller::FormatInstaller() {
    HDF::installFormat();
    Protobuf::installFormat();
    installYAMLFormat();
    InputFormat::default_format = &InputFormat::lookupName("yaml");
    OutputFormat::default_format = &OutputFormat::lookupName("yaml");
}
vector<string> FormatTypeException::getAcceptedFormatNames() const {
    if(format_type_name == "input") return InputFormat::listNames();
    if(format_type_name == "output") return OutputFormat::listNames();
    return vector<string>();
}
void deconstructOperatorTo(
    Operator const& operator_sites
  , vector<shared_ptr<OperatorSite const> >& unique_operator_sites
  , vector<unsigned int>& sequence
) {
    unique_operator_sites.clear();
    sequence.clear();
    sequence.reserve(operator_sites.size());

    typedef map<shared_ptr<OperatorSite const>,unsigned int> SequenceNumberMap;
    SequenceNumberMap sequence_number_map;

    unsigned int next_sequence_number = 0;
    BOOST_FOREACH(shared_ptr<OperatorSite const> const operator_site_ptr, operator_sites) {
        SequenceNumberMap::iterator sequence_number_iterator = sequence_number_map.find(operator_site_ptr);
        if(sequence_number_iterator == sequence_number_map.end()) {
            sequence.push_back(next_sequence_number);
            sequence_number_map.insert(make_pair(operator_site_ptr,next_sequence_number++));
            unique_operator_sites.push_back(operator_site_ptr);
        } else {
            sequence.push_back(sequence_number_iterator->second);
        }
    }
}

}
