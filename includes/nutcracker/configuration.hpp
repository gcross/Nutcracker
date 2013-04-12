#ifndef NUTCRACKER_CONFIGURATION_HPP
#define NUTCRACKER_CONFIGURATION_HPP

#include <boost/optional.hpp>
#include <boost/program_options.hpp>
#include <string>

#include "nutcracker/chain.hpp"
#include "nutcracker/io.hpp"

namespace Nutcracker {

using boost::optional;
using boost::program_options::options_description;

using std::string;

class Options {
    public:

    Options(string const& name) : options(name) {}
    public:

    operator options_description&() { return options; }
    protected:

    options_description options;

    public:

    options_description& getOptions() { return options; }
};
class InputOptions : public Options {
    public:

    InputOptions();
    protected:

    optional<string> maybe_input_filepath, maybe_input_format, maybe_input_location;

    void setInputFilepath(string const& input_filepath);
    void setInputFormat(string const& input_format);
    void setInputLocation(string const& input_location);

    public:

    optional<string> const& getInputMaybeFilepath() const;
    optional<string> const& getInputMaybeFormat() const;
    optional<string> const& getInputMaybeLocation() const;
    public:

    InputFormat const& resolveInputFormat() const;

    Operator readOperatorUsingInputFormat(InputFormat const& input_format) const;
};
class OutputOptions : public Options {
    public:

    OutputOptions();
    protected:

    optional<string> maybe_output_filepath, maybe_output_format, maybe_output_location;
    bool output_states, output_overwrite;

    void setOutputFilepath(string const& output_filepath);
    void setOutputFormat(string const& output_format);
    void setOutputLocation(string const& output_location);

    public:

    optional<string> const& getOutputMaybeFilepath() const;
    optional<string> const& getOutputMaybeFormat() const;
    optional<string> const& getOutputMaybeLocation() const;
    bool getOutputStates() const;
    bool getOutputOverwrite() const;
    public:

    OutputFormat const& resolveOutputFormat() const;

    auto_ptr<Destructable const> connectToChainUsingOutputFormat(OutputFormat const& output_format, Chain& chain);
};
class ToleranceOptions : public Options {
    public:

    ToleranceOptions();
    protected:

    ChainOptions chain_options;

    public:

    double getToleranceSite() const;
    double getToleranceSweep() const;
    double getToleranceChain() const;
    double getToleranceSanity() const;
    unsigned int getToleranceIterations() const;

    ChainOptions getToleranceChainOptions() const;
};

}

#endif
