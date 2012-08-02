//@+leo-ver=5-thin
//@+node:gcross.20110511190907.3535: * @file io.hpp
//@@language cplusplus
#ifndef NUTCRACKER_IO_HPP
#define NUTCRACKER_IO_HPP

//@+<< Includes >>
//@+node:gcross.20110511190907.3538: ** << Includes >>
#include <boost/filesystem.hpp>
#include <boost/format.hpp>
#include <boost/function.hpp>
#include <boost/move/move.hpp>
#include <boost/optional.hpp>
#include <boost/range/adaptor/map.hpp>
#include <boost/range/algorithm/copy.hpp>
#include <boost/range/iterator.hpp>
#include <boost/smart_ptr/shared_ptr.hpp>
#include <boost/tokenizer.hpp>
#include <iterator>
#include <map>
#include <memory>
#include <set>
#include <utility>

#include "nutcracker/tensors.hpp"
#include "nutcracker/utilities.hpp"
//@-<< Includes >>

namespace Nutcracker {

//@+<< Usings >>
//@+node:gcross.20110511190907.3539: ** << Usings >>
using boost::adaptors::map_keys;
using boost::copy;
using boost::filesystem::path;
using boost::function;
using boost::optional;
using boost::range_iterator;
using boost::shared_ptr;

using std::auto_ptr;
using std::back_inserter;
using std::map;
using std::make_pair;
using std::set;
//@-<< Usings >>

//@+others
//@+node:gcross.20110511190907.3565: ** Exceptions
//@+node:gcross.20110511190907.3803: *3* FormatTypeException
struct FormatTypeException : public std::runtime_error {
    string const format_type_name;
    FormatTypeException(string const& message, string const& format_type_name)
      : std::runtime_error(message)
      , format_type_name(format_type_name)
    {}
    virtual ~FormatTypeException() throw() {}
    vector<string> getAcceptedFormatNames() const;
};
//@+node:gcross.20110511190907.3806: *3* FormatException
struct FormatException : public FormatTypeException {
    string const format_name;
    FormatException(string const& message, string const& format_type_name, string const& format_name)
      : FormatTypeException(message,format_type_name)
      , format_name(format_name)
    {}
    virtual ~FormatException() throw() {}
};
//@+node:gcross.20110511190907.3805: *3* FormatDoesNotSupportLocationsError
struct FormatDoesNotSupportLocationsError : public FormatException {
    FormatDoesNotSupportLocationsError(string const& format_type_name, string const& format_name)
      : FormatException(
          (boost::format("The %1% %2% format does not support locations.") % format_name % format_type_name).str()
         ,format_type_name
         ,format_name
        )
    {}
};
//@+node:gcross.20110511190907.3809: *3* FormatDoesNotSupportPipeError
struct FormatDoesNotSupportPipeError : public FormatException {
    FormatDoesNotSupportPipeError(string const& format_type_name, string const& format_name)
      : FormatException(
          (boost::format("The %1% format does not support piped %2%.") % format_name % format_type_name).str()
         ,format_type_name
         ,format_name
        )
    {}
};
//@+node:gcross.20110511190907.3815: *3* OutputFormatDoesNotSupportStatesError
struct OutputFormatDoesNotSupportStatesError : public FormatException {
    OutputFormatDoesNotSupportStatesError(string const& format_name)
      : FormatException(
          (boost::format("The %1% output format does not support outputting states.") % format_name).str()
         ,"output"
         ,format_name
        )
    {}
};
//@+node:gcross.20110511190907.3802: *3* NoFormatTypeSpecifiedError
struct NoFormatTypeSpecifiedError : public FormatTypeException {
    NoFormatTypeSpecifiedError(string const& format_type_name)
      : FormatTypeException(
          (boost::format("No %1% format type has been specified.") % format_type_name).str()
         ,format_type_name
        )
    {}
};
//@+node:gcross.20110511190907.3625: *3* NoSuchFormatError
struct NoSuchFormatError : public FormatException {
    NoSuchFormatError(string const& format_type_name, string const& format_name)
      : FormatException(
          (boost::format("There is no %1% format named %2%.") % format_type_name % format_name).str()
         ,format_type_name
         ,format_name
        )
    {}
};
//@+node:gcross.20110511190907.3619: *3* NoSuchLocationError
struct NoSuchLocationError : public std::runtime_error {
    string const location;
    NoSuchLocationError(string const& location)
      : std::runtime_error((format("No such location %1%.") % location).str())
      , location(location)
    {}
    virtual ~NoSuchLocationError() throw() {}
};
//@+node:gcross.20110511190907.3566: *3* NoSuchOperatorSiteNumberError
struct NoSuchOperatorSiteNumberError : public std::runtime_error {
    unsigned int index;
    NoSuchOperatorSiteNumberError(unsigned int index)
      : std::runtime_error((format("When reading in an operator, a reference to the non-existent operator site number %1% appeared in the sequence.") % index).str())
      , index(index)
    {}
};
//@+node:gcross.20110511190907.3676: *3* OutputFileAlreadyExistsError
struct OutputFileAlreadyExists : public std::runtime_error {
    string const filename;
    OutputFileAlreadyExists(string const& filename)
      : std::runtime_error((format("The output filename %1% already exists.") % filename).str())
      , filename(filename)
    {}
    virtual ~OutputFileAlreadyExists() throw() {}
};
//@+node:gcross.20110511190907.3678: *3* OutputLocationAlreadyExistsError
struct OutputLocationAlreadyExists : public std::runtime_error {
    string const filename;
    string const location;
    OutputLocationAlreadyExists(string const& filename, string const& location)
      : std::runtime_error((format("The output location %1% in file %2% already exists.") % location % filename).str())
      , filename(filename)
      , location(location)
    {}
    virtual ~OutputLocationAlreadyExists() throw() {}
};
//@+node:gcross.20110726215559.2334: *3* BoundaryDimensionNotOneError
struct BoundaryDimensionNotOneError: public std::runtime_error {
    string boundary_name;
    unsigned int boundary_dimension;
    BoundaryDimensionNotOneError(string const& boundary_name, unsigned int boundary_dimension)
      : std::runtime_error((format("The %1% dimension of the %1%-most site must be one but instead is %2%.") % boundary_name % boundary_dimension).str())
      , boundary_name(boundary_name)
      , boundary_dimension(boundary_dimension)
    {}
    virtual ~BoundaryDimensionNotOneError() throw () {}
};
//@+node:gcross.20110726215559.2336: *3* MismatchedSiteDimensionsError
struct MismatchedSiteDimensionsError: public std::runtime_error {
    unsigned int site_number, left_dimension, right_dimension;
    MismatchedSiteDimensionsError(unsigned int const site_number, unsigned int const left_dimension, unsigned int const right_dimension)
      : std::runtime_error((format("The left dimension (%2%) of site %1% does not match the right dimension (%4%) of site %3%.  (%2% != %4%)") % (site_number-1) % left_dimension % site_number % right_dimension).str())
      , site_number(site_number)
      , left_dimension(left_dimension)
      , right_dimension(right_dimension)
    {}
    virtual ~MismatchedSiteDimensionsError() throw () {}
};
//@+node:gcross.20110726215559.2347: *3* NoSitesError
struct NoSitesError: public std::runtime_error {
    NoSitesError()
      : std::runtime_error("No operator sites were specified.")
    {}
};
//@+node:gcross.20110511190907.3591: ** Classes
//@+node:gcross.20110511190907.3607: *3* Format
template<typename FormatType, char const*& format_type_name> struct Format {
    //@+others
    //@+node:gcross.20110511190907.3608: *4* Associated types
    typedef map<string const,FormatType const*> ExtensionRegistry;
    typedef map<string const,FormatType const*> NameRegistry;
    //@+node:gcross.20110511190907.3609: *4* Constructors
    protected:

    Format(
        string const& name
      , string const& description
      , bool const supports_pipe
      , bool const supports_location
      , set<string> const& recognized_extensions
    )
      : name(name)
      , description(description)
      , supports_pipe(supports_pipe)
      , supports_location(supports_location)
      , recognized_extensions(recognized_extensions)
    {
        registerFormat(static_cast<FormatType const*>(this));
    }
    //@+node:gcross.20110511190907.3612: *4* Destructor
    public:

    ~Format() {
        unregisterFormat(name);
    }
    //@+node:gcross.20110511190907.3613: *4* Exceptions
    struct FormatAlreadyRegisteredException : public std::logic_error {
        FormatType const& format;
        FormatAlreadyRegisteredException(FormatType const& format)
          : std::logic_error((boost::format("An %1% format named %2% has already been registered.") % format_type_name % format.name).str())
          , format(format)
        {}
    };
    //@+node:gcross.20110511190907.3610: *4* Fields
    public:

    string name, description;

    bool supports_pipe;
    bool supports_location;

    set<string> recognized_extensions;
    //@+node:gcross.20110511190907.3611: *4* Registry
    protected:

    static ExtensionRegistry& getExtensionRegistry() {
        static ExtensionRegistry registry;
        return registry;
    }

    static NameRegistry& getNameRegistry() {
        static NameRegistry registry;
        return registry;
    }

    static void registerFormat(FormatType const* format) {
        {
            NameRegistry& registry = getNameRegistry();
            typename NameRegistry::const_iterator existing_iter = registry.find(format->name);
            if(existing_iter != registry.end())
                throw FormatAlreadyRegisteredException(*format);
            else
                registry.insert(make_pair(format->name,format));
        }
        {
            ExtensionRegistry& registry = getExtensionRegistry();
            BOOST_FOREACH(string const& extension, format->recognized_extensions) {
                registry.insert(make_pair(extension,format));
            }
        }
    }

    static void unregisterFormat(string const& name) {
        getNameRegistry().erase(name);
    }

    public:

    static vector<string> listNames() {
        vector<string> names;
        copy(getNameRegistry() | map_keys,back_inserter(names));
        return boost::move(names);
    }

    static FormatType const& lookupName(string const& name) {
        NameRegistry const& registry = getNameRegistry();
        typename NameRegistry::const_iterator existing_iter = registry.find(name);
        if(existing_iter == registry.end())
            throw NoSuchFormatError(format_type_name,name);
        else
            return *existing_iter->second;
    }

    static FormatType const* lookupExtension(string const& name) {
        ExtensionRegistry const& registry = getExtensionRegistry();
        typename ExtensionRegistry::const_iterator existing_iter = registry.find(name);
        if(existing_iter == registry.end())
            return NULL;
        else
            return existing_iter->second;
    }
    //@+node:gcross.20110511190907.3623: *4* Miscellaneous
    public:

    static char const* type_name;
    static FormatType const* default_format;
    //@-others
};
template<typename FormatType, char const*& format_type_name> char const* Format<FormatType,format_type_name>::type_name = format_type_name;
template<typename FormatType, char const*& format_type_name> FormatType const* Format<FormatType,format_type_name>::default_format = NULL;
//@+node:gcross.20110525201928.2447: *3* FormatInstaller
class FormatInstaller {
    FormatInstaller();
    static FormatInstaller const _;
};
//@+node:gcross.20110511190907.3592: *3* InputFormat
extern const char* input_format_type_name;
struct InputFormat;
typedef Format<InputFormat,input_format_type_name> InputFormatBase;
struct InputFormat : public InputFormatBase {
    //@+others
    //@+node:gcross.20110511190907.3595: *4* Associated types
    typedef function<
        Operator (
            optional<string> const& maybe_filename
          , optional<string> const& maybe_location
        )
    > OperatorReader;
    //@+node:gcross.20110511190907.3594: *4* Constructors
    InputFormat(
        string const& name
      , string const& description
      , bool const supports_stdin
      , bool const supports_location
      , set<string> const supported_extensions
      , OperatorReader readOperator
    )
      : InputFormatBase(name,description,supports_stdin,supports_location,supported_extensions)
      , readOperator(readOperator)
    {}
    //@+node:gcross.20110511190907.3593: *4* Fields
    protected:

    OperatorReader readOperator;
    //@+node:gcross.20110511190907.3620: *4* Operators
    public:

    Operator operator()(optional<string> const& maybe_filename, optional<string> const& maybe_location) const {
        return readOperator(maybe_filename,maybe_location);
    }
    //@-others
};
//@+node:gcross.20110511190907.3631: *3* OutputFormat
extern const char* output_format_type_name;
struct OutputFormat;
class Chain;
typedef Format<OutputFormat,output_format_type_name> OutputFormatBase;
struct OutputFormat : public OutputFormatBase {
    //@+others
    //@+node:gcross.20110511190907.3632: *4* Associated types
    typedef function<
        auto_ptr<Destructable const> (
            optional<string> const& maybe_filename
          , optional<string> const& maybe_location
          , bool output_states
          , bool overwrite
          , Chain& chain
        )
    > ChainConnector;
    //@+node:gcross.20110511190907.3633: *4* Constructors
    OutputFormat(
        string const& name
      , string const& description
      , bool const supports_stdin
      , bool const supports_location
      , set<string> const supported_extensions
      , bool const supports_states
      , ChainConnector connectToChain
    )
      : OutputFormatBase(name,description,supports_stdin,supports_location,supported_extensions)
      , supports_states(supports_states)
      , connectToChain(connectToChain)
    {}
    //@+node:gcross.20110511190907.3634: *4* Fields
    public:

    bool supports_states;

    protected:

    ChainConnector connectToChain;
    //@+node:gcross.20110511190907.3635: *4* Operators
    public:

    auto_ptr<Destructable const> operator()(
        optional<string> const& maybe_filename
      , optional<string> const& maybe_location
      , bool output_states
      , bool overwrite
      , Chain& chain
    ) const {
        return connectToChain(maybe_filename,maybe_location,output_states,overwrite,chain);
    }
    //@-others
};
//@+node:gcross.20110511190907.3618: *3* SlashTokenizer
struct LocationSlashTokenizer : public boost::tokenizer<boost::char_separator<char> > {

    LocationSlashTokenizer(string const& location) : boost::tokenizer<boost::char_separator<char> >(location,boost::char_separator<char>("/")) {}

};
//@+node:gcross.20110511190907.3540: ** Functions
//@+node:gcross.20110511190907.3558: *3* constructOperatorFrom
template<typename SequenceType> Operator constructOperatorFrom(
    vector<shared_ptr<OperatorSite const> > unique_operator_sites
  , SequenceType const& sequence
) {
    if(sequence.size() == 0) throw NoSitesError();
    Operator operator_sites;
    operator_sites.reserve(sequence.size());

    BOOST_FOREACH(unsigned int const index, sequence) {
        if(index >= unique_operator_sites.size())
            throw NoSuchOperatorSiteNumberError(index);
        shared_ptr<OperatorSite const> operator_site_ptr = unique_operator_sites[index];
        if(operator_sites.empty()) {
            if(operator_site_ptr->leftDimension() != 1) throw BoundaryDimensionNotOneError("left",operator_site_ptr->leftDimension());
        } else {
            if(operator_site_ptr->leftDimension() != operator_sites.back()->rightDimension()) throw MismatchedSiteDimensionsError(operator_sites.size(),operator_site_ptr->leftDimension(),operator_sites.back()->rightDimension());
        }
        operator_sites.emplace_back(operator_site_ptr);
    }
    if(operator_sites.back()->rightDimension() != 1) throw BoundaryDimensionNotOneError("right",operator_sites.back()->rightDimension());

    return boost::move(operator_sites);
}
//@+node:gcross.20110511190907.3546: *3* deconstructOperatorTo
void deconstructOperatorTo(
    Operator const& operator_sites
  , vector<shared_ptr<OperatorSite const> >& unique_operator_sites
  , vector<unsigned int>& sequence
);
//@+node:gcross.20110511190907.3541: *3* readUniqueOperatorSites
template<typename SitesType> vector<shared_ptr<OperatorSite const> > readUniqueOperatorSites(SitesType const& sites) {
    vector<shared_ptr<OperatorSite const> > unique_operator_sites;
    unique_operator_sites.reserve(sites.size());

    typedef typename range_iterator<SitesType const>::type Iterator;

    BOOST_FOREACH(typename iterator_traits<Iterator>::reference in, sites) {
        shared_ptr<OperatorSite> operator_site_ptr(new OperatorSite());
        in >> *operator_site_ptr;
        unique_operator_sites.push_back(static_cast<shared_ptr<OperatorSite const> >(operator_site_ptr));
    }

    return boost::move(unique_operator_sites);
}
//@+node:gcross.20110511190907.3800: *3* resolveAndCheckFormat
template<typename FormatType> FormatType const& resolveAndCheckFormat(
    optional<string> const& maybe_format_name
  , optional<string> const& maybe_filename
  , optional<string> const& maybe_location
) {
    FormatType const* format = NULL;
    char const* const format_type_name = FormatType::type_name;

    if(maybe_format_name) {
        string const& format_name = maybe_format_name.get();
        format = &FormatType::lookupName(format_name);
    } else if(maybe_filename) {
        path const filename(maybe_filename.get());
        if(filename.has_extension()) {
            format = FormatType::lookupExtension(filename.extension().native().substr(1));
        }
    }

    if(format == NULL) {
        if(FormatType::default_format == NULL) throw NoFormatTypeSpecifiedError(format_type_name);
        format = FormatType::default_format;
    }

    if(!maybe_filename && !format->supports_pipe) throw FormatDoesNotSupportPipeError(format_type_name,format->name);

    if(maybe_location && !format->supports_location) throw FormatDoesNotSupportLocationsError(format_type_name,format->name);

    return (*format);
}
//@-others

}

#endif
//@-leo
