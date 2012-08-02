//@+leo-ver=5-thin
//@+node:gcross.20110902235008.2729: * @file errors.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110902235008.2731: ** << Includes >>
#include <boost/thread/tss.hpp>

#include "common.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110902235008.2732: ** << Usings >>
//@-<< Usings >>

//@+<< C++ Prelude >>
//@+node:gcross.20110903000806.2753: ** << C++ Prelude >>
namespace Nutcracker {
    boost::thread_specific_ptr<std::string> thread_local_error_string;
}
//@-<< C++ Prelude >>

extern "C" {

//@+others
//@+node:gcross.20110903000806.2754: ** Functions
//@+node:gcross.20110903000806.2755: *3* setError
void Nutcracker_setError(char const* message) {
    Nutcracker::thread_local_error_string.reset(new std::string(message));
}
//@+node:gcross.20110903000806.2756: *3* getError
char const* Nutcracker_getError() {
    return
        Nutcracker::thread_local_error_string.get() == NULL
            ? NULL
            : Nutcracker::thread_local_error_string->c_str()
        ;
}
//@+node:gcross.20110903000806.2757: *3* clearError
void Nutcracker_clearError() {
    Nutcracker::thread_local_error_string.reset();
}
//@-others

}
//@-leo
