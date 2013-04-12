#include <boost/thread/tss.hpp>

#include "common.hpp"


namespace Nutcracker {
    boost::thread_specific_ptr<std::string> thread_local_error_string;
}

extern "C" {

void Nutcracker_setError(char const* message) {
    Nutcracker::thread_local_error_string.reset(new std::string(message));
}
char const* Nutcracker_getError() {
    return
        Nutcracker::thread_local_error_string.get() == NULL
            ? NULL
            : Nutcracker::thread_local_error_string->c_str()
        ;
}
void Nutcracker_clearError() {
    Nutcracker::thread_local_error_string.reset();
}

}
