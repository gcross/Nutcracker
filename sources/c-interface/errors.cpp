//@+leo-ver=5-thin
//@+node:gcross.20110902235008.2729: * @file errors.cpp
//@@language cplusplus

//@+<< License >>
//@+node:gcross.20110902235008.2730: ** << License >>
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
    return Nutcracker::thread_local_error_string->c_str();
}
//@+node:gcross.20110903000806.2757: *3* clearError
void Nutcracker_clearError() {
    Nutcracker::thread_local_error_string.reset();
}
//@-others

}
//@-leo
