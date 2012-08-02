//@+leo-ver=5-thin
//@+node:gcross.20110910181738.4763: * @file version.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110910181738.4765: ** << Includes >>
#include "common.hpp"

#include "nutcracker/version.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110910181738.4766: ** << Usings >>
//@-<< Usings >>

extern "C" {

//@+others
//@+node:gcross.20110910181738.4767: ** Functions
//@+node:gcross.20110910181738.4769: *3* getComponents
uint32_t Nutcracker_Version_getComponent(uint32_t index) { return Nutcracker::version[index]; }
//@+node:gcross.20110910181738.4768: *3* getSize
uint32_t Nutcracker_Version_getSize() { return Nutcracker::version.size(); }
//@+node:gcross.20110910190412.2950: *3* getString
char const* Nutcracker_Version_getString() {
    return Nutcracker::version_string.c_str();
}
//@+node:gcross.20110910181738.4770: *3* write
void Nutcracker_Version_write(uint32_t* version) { boost::copy(Nutcracker::version,version); }
//@-others

}
//@-leo
