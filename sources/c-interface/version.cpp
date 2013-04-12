#include "common.hpp"

#include "nutcracker/version.hpp"


extern "C" {

uint32_t Nutcracker_Version_getComponent(uint32_t index) { return Nutcracker::version[index]; }
uint32_t Nutcracker_Version_getSize() { return Nutcracker::version.size(); }
char const* Nutcracker_Version_getString() {
    return Nutcracker::version_string.c_str();
}
void Nutcracker_Version_write(uint32_t* version) { boost::copy(Nutcracker::version,version); }

}
