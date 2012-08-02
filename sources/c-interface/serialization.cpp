//@+leo-ver=5-thin
//@+node:gcross.20110908221100.3008: * @file serialization.cpp
//@@language cplusplus
//@+<< Includes >>
//@+node:gcross.20110908221100.3010: ** << Includes >>
#include "common.hpp"
//@-<< Includes >>

//@+<< Usings >>
//@+node:gcross.20110908221100.3011: ** << Usings >>
//@-<< Usings >>

extern "C" {

//@+others
//@+node:gcross.20110908221100.3012: ** Functions
//@+node:gcross.20110908221100.3014: *3* free
void Nutcracker_Serialization_free(NutcrackerSerialization* s) {
    delete s;
}
//@+node:gcross.20110908221100.3015: *3* getSize
uint32_t Nutcracker_Serialization_getSize(NutcrackerSerialization* s) {
    return (*s)->ByteSize();
}
//@+node:gcross.20110908221100.3016: *3* write
void Nutcracker_Serialization_write(NutcrackerSerialization* s, void* buffer) {
    (*s)->SerializeToArray(buffer,(*s)->ByteSize());
}
//@-others

}
//@-leo
