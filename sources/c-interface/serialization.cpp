#include "common.hpp"


extern "C" {

void Nutcracker_Serialization_free(NutcrackerSerialization* s) {
    delete s;
}
uint32_t Nutcracker_Serialization_getSize(NutcrackerSerialization* s) {
    return (*s)->ByteSize();
}
void Nutcracker_Serialization_write(NutcrackerSerialization* s, void* buffer) {
    (*s)->SerializeToArray(buffer,(*s)->ByteSize());
}

}
