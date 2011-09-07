#@+leo-ver=5-thin
#@+node:gcross.20110906104131.2847: * @file Nutcracker.py
#@@language python

#@+<< License >>
#@+node:gcross.20110906104131.3036: ** << License >>
#@+at
# Copyright (c) 2011, Gregory Crosswhite
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#@@c
#@-<< License >>

#@+<< Imports >>
#@+node:gcross.20110906104131.3052: ** << Imports >>
import ctypes
from ctypes import byref, c_char_p, c_uint32, POINTER
import itertools
#@-<< Imports >>

#@+<< Initialize library bindings >>
#@+node:gcross.20110906104131.3053: ** << Initialize library bindings >>
c_complex_double = ctypes.c_double * 2
c_complex_double_p = POINTER(c_complex_double)

c_uint32_p = POINTER(c_uint32)

import ctypes.util

library = ctypes.cdll.LoadLibrary(ctypes.util.find_library("Nutcracker"))

clearError = library.Nutcracker_clearError
clearError.argtypes = []
clearError.restype = None

def getError():
    result = getError._c()
    if result:
        result = result.decode("utf-8")
    return None
getError._c = library.Nutcracker_getError
getError._c.argtypes = []
getError._c.restype = c_char_p

def setError(message):
    setError._c(message.encode("utf-8"))
setError._c = library.Nutcracker_setError
setError._c.argtypes = [c_char_p]
setError._c.restype = None

#@+others
#@-others
#@-<< Initialize library bindings >>

#@+others
#@+node:gcross.20110906130654.2872: ** Functions
#@+node:gcross.20110906130654.2875: *3* bind
def bind(name,argtypes,restype):
    function = getattr(library,name)
    function.argtypes = argtypes
    function.restype = restype
    def wrapped_function(*args):
        clearError()
        result = function(*args)
        message = getError()
        if(message):
            clearError()
            raise Exception(message)
        else:
            return result
    return staticmethod(wrapped_function)
#@+node:gcross.20110906130654.2873: *3* numpyBuffer
def numpyBuffer(arr):
    return ctypes.addressof(c_void_p.from_buffer(arr))
#@+node:gcross.20110906130654.2886: *3* toComplexDouble
def toComplexDouble(x):
    c = complex(x)
    return c_complex_double(c.real,c.imag)
#@+node:gcross.20110906130654.2885: *3* toComplexDoubleArray
def toComplexDoubleArray(data):
    return (c_complex_double * len(data))(*[toComplexDouble(x) for x in data])
#@+node:gcross.20110906104131.3054: ** Classes
#@+<< Base classes >>
#@+node:gcross.20110906130654.2925: *3* << Base classes >>
#@+others
#@+node:gcross.20110906130654.2926: *4* ArrayLike
class ArrayLike:
    #@+others
    #@+node:gcross.20110906130654.2887: *5* __iter__
    def __iter__(self):
            return (self[i] for i in range(len(self)))
    #@+node:gcross.20110906130654.2889: *5* __reversed__
    def __reversed__(self):
        for i in reversed(range(0,len(self))):
            yield self[i]
    #@-others
#@+node:gcross.20110906130654.2905: *4* Handle
class Handle:
    dont_free = False

    #@+others
    #@+node:gcross.20110906104131.3065: *5* __del__
    def __del__(self):
        if hasattr(self,"_"):
            if not self.dont_free:
                self._free(self._)
            del self._
    #@-others
#@-others
#@-<< Base classes >>
#@+<< Vector/Matrix classes >>
#@+node:gcross.20110906130654.2950: *3* << Vector/Matrix classes >>
#@+others
#@+node:gcross.20110906155043.2987: *4* Matrix
class Matrix(Handle):
    #@+<< Bindings >>
    #@+node:gcross.20110906155043.2988: *5* << Bindings >>
    class C(ctypes.Structure): pass
    C_P = POINTER(C)

    _free = bind("Nutcracker_Matrix_free",[C_P],None)
    _getElementAtCoordinate = bind("Nutcracker_Matrix_getElementAtCoordinate",[C_P,c_uint32,c_uint32,c_complex_double_p],None)
    _getSize = bind("Nutcracker_Matrix_getSize",[C_P],c_uint32)
    _new = bind("Nutcracker_Matrix_new",[c_uint32,c_complex_double_p],C_P)
    _newDiagonal = bind("Nutcracker_Matrix_newDiagonal",[c_uint32,c_complex_double_p],C_P)
    #@-<< Bindings >>
    #@+others
    #@+node:gcross.20110906155043.2991: *5* __getitem__
    def __getitem__(self,index):
        i,j = index
        data = c_complex_double()
        self._getElementAtCoordinate(self._,i,j,byref(data))
        return complex(*data)
    #@+node:gcross.20110906155043.2992: *5* __init__
    def __init__(self,data):
        if isinstance(data,self.C_P):
            self._ = data
        elif hasattr(data[0],"__len__"):
            dimension = len(data)
            for row in data:
                assert len(row) == dimension
            self._ = self._new(dimension,toComplexDoubleArray(sum(list(list(row) for row in data),[])))
        else:
            data = toComplexDoubleArray(data)
            self._ = self._newDiagonal(len(data),data)
    #@+node:gcross.20110906155043.2993: *5* __len__
    def __len__(self):
        return int(self._getSize(self._))
    #@+node:gcross.20110906155043.4812: *5* __iter__
    def __iter__(self):
        return (self[index] for index in itertools.product(*(range(len(self)),)*2))
    #@-others

#@+<< Constants >>
#@+node:gcross.20110906155043.2989: *5* << Constants >>
Matrix.pauli_I = Matrix(Matrix.C_P.in_dll(library,"Nutcracker_Matrix_Pauli_I"))
Matrix.pauli_I.dont_free = True

Matrix.pauli_X = Matrix(Matrix.C_P.in_dll(library,"Nutcracker_Matrix_Pauli_X"))
Matrix.pauli_X.dont_free = True

Matrix.pauli_Y = Matrix(Matrix.C_P.in_dll(library,"Nutcracker_Matrix_Pauli_Y"))
Matrix.pauli_Y.dont_free = True

Matrix.pauli_Z = Matrix(Matrix.C_P.in_dll(library,"Nutcracker_Matrix_Pauli_Z"))
Matrix.pauli_Z.dont_free = True
#@-<< Constants >>
#@+node:gcross.20110906104131.3055: *4* Vector
class Vector(Handle,ArrayLike):
    #@+<< Bindings >>
    #@+node:gcross.20110906104131.3063: *5* << Bindings >>
    class C(ctypes.Structure): pass
    C_P = POINTER(C)

    _add = bind("Nutcracker_Vector_add",[C_P,C_P],C_P)
    _free = bind("Nutcracker_Vector_free",[C_P],None)
    _getElementAtIndex = bind("Nutcracker_Vector_getElementAtIndex",[C_P,c_uint32,c_complex_double_p],None)
    _getSize = bind("Nutcracker_Vector_getSize",[C_P],c_uint32)
    _multiply = bind("Nutcracker_Vector_multiply",[c_complex_double_p,C_P],C_P)
    _new = bind("Nutcracker_Vector_new",[c_uint32,c_complex_double_p],C_P)
    _newBasis = bind("Nutcracker_Vector_newBasis",[c_uint32,c_uint32],C_P)
    #@-<< Bindings >>
    #@+others
    #@+node:gcross.20110906104131.3068: *5* __add__
    def __add__(self,other):
        assert(isinstance(other,Vector))
        return Vector(self._add(self._,other._))
    #@+node:gcross.20110906104131.3073: *5* __getitem__
    def __getitem__(self,index):
        data = c_complex_double()
        self._getElementAtIndex(self._,index,byref(data))
        return complex(*data)
    #@+node:gcross.20110906104131.3064: *5* __init__
    def __init__(self,*args):
        assert(len(args) == 1 or len(args) == 2)
        if(len(args)) == 2:
            self._ = self._newBasis(args[0],args[1])
        elif type(args[0]) == self.C_P:
            self._ = args[0]
        else:
            data = toComplexDoubleArray(args[0])
            self._ = self._new(len(data),data)
    #@+node:gcross.20110906104131.3072: *5* __len__
    def __len__(self):
        return int(self._getSize(self._))
    #@+node:gcross.20110906104131.3066: *5* __mul__
    def __mul__(self,c):
        return Vector(self._multiply(toComplexDouble(c),self._))
    #@+node:gcross.20110906155043.2972: *5* __rmul__
    def __rmul__(self,c):
        return Vector(self._multiply(toComplexDouble(c),self._))
    #@-others

#@+<< Constants >>
#@+node:gcross.20110906130654.2898: *5* << Constants >>
Vector.qubit_up = Vector(Vector.C_P.in_dll(library,"Nutcracker_Vector_Qubit_Up"))
Vector.qubit_up.dont_free = True

Vector.qubit_down = Vector(Vector.C_P.in_dll(library,"Nutcracker_Vector_Qubit_Down"))
Vector.qubit_down.dont_free = True
#@-<< Constants >>
#@-others
#@-<< Vector/Matrix classes >>
#@+<< State/Operator classes >>
#@+node:gcross.20110906130654.2951: *3* << State/Operator classes >>
#@+others
#@+node:gcross.20110906155043.4817: *4* Operator
class Operator(Handle):
    #@+<< Bindings >>
    #@+node:gcross.20110906155043.4818: *5* << Bindings >>
    class C(ctypes.Structure): pass
    C_P = POINTER(C)

    _free = bind("Nutcracker_Operator_free",[C_P],None)
    #@-<< Bindings >>
    #@+others
    #@+node:gcross.20110906155043.4819: *5* __init__
    def __init__(self,handle):
        self._ = handle
    #@-others
#@+node:gcross.20110906130654.2901: *4* State
class State(Handle):
    #@+<< Bindings >>
    #@+node:gcross.20110906130654.2902: *5* << Bindings >>
    class C(ctypes.Structure): pass
    C_P = POINTER(C)

    _free = bind("Nutcracker_State_free",[C_P],None)
    _computeOverlap = bind("Nutcracker_State_computeOverlap",[C_P,C_P,c_complex_double_p],None)
    _computeExpectation = bind("Nutcracker_State_computeExpectation",[C_P,Operator.C_P,c_complex_double_p],None)
    #@-<< Bindings >>
    #@+others
    #@+node:gcross.20110906130654.2908: *5* __init__
    def __init__(self,handle):
        self._ = handle
    #@+node:gcross.20110906130654.2910: *5* __mul__
    def __mul__(self,other):
        data = c_complex_double()
        if isinstance(other,State):
            self._computeOverlap(self._,other._,byref(data))
        elif isinstance(other,Operator):
            self._computeExpectation(self._,other._,byref(data))
        else:
            raise TypeError("Can only multiple State objects by either Operator objects or other State objects.")
        return complex(*data)
    #@+node:gcross.20110906155043.4821: *5* __rmul__
    def __rmul__(self,other):
        return self * other
    #@-others
#@-others
#@-<< State/Operator classes >>
#@+<< Builder classes >>
#@+node:gcross.20110906130654.2952: *3* << Builder classes >>
#@+others
#@+node:gcross.20110906130654.2939: *4* OperatorBuilder
class OperatorBuilder(Handle,ArrayLike):
    #@+<< Bindings >>
    #@+node:gcross.20110906130654.2940: *5* << Bindings >>
    class C(ctypes.Structure): pass
    C_P = POINTER(C)

    _addProductTerm = bind("Nutcracker_OperatorBuilder_addProductTerm",[C_P,POINTER(Matrix.C_P)],None)
    _compile = bind("Nutcracker_OperatorBuilder_compile",[C_P],Operator.C_P)
    _dimensionOfSite = bind("Nutcracker_OperatorBuilder_dimensionOfSite",[C_P,c_uint32],c_uint32)
    _free = bind("Nutcracker_OperatorBuilder_free",[C_P],None)
    _new = bind("Nutcracker_OperatorBuilder_new",[c_uint32,c_uint32_p],C_P)
    _newSimple = bind("Nutcracker_OperatorBuilder_newSimple",[c_uint32,c_uint32],C_P)
    _numberOfSites = bind("Nutcracker_OperatorBuilder_numberOfSites",[C_P],c_uint32)
    #@-<< Bindings >>
    #@+others
    #@+node:gcross.20110906130654.2941: *5* __getitem__
    def __getitem__(self,index):
        return int(self._dimensionOfSite(self._,index))
    #@+node:gcross.20110906130654.2942: *5* __init__
    def __init__(self,*args):
        assert(len(args) == 1 or len(args) == 2)
        if(len(args)) == 2:
            self._ = self._newSimple(args[0],args[1])
        elif type(args[0]) == self.C_P:
            self._ = args[0]
        else:
            data = (c_uint32 * len(args[0]))(*args[0])
            self._ = self._new(len(data),data)
    #@+node:gcross.20110906130654.2943: *5* __len__
    def __len__(self):
        return int(self._numberOfSites(self._))
    #@+node:gcross.20110906155043.4824: *5* addProductTerm
    def addProductTerm(self,vectors):
        data = (Matrix.C_P * len(vectors))(*[x._ for x in vectors])
        assert len(data) == len(self)
        self._addProductTerm(self._,data)
        return self
    #@+node:gcross.20110906155043.4826: *5* compile
    def compile(self):
        return Operator(self._compile(self._))
    #@-others
#@+node:gcross.20110906130654.2915: *4* StateBuilder
class StateBuilder(Handle,ArrayLike):
    #@+<< Bindings >>
    #@+node:gcross.20110906130654.2916: *5* << Bindings >>
    class C(ctypes.Structure): pass
    C_P = POINTER(C)

    _addProductTerm = bind("Nutcracker_StateBuilder_addProductTerm",[C_P,POINTER(Vector.C_P)],None)
    _compile = bind("Nutcracker_StateBuilder_compile",[C_P],State.C_P)
    _dimensionOfSite = bind("Nutcracker_StateBuilder_dimensionOfSite",[C_P,c_uint32],c_uint32)
    _free = bind("Nutcracker_StateBuilder_free",[C_P],None)
    _new = bind("Nutcracker_StateBuilder_new",[c_uint32,c_uint32_p],C_P)
    _newSimple = bind("Nutcracker_StateBuilder_newSimple",[c_uint32,c_uint32],C_P)
    _numberOfSites = bind("Nutcracker_StateBuilder_numberOfSites",[C_P],c_uint32)
    #@-<< Bindings >>
    #@+others
    #@+node:gcross.20110906130654.2929: *5* __getitem__
    def __getitem__(self,index):
        return int(self._dimensionOfSite(self._,index))
    #@+node:gcross.20110906130654.2917: *5* __init__
    def __init__(self,*args):
        assert(len(args) == 1 or len(args) == 2)
        if(len(args)) == 2:
            self._ = self._newSimple(args[0],args[1])
        elif type(args[0]) == self.C_P:
            self._ = args[0]
        else:
            data = (c_uint32 * len(args[0]))(*args[0])
            self._ = self._new(len(data),data)
    #@+node:gcross.20110906130654.2919: *5* __len__
    def __len__(self):
        return int(self._numberOfSites(self._))
    #@+node:gcross.20110906155043.2973: *5* addProductTerm
    def addProductTerm(self,vectors):
        data = (Vector.C_P * len(vectors))(*[x._ for x in vectors])
        assert len(data) == len(self)
        self._addProductTerm(self._,data)
        return self
    #@+node:gcross.20110906155043.2974: *5* compile
    def compile(self):
        return State(self._compile(self._))
    #@-others
#@-others
#@-<< Builder classes >>
#@-others

#@+<< Export list >>
#@+node:gcross.20110906104131.3062: ** << Export list >>
__all__ = [
    "library",

    "clearError",
    "getError",
    "setError",

    "Matrix",
    "OperatorBuilder",
    "StateBuilder",
    "Vector",
]
#@-<< Export list >>
#@-leo