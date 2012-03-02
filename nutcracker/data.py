# Imports {{{

from numpy import allclose, prod, tensordot

from utils import crand

# }}}

# Classes {{{

class NDArrayData(object): # {{{

# Class construction methods {{{

    def __init__(self,_arr): # {{{
        self._arr = _arr
    # }}}

    @classmethod
    def newFilled(cls,shape,value,dtype=None): # {{{
        if not dtype:
            dtype = typeof(value)
        _arr = nd_array(shape,dtype)
        _arr[...] = value
        return NDArrayData(value)
    # }}}

    @classmethod
    def newRandom(cls,shape): # {{{
        return cls(crand(*shape))
    # }}}

    @classmethod
    def newOuterProduct(cls,factors): # {{{
        return cls(reduce(multiply.outer,factors))
    # }}}
# }}}

# Instance methods {{{

    def __iadd__(self,other): # {{{
        self._arr += other._arr
    # }}}

    def __getitem__(self,index): # {{{
        return NDArrayData(self._arr[index])
    # }}}

    def __setitem__(self,index,value): # {{{
        self._arr[index] = value._arr
    # }}}

    def asArray(self):  #{{{
        return self._arr
    # }}}

    def allcloseTo(self,other,rtol=1e-05,atol=1e-08): # {{{
        return allclose(self._arr,other._arr,rtol=rtol,atol=atol)
    # }}}

    def contractWith(self,other,self_axes,other_axes): # {{{
        return NDArrayData(tensordot(self._arr,other._arr,(self_axes,other_axes)))
    # }}}

    def extractScalar(self): # {{{
        if self.ndim != 0:
            raise ValueError("tensor is not a scalar")
        else:
            return self._arr
    # }}}

    def join(self,groups): # {{{
        _arr = self._arr.transpose([index for group in groups for index in group])
        shape = []
        index = 0
        for group in groups:
            shape.append(prod(_arr.shape[index:index+len(group)]))
            index += len(group)
        return NDArrayData(_arr.reshape(shape))
    # }}}

    def transpose(self,args): # {{{
        return NDArrayData(self,self._arr.transpose(*args))
    # }}}

# }}}

    # Properties {{{

    ndim = property(fget = lambda self: self._arr.ndim)
    shape = property(fget = lambda self: self._arr.shape)

    # }}}

# }}}

# }}}

# Exports {{{
__all__ = [
    # Classes {{{
    "NDArrayData",
    # }}}
]
# }}}
