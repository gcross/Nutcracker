# Imports {{{
from numpy import array, complex128, identity

from .miscellaneous.enum_meta import Enum
# }}}

class Pauli(Enum): # {{{
    I = identity(2,dtype=complex128)
    X = array([[0,  1],[ 1, 0]],dtype=complex128)
    Y = array([[0,-1j],[1j, 0]],dtype=complex128)
    Z = array([[1,  0],[ 0,-1]],dtype=complex128)
# }}}

class Qubit(Enum): # {{{
    up = array([1,0],dtype=complex128)
    down = array([0,1],dtype=complex128)
# }}}

# Exports {{{
__all__ = [
    "Pauli",
    "Qubit",
]
# }}}
