#@+leo-ver=5-thin
#@+node:gcross.20111109104457.1634: * @file qubit.py
#@+<< License >>
#@+node:gcross.20111109104457.1635: ** << License >>
#@-<< License >>

#@+<< Imports >>
#@+node:gcross.20111109104457.1636: ** << Imports >>
from numpy import array, complex128, identity

from .miscellaneous.enum_meta import Enum
#@-<< Imports >>

#@+others
#@+node:gcross.20111109104457.1638: ** Paulis
class Pauli(Enum):
    I = identity(2,dtype=complex128)
    X = array([[0,  1],[ 1, 0]],dtype=complex128)
    Y = array([[0,-1j],[1j, 0]],dtype=complex128)
    Z = array([[1,  0],[ 0,-1]],dtype=complex128)
#@+node:gcross.20111110151700.1740: ** Qubits
class Qubit(Enum):
    up = array([1,0],dtype=complex128)
    down = array([0,1],dtype=complex128)
#@-others

#@+<< Exports >>
#@+node:gcross.20111109104457.1637: ** << Exports >>
__all__ = [
    "Pauli",
    "Qubit",
]
#@-<< Exports >>
#@-leo
