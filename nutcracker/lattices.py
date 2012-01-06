# Imports {{{
from numpy import identity

from .qubit import Pauli, Qubit
from .utils import basisVector
# }}}

class OperatorLattice(object): # {{{
    @classmethod
    def buildMagneticField(cls,field_operator):
        return cls.buildAllSitesSameButOne(identity(field_operator.shape[0]),field_operator)
# }}}

class StateLattice(object): # {{{
    @classmethod
    def simpleObservation(cls,physical_dimension,observation):
        return cls.simple(basisVector(physical_dimension,observation))
    @classmethod
    def buildWState(cls):
        return cls.buildAllSitesSameButOne(Qubit.up,Qubit.down)
# }}}

# Exports {{{
__all__ = [
    "OperatorLattice",
    "StateLattice",
]
# }}}
