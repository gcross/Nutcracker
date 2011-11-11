#@+leo-ver=5-thin
#@+node:gcross.20111110233742.1801: * @file lattices.py
#@+<< License >>
#@+node:gcross.20111110233742.1802: ** << License >>
#@-<< License >>

#@+<< Imports >>
#@+node:gcross.20111110233742.1803: ** << Imports >>
from numpy import identity

from .qubit import Pauli, Qubit
from .utils import basisVector
#@-<< Imports >>

#@+others
#@+node:gcross.20111110233742.1805: ** Classes
#@+node:gcross.20111110233742.1806: *3* OperatorLattice
class OperatorLattice(object):
    #@+others
    #@+node:gcross.20111110151700.1741: *4* buildMagneticField
    @classmethod
    def buildMagneticField(cls,field_operator):
        return cls.buildAllSitesSameButOne(identity(field_operator.shape[0]),field_operator)
    #@-others
#@+node:gcross.20111110233742.1807: *3* StateLattice
class StateLattice(object):
    #@+others
    #@+node:gcross.20111109104457.1884: *4* simpleObservation
    @classmethod
    def simpleObservation(cls,physical_dimension,observation):
        return cls.simple(basisVector(physical_dimension,observation))
    #@+node:gcross.20111110233742.1808: *4* buildWState
    @classmethod
    def buildWState(cls):
        return cls.buildAllSitesSameButOne(Qubit.up,Qubit.down)
    #@-others
#@-others

#@+<< Exports >>
#@+node:gcross.20111110233742.1804: ** << Exports >>
__all__ = [
    "OperatorLattice",
    "StateLattice",
]
#@-<< Exports >>
#@-leo
