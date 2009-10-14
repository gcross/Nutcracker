#@+leo-ver=4-thin
#@+node:gcross.20091013134215.1452:@thin models.py
#@@language Python

#@<< Import needed modules >>
#@+node:gcross.20091013134215.1455:<< Import needed modules >>
from foolscap.api import Copyable, RemoteCopy
from numpy import zeros, identity, complex128
from paulis import *
#@-node:gcross.20091013134215.1455:<< Import needed modules >>
#@nl

#@+others
#@+node:gcross.20091013134215.1453:Classes
#@+node:gcross.20091013134215.1454:GadgetHamiltonianFactory
class GadgetHamiltonianFactory(Copyable,RemoteCopy):
    #@    @+others
    #@+node:gcross.20091013134215.1460:(copy type)
    typeToCopy = copytype = "GadgetHamiltonianFactory"
    #@-node:gcross.20091013134215.1460:(copy type)
    #@+node:gcross.20091013134215.1456:__init__
    def __init__(self,number_of_sites=None,perturbation_strength=None):
        self.number_of_sites = number_of_sites
        self.perturbation_strength = perturbation_strength
    #@-node:gcross.20091013134215.1456:__init__
    #@+node:gcross.20091013134215.1457:__call__
    def __call__(self):
        left_operator_site_tensor = zeros((2,2,1,6),complex128)
        left_operator_site_tensor[...,0,0] = -self.perturbation_strength*X/2
        left_operator_site_tensor[...,0,2] = I
        left_operator_site_tensor[...,0,4] = I
        left_operator_site_tensor[...,0,5] = 0

        middle_operator_site_tensor = zeros((2,2,6,6),complex128)
        middle_operator_site_tensor[...,0,0] = I
        middle_operator_site_tensor[...,0,1] = X
        middle_operator_site_tensor[...,1,1] = I
        middle_operator_site_tensor[...,2,2] = I
        middle_operator_site_tensor[...,2,3] = X
        middle_operator_site_tensor[...,3,3] = I
        middle_operator_site_tensor[...,4,4] = I
        middle_operator_site_tensor[...,4,5] = Z
        middle_operator_site_tensor[...,5,5] = I

        right_operator_site_tensor = zeros((2,2,6,1),complex128)
        right_operator_site_tensor[...,1,0] = I
        right_operator_site_tensor[...,3,0] = -self.perturbation_strength*X/2
        right_operator_site_tensor[...,4,0] = 0
        right_operator_site_tensor[...,5,0] = I

        return [left_operator_site_tensor] + [middle_operator_site_tensor]*(self.number_of_sites-2) + [right_operator_site_tensor]
    #@-node:gcross.20091013134215.1457:__call__
    #@+node:gcross.20091013134215.1459:setCopyableState
    def setCopyableState(self, state):
        self.__dict__ = state
    #@-node:gcross.20091013134215.1459:setCopyableState
    #@-others
#@-node:gcross.20091013134215.1454:GadgetHamiltonianFactory
#@-node:gcross.20091013134215.1453:Classes
#@-others
#@-node:gcross.20091013134215.1452:@thin models.py
#@-leo
